package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.{BaseModule, SourceInfo, UnlocatableSourceInfo}
import chisel3.experimental.dataview._
import chisel3.experimental.hierarchy._
import chisel3.experimental.hierarchy.core.Clone
import chisel3.internal.HasId
import chisel3.internal.Builder
import chisel3.internal.binding._
import chisel3.internal.firrtl.ir
import chisel3.reflect.DataMirror

import scala.collection.immutable.VectorMap
import scala.collection.mutable

private[cacheable] object CachePlan {
  sealed trait Access
  case object Read extends Access
  case object Write extends Access

  /** A temporary miss-path representation.  It intentionally refers to prototype IDs. */
  final case class CapturedRegion(
    commands:        Seq[ir.Command],
    localIds:        Seq[Data],
    localSourceInfo: Map[Data, SourceInfo],
    captures:        Seq[Capture]
  )

  /** A pre-cacheable Data reference and its only permitted access mode. */
  final case class Capture(id: Data, access: Access, path: CapturePath, sourceInfo: SourceInfo)

  /** A stable relative path from a named pre-cacheable root to a Data leaf. */
  final case class CapturePath(value: String) {
    require(value.nonEmpty, "Cacheable capture paths must not be empty")
  }

  /** Mapping used when a captured command region is copied into another module.
    *
    * Read and write mappings remain separate because the copied IR carries access mode. Capture
    * validation rejects any boundary value that is both read and written.
    */
  final case class Rebinding(
    local:  Map[HasId, HasId] = Map.empty,
    reads:  Map[HasId, HasId] = Map.empty,
    writes: Map[HasId, HasId] = Map.empty
  ) {
    private def resolve(id: HasId, access: Access): HasId = {
      val mapping = access match {
        case Read  => reads
        case Write => writes
      }
      mapping.getOrElse(id, local.getOrElse(id, id))
    }

    private[cacheable] def id(id: HasId, access: Access): HasId = resolve(id, access)
  }

  /** The complete reusable boundary contract for a cacheable region. */
  final case class RegionInterface(
    ports: Seq[PortBinding]
  )

  /** One synthetic port and the enclosing-module path it represents. */
  final case class PortBinding(
    index:     Int,
    path:      CapturePath,
    direction: Access,
    gen:       Data
  ) {
    def name: String = s"cacheable_${index}_${direction.toString.toLowerCase}"

    def ioType: Data = direction match {
      case Read  => Input(gen.cloneTypeFull)
      case Write => Output(gen.cloneTypeFull)
    }
  }

  private def withSourceInfo(message: String, sourceInfo: SourceInfo): String =
    message + sourceInfo.makeMessage(" " + _)

  private def sourceLocation(sourceInfo: SourceInfo): String =
    sourceInfo.makeMessage(" at " + _)

  private def describeData(data: Data, paths: Map[Data, CapturePath] = Map.empty): String = {
    val name = paths.get(data).map(_.value).getOrElse(data.earlyName)
    s"'$name' (${data.typeName})"
  }

  private def describeId(id: HasId, paths: Map[Data, CapturePath] = Map.empty): String = id match {
    case data: Data => describeData(data, paths)
    case other =>
      other._computeName(None).map(name => s"'$name' (${other.getClass.getName})").getOrElse(other.getClass.getName)
  }

  /** A reusable synthetic definition and its path-based enclosing-module interface. */
  private[cacheable] final case class CachedRegion(
    definition: Definition[CachePlanModule],
    interface:  RegionInterface
  )

  /** Build the internal IO record used by a synthetic cache-plan module. */
  private[cacheable] final class IORecord(specs: Seq[PortBinding]) extends Record {
    override val elements: VectorMap[String, Data] =
      VectorMap.from(specs.iterator.map(spec => spec.name -> spec.ioType))
    override def cloneType: this.type =
      new IORecord(specs).asInstanceOf[this.type]
  }

  /** Create read/write rebinding maps for an instantiated [[IORecord]]. */
  def portRebinding(
    captured:  CapturedRegion,
    interface: RegionInterface,
    io:        Record,
    local:     Map[HasId, HasId]
  ): Rebinding = {
    // FlatIO returns a Record DataView.  Its elements must be reified to the actual module ports
    // before they are inserted into command IR, otherwise the copied commands refer to the view
    // root rather than a FIRRTL port.
    require(
      captured.captures.length == interface.ports.length,
      "Cacheable region captures and interface ports must have the same length"
    )
    val reads = mutable.HashMap.empty[HasId, HasId]
    val writes = mutable.HashMap.empty[HasId, HasId]
    captured.captures.iterator.zip(interface.ports.iterator).foreach { case (capture, spec) =>
      require(
        capture.path == spec.path && capture.access == spec.direction,
        s"Cacheable capture ${capture.path.value} does not match its synthetic port"
      )
      val rawField = io._elements(spec.name)
      val field = reifySingleTarget(rawField).getOrElse(rawField)
      capture.access match {
        case Read  => reads += capture.id -> field
        case Write => writes += capture.id -> field
      }
    }
    Rebinding(
      local = local,
      reads = reads.toMap,
      writes = writes.toMap
    )
  }

  /** Build the reusable synthetic definition for a cache miss. */
  def cache(captured: CapturedRegion)(implicit sourceInfo: SourceInfo): CachedRegion = {
    val interface = RegionInterface(
      captured.captures.zipWithIndex.map { case (capture, index) =>
        PortBinding(index, capture.path, capture.access, capture.id.cloneTypeFull)
      }
    )
    CachedRegion(Definition(new CachePlanModule(captured, interface)), interface)
  }

  /** Instantiate a cached synthetic definition and connect it to this module's resolved captures.
    */
  def instantiate(
    cached:    CachedRegion,
    beforeIds: IndexedSeq[HasId]
  )(implicit sourceInfo: SourceInfo): Unit = {
    val pathIndex = capturePathIndex(beforeIds)
    val resolvedPorts = cached.interface.ports.map { spec =>
      val captured = pathIndex.dataByPath.getOrElse(spec.path, Vector.empty) match {
        case Vector(value) => value
        case Vector() =>
          throw new IllegalArgumentException(s"Cacheable capture path '${spec.path.value}' is absent from this module")
        case _ =>
          throw new IllegalArgumentException(s"Cacheable capture path '${spec.path.value}' is ambiguous in this module")
      }
      require(
        DataMirror.checkTypeEquivalence(spec.gen, captured),
        s"Cacheable capture '${spec.path.value}' has incompatible type in this module instance"
      )
      spec -> captured
    }
    val instance = Instance(cached.definition)
    val instancePorts = instance.underlying match {
      case Clone(module: ModuleClone[_]) => module.getPorts
      case other =>
        throw new IllegalStateException(s"Unexpected cache-plan instance representation: $other")
    }

    resolvedPorts.foreach { case (spec, captured) =>
      val instancePort = instancePorts._elements(spec.name)
      spec.direction match {
        case Read  => instancePort := captured
        case Write => captured := instancePort
      }
    }
  }

  private[cacheable] final class CachePlanModule(
    captured:  CapturedRegion,
    interface: RegionInterface
  )(implicit sourceInfo: SourceInfo)
      extends RawModule {
    val io = FlatIO(new IORecord(interface.ports))

    private val local: Map[HasId, HasId] = captured.localIds.flatMap(cloneLocalTree).toMap
    private val rebinding = portRebinding(captured, interface, io, local)
    rebind(captured, rebinding).foreach(Builder.pushCommand)

    override def desiredName: String = "CachePlanModule"

    private def cloneLocalTree(original: Data): Map[HasId, HasId] = {
      val clone = cloneLocal(original)
      val originalMembers = DataMirror.collectAllMembers(original)
      val cloneMembers = DataMirror.collectAllMembers(clone)
      require(
        originalMembers.length == cloneMembers.length,
        withSourceInfo(
          s"Cannot rebind local ${describeData(original)} because its cloned structure changed",
          captured.localSourceInfo.getOrElse(original, sourceInfo)
        )
      )
      originalMembers.iterator
        .zip(cloneMembers.iterator)
        .map { case (originalMember, cloneMember) =>
          (originalMember: HasId) -> (cloneMember: HasId)
        }
        .toMap
    }

    private def cloneLocal(original: Data): Data = {
      val clone = original.cloneTypeFull
      val binding = original.topBinding match {
        case _: WireBinding           => WireBinding(this, Builder.currentBlock)
        case _: RegBinding            => RegBinding(this, Builder.currentBlock)
        case _: OpBinding             => OpBinding(this, Builder.currentBlock)
        case _: MemoryPortBinding     => MemoryPortBinding(this, Builder.currentBlock)
        case _: InstanceChoiceBinding => InstanceChoiceBinding(this, Builder.currentBlock)
        case other =>
          throw new UnsupportedOperationException(
            withSourceInfo(
              s"Cannot rebind local ${describeData(original)} with binding $other",
              captured.localSourceInfo.getOrElse(original, sourceInfo)
            )
          )
      }
      clone.bind(binding)
      clone
    }
  }

  /** Resolve every pre-cacheable Data leaf to a stable relative structural path.
    *
    * A capture boundary is intentionally restricted to explicitly or automatically named roots.
    * This avoids binding an unnamed temporary by elaboration order on a cache hit.
    */
  private final case class CapturePathIndex(
    pathsByData: Map[Data, Vector[CapturePath]],
    dataByPath:  Map[CapturePath, Vector[Data]]
  ) {
    // Diagnostic text only needs one representative path per Data, so defer this extra pass
    // until an error path actually asks for it.
    lazy val describePaths: Map[Data, CapturePath] =
      pathsByData.iterator.collect { case (data, paths) if paths.nonEmpty => data -> paths.head }.toMap

    def resolveCapturedPath(data: Data): (Option[CapturePath], Boolean) = {
      val candidatePaths = pathsByData.getOrElse(data, Vector.empty)
      val path = candidatePaths
        .find(candidate => dataByPath.getOrElse(candidate, Vector.empty).size == 1)
        .orElse(candidatePaths.headOption)
      val ambiguous = path.exists(candidate => dataByPath.getOrElse(candidate, Vector.empty).size != 1)
      (path, ambiguous)
    }
  }

  private def foreachCapturePath(beforeIds: IndexedSeq[HasId])(f: (CapturePath, Data) => Unit): Unit = {
    beforeIds.iterator.foreach {
      case root: Data =>
        root._computeName(None).filter(_.nonEmpty).foreach { rootName =>
          getRecursiveFields.lazily(root, rootName).iterator.foreach { case (field, path) =>
            f(CapturePath(path), field)
          }
        }
      case child: BaseModule =>
        child._computeName(None).filter(_.nonEmpty).foreach { childName =>
          child.getChiselPorts(UnlocatableSourceInfo).foreach { case (portName, port: Data) =>
            getRecursiveFields.lazily(port, s"$childName.$portName").iterator.foreach { case (field, path) =>
              f(CapturePath(path), field)
            }
          }
        }
      case _ =>
    }
  }

  private def capturePathIndex(beforeIds: IndexedSeq[HasId]): CapturePathIndex = {
    val dataByPath = mutable.LinkedHashMap.empty[CapturePath, mutable.LinkedHashSet[Data]]
    val pathsByData = mutable.LinkedHashMap.empty[Data, mutable.LinkedHashSet[CapturePath]]
    foreachCapturePath(beforeIds) { (path, data) =>
      dataByPath.getOrElseUpdate(path, mutable.LinkedHashSet.empty) += data
      pathsByData.getOrElseUpdate(data, mutable.LinkedHashSet.empty) += path
    }
    CapturePathIndex(
      pathsByData.iterator.map { case (data, paths) => data -> paths.toVector }.toMap,
      dataByPath.iterator.map { case (path, data) => path -> data.toVector }.toMap
    )
  }

  /** Capture a region after validating its restricted, Data-only closure. */
  def capture(
    beforeIds: IndexedSeq[HasId],
    afterIds:  IndexedSeq[HasId],
    commands:  Seq[ir.Command]
  ): CapturedRegion = {
    val locals = mutable.LinkedHashSet[Data]()
    val localMembers = mutable.LinkedHashSet[Data]()
    val localSourceInfo = mutable.HashMap[Data, SourceInfo]()

    val definitionSourceInfo = mutable.HashMap[HasId, SourceInfo]()
    def indexDefinitionSource(command: ir.Command): Unit = command match {
      case definition: ir.Definition => definitionSourceInfo.getOrElseUpdate(definition.id, command.sourceInfo)
      case ir.DefContract(_, ids, _) =>
        ids.foreach { id => definitionSourceInfo.getOrElseUpdate(id, command.sourceInfo) }
      case ir.When(_, _, ifCommands, elseCommands) =>
        ifCommands.foreach(indexDefinitionSource)
        elseCommands.foreach(indexDefinitionSource)
      case ir.LayerBlock(_, _, nested) => nested.foreach(indexDefinitionSource)
      case _                           =>
    }
    commands.foreach(indexDefinitionSource)

    val pathIndex = capturePathIndex(beforeIds)
    lazy val describePaths = pathIndex.describePaths

    val errors = mutable.ArrayBuffer[String]()
    def guardError(): Unit = {
      if (errors.nonEmpty) {
        throw new IllegalArgumentException(errors.toSeq.mkString("\n"))
      }
    }

    def addLocal(id: HasId)(implicit sourceInfo: SourceInfo): Unit = id match {
      case data: Data =>
        locals += data
        localMembers ++= DataMirror.collectAllMembers(data)
        localSourceInfo.getOrElseUpdate(data, sourceInfo)
      case other =>
        errors += withSourceInfo(
          s"Cacheable regions only support Data local definitions; found ${describeId(other)}",
          sourceInfo
        )
    }

    def isLocal(id: HasId): Boolean = id match {
      case data: Data => localMembers.contains(data)
      case _ => false
    }

    val beforeIdSet = beforeIds.toSet
    afterIds.iterator.filterNot(beforeIdSet).foreach { id =>
      addLocal(id)(definitionSourceInfo.getOrElse(id, UnlocatableSourceInfo))
    }
    guardError()

    val accesses = mutable.LinkedHashMap[HasId, (Access, SourceInfo)]()

    def record(id: HasId, access: Access)(implicit sourceInfo: SourceInfo): Unit = {
      if (!isLocal(id)) {
        accesses.get(id) match {
          case Some((previous, previousInfo)) =>
            if (previous != access) {
              errors += (s"Cacheable capture ${describeId(id, describePaths)} is both ${previous.toString.toLowerCase}" +
                sourceLocation(previousInfo) + s" and ${access.toString.toLowerCase}" + sourceLocation(sourceInfo) +
                "; mixed access is unsupported")
            }
          case None => accesses += id -> (access -> sourceInfo)
        }
      }
    }

    def recordArg(arg: ir.Arg, access: Access)(implicit sourceInfo: SourceInfo): Unit = arg match {
      case ir.Node(id)        => record(id, access)
      case ir.Slot(imm, _)    => recordArg(imm, access)
      case ir.OpaqueSlot(imm) => recordArg(imm, access)
      case ir.Index(imm, value) =>
        recordArg(imm, access)
        recordArg(value, Read)
      case ir.LitIndex(imm, _)                => recordArg(imm, access)
      case ir.ProbeExpr(probe)                => recordArg(probe, access)
      case ir.RWProbeExpr(probe)              => recordArg(probe, access)
      case ir.ProbeRead(probe)                => recordArg(probe, access)
      case ir.PrimExpr(_, args @ _*)          => args.foreach(recordArg(_, Read))
      case ir.PropExpr(_, _, _, args)         => args.foreach(recordArg(_, Read))
      case ir.DomainSubfield(_, domain, _, _) => recordArg(domain, Read)
      case _                                  =>
    }

    def recordCommand(command: ir.Command): Unit = {
      implicit val sourceInfo: SourceInfo = command.sourceInfo
      command match {
        case ir.DefPrim(_, id, _, args @ _*) =>
          addLocal(id)
          args.foreach(recordArg(_, Read))
        case ir.DefInvalid(_, arg) => recordArg(arg, Write)
        case ir.DefWire(_, id)     => addLocal(id)
        case ir.DefReg(_, id, clock) =>
          addLocal(id)
          recordArg(clock, Read)
        case ir.DefRegInit(_, id, clock, reset, init) =>
          addLocal(id)
          recordArg(clock, Read)
          recordArg(reset, Read)
          recordArg(init, Read)
        case ir.DefMemPort(_, id, source, _, index, clock) =>
          addLocal(id)
          recordArg(source, Read)
          recordArg(index, Read)
          recordArg(clock, Read)
        case ir.When(_, pred, ifCommands, elseCommands) =>
          recordArg(pred, Read)
          ifCommands.foreach(recordCommand)
          elseCommands.foreach(recordCommand)
        case ir.Connect(_, loc, exp) =>
          recordArg(loc, Write)
          recordArg(exp, Read)
        case ir.PropAssign(_, loc, exp) =>
          recordArg(loc, Write)
          recordArg(exp, Read)
        case ir.PropertyAssert(_, condition, _) => recordArg(condition, Read)
        case ir.Attach(_, locs)                 => locs.foreach(recordArg(_, Write))
        case ir.LayerBlock(_, _, commands)      => commands.foreach(recordCommand)
        case ir.DefContract(_, ids, exprs) =>
          ids.foreach(addLocal(_))
          exprs.foreach(recordArg(_, Read))
        case _: ir.Flush =>
          errors += withSourceInfo("Cacheable regions do not support flush statements", command.sourceInfo)
        case ir.ProbeDefine(_, sink, probe) =>
          recordArg(sink, Write)
          recordArg(probe, Read)
        case ir.ProbeForceInitial(_, probe, value) =>
          recordArg(probe, Write)
          recordArg(value, Read)
        case ir.ProbeReleaseInitial(_, probe) =>
          recordArg(probe, Write)
        case ir.ProbeForce(_, clock, cond, probe, value) =>
          recordArg(clock, Read)
          recordArg(cond, Read)
          recordArg(probe, Write)
          recordArg(value, Read)
        case ir.ProbeRelease(_, clock, cond, probe) =>
          recordArg(clock, Read)
          recordArg(cond, Read)
          recordArg(probe, Write)
        case ir.DomainDefine(_, sink, source) =>
          recordArg(sink, Write)
          recordArg(source, Read)
        case ir.DomainInstance(_, id, _, properties) =>
          addLocal(id)
          properties.foreach(recordArg(_, Read))
        case ir.DefIntrinsicExpr(_, _, id, args, _) =>
          addLocal(id)
          args.foreach(recordArg(_, Read))
        case ir.DefIntrinsic(_, _, args, _) =>
          args.foreach(recordArg(_, Read))
        case _: ir.FirrtlComment | _: ir.Placeholder =>
        case definition: ir.Definition =>
          addLocal(definition.id)
          errors += withSourceInfo(
            s"Unsupported command in cacheable region: ${definition.getClass.getName}",
            command.sourceInfo
          )
        case other =>
          errors += withSourceInfo(
            s"Unsupported command in cacheable region: ${other.getClass.getName}",
            other.sourceInfo
          )
      }
    }

    commands.foreach(recordCommand)
    guardError()

    def resolveCapture(id: HasId, access: Access, info: SourceInfo): Option[Capture] = id match {
      case data: Data =>
        val (pathOption, ambiguous) = pathIndex.resolveCapturedPath(data)
        pathOption match {
          case None =>
            errors += withSourceInfo(
              s"Cacheable capture ${describeData(data)} is not rooted in named pre-cacheable module state",
              info
            )
            None
          case Some(path) if ambiguous =>
            errors += withSourceInfo(
              s"Cacheable capture ${describeData(data, describePaths)} has ambiguous pre-cacheable path '${path.value}'",
              info
            )
            None
          case Some(path) => Some(Capture(data, access, path, info))
        }
      case other =>
        errors += withSourceInfo(
          s"Cacheable regions only support Data captures; found ${describeId(other)}",
          info
        )
        None
    }

    val captures = accesses.iterator.flatMap { case (id, (access, info)) =>
      resolveCapture(id, access, info)
    }.toSeq
    guardError()

    CapturedRegion(
      commands,
      locals.toSeq,
      localSourceInfo.toMap,
      captures
    )
  }

  /** Rebind a captured command region to fresh IDs.
    *
    * This copies the Chisel IR command objects; it does not mutate the prototype commands.  The
    * caller is responsible for allocating fresh local IDs and synthetic IOs, then supplying all
    * mappings in `rebinding`.
    */
  def rebind(captured: CapturedRegion, rebinding: Rebinding): Seq[ir.Command] = {
    captured.localIds.foreach { value =>
      require(
        rebinding.local.contains(value),
        withSourceInfo(
          s"Missing local rebinding for ${describeData(value)}",
          captured.localSourceInfo.getOrElse(value, UnlocatableSourceInfo)
        )
      )
    }
    captured.captures.foreach { capture =>
      val mapping = capture.access match {
        case Read  => rebinding.reads
        case Write => rebinding.writes
      }
      require(
        mapping.contains(capture.id) || rebinding.local.contains(capture.id),
        withSourceInfo(
          s"Missing ${capture.access.toString.toLowerCase} rebinding for '${capture.path.value}'",
          capture.sourceInfo
        )
      )
    }

    def id(value: HasId, access: Access): HasId = rebinding.id(value, access)

    def arg(value: ir.Arg, access: Access): ir.Arg = value match {
      case ir.Node(value)                   => ir.Node(id(value, access))
      case ir.Slot(imm, name)               => ir.Slot(arg(imm, access), name)
      case ir.OpaqueSlot(imm)               => ir.OpaqueSlot(arg(imm, access).asInstanceOf[ir.Node])
      case ir.Index(imm, index)             => ir.Index(arg(imm, access), arg(index, Read))
      case ir.LitIndex(imm, index)          => ir.LitIndex(arg(imm, access), index)
      case ir.ProbeExpr(probe)              => ir.ProbeExpr(arg(probe, access))
      case ir.RWProbeExpr(probe)            => ir.RWProbeExpr(arg(probe, access))
      case ir.ProbeRead(probe)              => ir.ProbeRead(arg(probe, access))
      case ir.PrimExpr(op, args @ _*)       => ir.PrimExpr(op, args.map(arg(_, Read)): _*)
      case ir.PropExpr(info, tpe, op, args) => ir.PropExpr(info, tpe, op, args.map(arg(_, Read)))
      case ir.DomainSubfield(info, domain, fieldName, fieldType) =>
        ir.DomainSubfield(info, arg(domain, Read), fieldName, fieldType)
      case other => other
    }

    def data(value: Data, access: Access): Data = id(value, access).asInstanceOf[Data]

    def command(value: ir.Command): ir.Command = value match {
      case ir.DefPrim(info, value, op, args @ _*) =>
        ir.DefPrim(info, data(value, Write), op, args.map(arg(_, Read)): _*)
      case ir.DefInvalid(info, value) =>
        ir.DefInvalid(info, arg(value, Write))
      case ir.DefWire(info, value) =>
        ir.DefWire(info, data(value, Write))
      case ir.DefReg(info, value, clock) =>
        ir.DefReg(info, data(value, Write), arg(clock, Read))
      case ir.DefRegInit(info, value, clock, reset, init) =>
        ir.DefRegInit(info, data(value, Write), arg(clock, Read), arg(reset, Read), arg(init, Read))
      case ir.DefMemPort(info, value, source, direction, index, clock) =>
        ir.DefMemPort(
          info,
          data(value, Write),
          arg(source, Read).asInstanceOf[ir.Node],
          direction,
          arg(index, Read),
          arg(clock, Read)
        )
      case value: ir.When =>
        val result = new ir.When(value.sourceInfo, arg(value.pred, Read))
        value.ifRegion.getAllCommands().foreach { nested =>
          result.ifRegion.addCommand(command(nested))
        }
        if (value.hasElse) {
          value.elseRegion.getAllCommands().foreach { nested =>
            result.elseRegion.addCommand(command(nested))
          }
        }
        result
      case ir.Connect(info, loc, exp) =>
        ir.Connect(info, arg(loc, Write), arg(exp, Read))
      case ir.PropAssign(info, loc, exp) =>
        ir.PropAssign(info, arg(loc, Write).asInstanceOf[ir.Node], arg(exp, Read))
      case ir.PropertyAssert(info, condition, message) =>
        ir.PropertyAssert(info, arg(condition, Read), message)
      case ir.Attach(info, locs) =>
        ir.Attach(info, locs.map(value => arg(value, Write).asInstanceOf[ir.Node]))
      case value: ir.LayerBlock =>
        val result = new ir.LayerBlock(value.sourceInfo, value.layer)
        value.region.getAllCommands().foreach { nested =>
          result.region.addCommand(command(nested))
        }
        result
      case value: ir.DefContract =>
        val result = ir.DefContract(
          value.sourceInfo,
          value.ids.map(data(_, Write)),
          value.exprs.map(arg(_, Read))
        )
        value.region.getAllCommands().foreach { nested =>
          result.region.addCommand(command(nested))
        }
        result
      case ir.ProbeDefine(info, sink, probe) =>
        ir.ProbeDefine(info, arg(sink, Write), arg(probe, Read))
      case ir.ProbeForceInitial(info, probe, value) =>
        ir.ProbeForceInitial(info, arg(probe, Write), arg(value, Read))
      case ir.ProbeReleaseInitial(info, probe) =>
        ir.ProbeReleaseInitial(info, arg(probe, Write))
      case ir.ProbeForce(info, clock, cond, probe, value) =>
        ir.ProbeForce(info, arg(clock, Read), arg(cond, Read), arg(probe, Write), arg(value, Read))
      case ir.ProbeRelease(info, clock, cond, probe) =>
        ir.ProbeRelease(info, arg(clock, Read), arg(cond, Read), arg(probe, Write))
      case ir.DomainDefine(info, sink, source) =>
        ir.DomainDefine(info, arg(sink, Write), arg(source, Read))
      case ir.DomainInstance(info, value, domain, properties) =>
        ir.DomainInstance(
          info,
          id(value, Write).asInstanceOf[chisel3.domain.Type],
          domain,
          properties.map(arg(_, Read))
        )
      case ir.DefIntrinsicExpr(info, intrinsic, value, args, params) =>
        ir.DefIntrinsicExpr(info, intrinsic, data(value, Write), args.map(arg(_, Read)), params)
      case ir.DefIntrinsic(info, intrinsic, args, params) =>
        ir.DefIntrinsic(info, intrinsic, args.map(arg(_, Read)), params)
      case _: ir.FirrtlComment => value
      case _: ir.Placeholder =>
        throw new UnsupportedOperationException("Cannot rebind a nested Placeholder")
      case other =>
        throw new UnsupportedOperationException(
          withSourceInfo(
            s"Unsupported command in cacheable region: ${other.getClass.getName}",
            other.sourceInfo
          )
        )
    }

    captured.commands.map(command)
  }
}
