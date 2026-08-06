package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.experimental.dataview.reifySingleTarget
import chisel3.experimental.hierarchy.ModuleClone
import chisel3.experimental.hierarchy.core.{Clone, Definition, Instance}
import chisel3.internal.HasId
import chisel3.internal.Builder
import chisel3.internal.binding.{InstanceChoiceBinding, MemoryPortBinding, OpBinding, RegBinding, WireBinding}
import chisel3.internal.firrtl.ir

import scala.collection.immutable.ListMap
import scala.collection.mutable

/** The result of capturing one cacheable command region.
  *
  * This is deliberately an internal representation for now.  Commands still refer to the
  * prototype module's ids; the captures describe the references which must be re-bound when the
  * plan is materialized in a synthetic module.
  */
private[cacheable] final case class CachePlan(
  commands: Seq[ir.Command],
  localIds: Set[HasId],
  captures: Seq[CachePlan.Capture]
)

private[cacheable] object CachePlan {
  sealed trait Access
  case object Read extends Access
  case object Write extends Access

  final case class Capture(id: HasId, accesses: Set[Access]) {
    def data: Option[Data] = id match {
      case value: Data => Some(value)
      case _ => None
    }
  }

  /** Mapping used when a captured command region is copied into another module.
    *
    * Read and write mappings are intentionally separate.  A surrounding wire can be both read
    * and written by a region, in which case the synthetic module needs an input and an output
    * port rather than one ambiguous replacement.
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

  /** A synthetic-module port candidate inferred from the captured command closure. */
  final case class PortCapture(index: Int, capture: Capture, direction: Access, gen: Data) {
    def name: String = s"cacheable_${index}_${direction.toString.toLowerCase}"

    def ioType: Data = direction match {
      case Read  => Input(gen)
      case Write => Output(gen)
    }
  }

  def ports(plan: CachePlan): Seq[PortCapture] = {
    var index = 0
    plan.captures.flatMap { capture =>
      capture.data.toSeq.flatMap { data =>
        capture.accesses.toSeq.sortBy(_.toString).map { direction =>
          val result = PortCapture(index, capture, direction, data.cloneTypeFull)
          index += 1
          result
        }
      }
    }
  }

  /** Build the internal IO record used by a synthetic cache-plan module. */
  private[cacheable] final class IORecord(specs: Seq[PortCapture]) extends Record {
    override val elements:  ListMap[String, Data] = ListMap(specs.map(spec => spec.name -> spec.ioType): _*)
    override def cloneType: this.type = new IORecord(specs).asInstanceOf[this.type]
  }

  /** Create read/write rebinding maps for an instantiated [[IORecord]]. */
  def portRebinding(plan: CachePlan, io: Record, local: Map[HasId, HasId]): Rebinding = {
    val specs = ports(plan)
    // FlatIO returns a Record DataView.  Its elements must be reified to the actual module ports
    // before they are inserted into command IR, otherwise the copied commands refer to the view
    // root rather than a FIRRTL port.
    val fields = specs.map { spec =>
      val field = io.elements(spec.name)
      spec -> reifySingleTarget(field).getOrElse(field)
    }.toMap
    Rebinding(
      local = local,
      reads = specs.collect { case spec @ PortCapture(_, capture, Read, _) => capture.id -> fields(spec) }.toMap,
      writes = specs.collect { case spec @ PortCapture(_, capture, Write, _) => capture.id -> fields(spec) }.toMap
    )
  }

  /** Materialize a cache plan in a private module definition and connect one instance in the
    * current module.
    *
    * The Definition/Instance hierarchy machinery handles the module boundary.  The only state
    * copied manually is the captured command graph, after all prototype ids have been rebound to
    * either synthetic-module ports or fresh local ids.
    */
  def materialize(plan: CachePlan)(implicit sourceInfo: SourceInfo): Unit = {
    val definition = Definition(new CachePlanModule(plan))
    val instance = Instance(definition)
    val instancePorts = instance.underlying match {
      case Clone(module: ModuleClone[_]) => module.getPorts
      case other =>
        throw new IllegalStateException(s"Unexpected cache-plan instance representation: $other")
    }

    ports(plan).foreach { spec =>
      val instancePort = instancePorts.elements(spec.name)
      val captured = spec.capture.data.get
      spec.direction match {
        case Read  => instancePort := captured
        case Write => captured := instancePort
      }
    }
  }

  private final class CachePlanModule(plan: CachePlan)(implicit sourceInfo: SourceInfo) extends RawModule {
    private val specs = ports(plan)
    val io = FlatIO(new IORecord(specs))

    private val local = plan.localIds.iterator.map { original =>
      original -> cloneLocal(original)
    }.toMap
    private val rebinding = portRebinding(plan, io, local)
    rebind(plan, rebinding).foreach(Builder.pushCommand)

    override def desiredName: String = "CachePlanModule"

    private def cloneLocal(original: HasId): HasId = original match {
      case data: Data =>
        val clone = data.cloneTypeFull
        val binding = data.topBinding match {
          case _: WireBinding           => WireBinding(this, Builder.currentBlock)
          case _: RegBinding            => RegBinding(this, Builder.currentBlock)
          case _: OpBinding             => OpBinding(this, Builder.currentBlock)
          case _: MemoryPortBinding     => MemoryPortBinding(this, Builder.currentBlock)
          case _: InstanceChoiceBinding => InstanceChoiceBinding(this, Builder.currentBlock)
          case other =>
            throw new UnsupportedOperationException(
              s"Cannot rebind cache-plan local ${data.getClass.getName} with binding $other"
            )
        }
        clone.bind(binding)
        clone
      case other =>
        throw new UnsupportedOperationException(
          s"Cannot rebind non-Data cache-plan local ${other.getClass.getName}"
        )
    }
  }

  def capture(beforeIds: Set[HasId], afterIds: Set[HasId], commands: Seq[ir.Command]): CachePlan = {
    // Some command-defined ids (notably printf/verification statements) are not registered in
    // Module._ids.  Collect definitions explicitly so they are treated as locals rather than
    // synthetic IO captures.
    val commandIds = mutable.HashSet.empty[HasId]
    def collectDefinitions(command: ir.Command): Unit = command match {
      case definition: ir.Definition => commandIds += definition.id
      case ir.When(_, _, ifCommands, elseCommands) =>
        ifCommands.foreach(collectDefinitions)
        elseCommands.foreach(collectDefinitions)
      case ir.LayerBlock(_, _, nested) => nested.foreach(collectDefinitions)
      case contract: ir.DefContract =>
        contract.ids.foreach(commandIds += _)
        contract.region.getAllCommands().foreach(collectDefinitions)
      case _: ir.FirrtlComment | _: ir.Placeholder =>
      case _                                       =>
    }
    commands.foreach(collectDefinitions)

    val localIds = (afterIds -- beforeIds) ++ commandIds
    val accesses = new mutable.LinkedHashMap[HasId, mutable.Set[Access]]

    def record(id: HasId, access: Access): Unit = {
      if (!localIds.contains(id)) {
        accesses.getOrElseUpdate(id, mutable.Set.empty) += access
      }
    }

    def recordArg(arg: ir.Arg, access: Access): Unit = arg match {
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

    def recordPrintable(value: Printable, access: Access): Unit = {
      // Name and FullName are resolved while elaborating the printable and do not become FIRRTL
      // arguments.  Counting their backing Data here would create an unnecessary synthetic port.
      Printable.unpackFirrtlArgs(value).foreach(data => record(data, access))
    }

    def recordCommand(command: ir.Command): Unit = command match {
      case ir.DefPrim(_, id, _, args @ _*) =>
        record(id, Write)
        args.foreach(recordArg(_, Read))
      case ir.DefInvalid(_, arg) => recordArg(arg, Write)
      case ir.DefWire(_, id)     => record(id, Write)
      case ir.DefReg(_, id, clock) =>
        record(id, Write)
        recordArg(clock, Read)
      case ir.DefRegInit(_, id, clock, reset, init) =>
        record(id, Write)
        recordArg(clock, Read)
        recordArg(reset, Read)
        recordArg(init, Read)
      case ir.DefMemory(_, id, _, _)                   => record(id, Write)
      case ir.DefSeqMemory(_, id, _, _, _)             => record(id, Write)
      case ir.FirrtlMemory(_, id, _, _, _, _, _, _, _) => record(id, Write)
      case ir.DefMemPort(_, id, source, _, index, clock) =>
        record(id, Write)
        recordArg(source, Read)
        recordArg(index, Read)
        recordArg(clock, Read)
      case ir.DefInstance(_, id, _)             => record(id, Write)
      case ir.DefInstanceChoice(_, id, _, _, _) => record(id, Write)
      case ir.DefObject(_, id, _)               => record(id, Write)
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
      case ir.Stop(id, _, clock, _) =>
        record(id, Write)
        recordArg(clock, Read)
      case ir.LayerBlock(_, _, commands) => commands.foreach(recordCommand)
      case ir.DefContract(_, ids, exprs) =>
        ids.foreach(record(_, Write))
        exprs.foreach(recordArg(_, Read))
      case ir.Printf(id, _, filename, clock, pable) =>
        record(id, Write)
        filename.foreach(recordPrintable(_, Read))
        recordArg(clock, Read)
        recordPrintable(pable, Read)
      case ir.Flush(_, filename, clock) =>
        filename.foreach(recordPrintable(_, Read))
        recordArg(clock, Read)
      case ir.ProbeDefine(_, sink, probe) =>
        recordArg(sink, Write)
        recordArg(probe, Read)
      case ir.ProbeForceInitial(_, probe, value) =>
        recordArg(probe, Write)
        recordArg(value, Read)
      case ir.ProbeReleaseInitial(_, probe) => recordArg(probe, Write)
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
        record(id, Write)
        properties.foreach(recordArg(_, Read))
      case ir.Verification(id, _, _, clock, predicate, pable) =>
        record(id, Write)
        recordArg(clock, Read)
        recordArg(predicate, Read)
        recordPrintable(pable, Read)
      case ir.DefIntrinsicExpr(_, _, id, args, _) =>
        record(id, Write)
        args.foreach(recordArg(_, Read))
      case ir.DefIntrinsic(_, _, args, _)          => args.foreach(recordArg(_, Read))
      case _: ir.FirrtlComment | _: ir.Placeholder =>
      case other =>
        throw new UnsupportedOperationException(
          s"Unsupported command in cacheable region: ${other.getClass.getName}"
        )
    }

    commands.foreach(recordCommand)
    CachePlan(
      commands,
      localIds,
      accesses.iterator.map { case (id, modes) => Capture(id, modes.toSet) }.toSeq
    )
  }

  /** Rebind a captured command region to fresh IDs.
    *
    * This copies the Chisel IR command objects; it does not mutate the prototype commands.  The
    * caller is responsible for allocating fresh local IDs and synthetic IOs, then supplying all
    * mappings in `rebinding`.
    */
  def rebind(plan: CachePlan, rebinding: Rebinding): Seq[ir.Command] = {
    plan.localIds.foreach { value =>
      require(
        rebinding.local.contains(value),
        s"Missing local rebinding for ${value.getClass.getName}"
      )
    }
    plan.captures.foreach { capture =>
      capture.accesses.foreach { access =>
        val mapping = access match {
          case Read  => rebinding.reads
          case Write => rebinding.writes
        }
        require(
          mapping.contains(capture.id) || rebinding.local.contains(capture.id),
          s"Missing ${access.toString.toLowerCase} rebinding for ${capture.id.getClass.getName}"
        )
      }
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

    def printable(value: Printable): Printable = value match {
      case Printables(values) => Printables(values.map(printable))
      case name:     Name     => name
      case fullName: FullName => fullName
      case other =>
        val (format, values) = other.unpack
        Printable.pack(format, values.map(data => id(data, Read).asInstanceOf[Data]): _*)
    }

    def data(value: Data, access: Access): Data = id(value, access).asInstanceOf[Data]

    def block(commands: Seq[ir.Command]): Seq[ir.Command] = commands.map(command)

    def command(value: ir.Command): ir.Command = value match {
      case ir.DefPrim(info, value, op, args @ _*) =>
        ir.DefPrim(info, data(value, Write), op, args.map(arg(_, Read)): _*)
      case ir.DefInvalid(info, value)    => ir.DefInvalid(info, arg(value, Write))
      case ir.DefWire(info, value)       => ir.DefWire(info, data(value, Write))
      case ir.DefReg(info, value, clock) => ir.DefReg(info, data(value, Write), arg(clock, Read))
      case ir.DefRegInit(info, value, clock, reset, init) =>
        ir.DefRegInit(info, data(value, Write), arg(clock, Read), arg(reset, Read), arg(init, Read))
      case ir.DefMemory(info, value, t, size) => ir.DefMemory(info, id(value, Write), t, size)
      case ir.DefSeqMemory(info, value, t, size, readUnderWrite) =>
        ir.DefSeqMemory(info, id(value, Write), t, size, readUnderWrite)
      case ir.FirrtlMemory(info, value, t, size, readPorts, writePorts, readWritePorts, readLatency, writeLatency) =>
        ir.FirrtlMemory(
          info,
          id(value, Write),
          t,
          size,
          readPorts,
          writePorts,
          readWritePorts,
          readLatency,
          writeLatency
        )
      case ir.DefMemPort(info, value, source, direction, index, clock) =>
        ir.DefMemPort(
          info,
          data(value, Write),
          arg(source, Read).asInstanceOf[ir.Node],
          direction,
          arg(index, Read),
          arg(clock, Read)
        )
      case ir.DefInstance(info, value, ports) =>
        throw new UnsupportedOperationException("Nested module instances are not yet supported in CachePlan")
      case ir.DefInstanceChoice(info, value, default, option, choices) =>
        throw new UnsupportedOperationException("Module choices are not yet supported in CachePlan")
      case ir.DefObject(info, value, className) => ir.DefObject(info, id(value, Write), className)
      case value: ir.When =>
        val result = new ir.When(value.sourceInfo, arg(value.pred, Read))
        block(value.ifRegion.getAllCommands()).foreach(result.ifRegion.addCommand)
        if (value.hasElse) {
          block(value.elseRegion.getAllCommands()).foreach(result.elseRegion.addCommand)
        }
        result
      case ir.Connect(info, loc, exp)    => ir.Connect(info, arg(loc, Write), arg(exp, Read))
      case ir.PropAssign(info, loc, exp) => ir.PropAssign(info, arg(loc, Write).asInstanceOf[ir.Node], arg(exp, Read))
      case ir.PropertyAssert(info, condition, message) => ir.PropertyAssert(info, arg(condition, Read), message)
      case ir.Attach(info, locs) => ir.Attach(info, locs.map(value => arg(value, Write).asInstanceOf[ir.Node]))
      case ir.Stop(value, info, clock, ret) =>
        ir.Stop(id(value, Write).asInstanceOf[chisel3.stop.Stop], info, arg(clock, Read), ret)
      case value: ir.LayerBlock =>
        val result = new ir.LayerBlock(value.sourceInfo, value.layer)
        block(value.region.getAllCommands()).foreach(result.region.addCommand)
        result
      case value: ir.DefContract =>
        val result = ir.DefContract(
          value.sourceInfo,
          value.ids.map(data(_, Write)),
          value.exprs.map(arg(_, Read))
        )
        block(value.region.getAllCommands()).foreach(result.region.addCommand)
        result
      case ir.Printf(value, info, filename, clock, pable) =>
        ir.Printf(
          id(value, Write).asInstanceOf[chisel3.printf.Printf],
          info,
          filename.map(printable),
          arg(clock, Read),
          printable(pable)
        )
      case ir.Flush(info, filename, clock)          => ir.Flush(info, filename.map(printable), arg(clock, Read))
      case ir.ProbeDefine(info, sink, probe)        => ir.ProbeDefine(info, arg(sink, Write), arg(probe, Read))
      case ir.ProbeForceInitial(info, probe, value) => ir.ProbeForceInitial(info, arg(probe, Write), arg(value, Read))
      case ir.ProbeReleaseInitial(info, probe)      => ir.ProbeReleaseInitial(info, arg(probe, Write))
      case ir.ProbeForce(info, clock, cond, probe, value) =>
        ir.ProbeForce(info, arg(clock, Read), arg(cond, Read), arg(probe, Write), arg(value, Read))
      case ir.ProbeRelease(info, clock, cond, probe) =>
        ir.ProbeRelease(info, arg(clock, Read), arg(cond, Read), arg(probe, Write))
      case ir.DomainDefine(info, sink, source) => ir.DomainDefine(info, arg(sink, Write), arg(source, Read))
      case ir.DomainInstance(info, value, domain, properties) =>
        ir.DomainInstance(
          info,
          id(value, Write).asInstanceOf[chisel3.domain.Type],
          domain,
          properties.map(arg(_, Read))
        )
      case ir.Verification(value, op, info, clock, predicate, pable) =>
        ir.Verification(
          id(value, Write).asInstanceOf[chisel3.VerificationStatement],
          op,
          info,
          arg(clock, Read),
          arg(predicate, Read),
          printable(pable)
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
          s"Unsupported command in cacheable region: ${other.getClass.getName}"
        )
    }

    plan.commands.map(command)
  }
}
