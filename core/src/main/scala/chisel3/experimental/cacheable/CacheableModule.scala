// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.{doNotDedup, BaseModule, SourceInfo}
import chisel3.internal.{Builder, BuilderContextCache, DynamicContext}
import chisel3.internal.firrtl.ir
import chisel3.internal.throwException
import scala.reflect.ClassTag

/** Provides the cache discriminator for a [[CacheableModule]].
  *
  * An instance of this type class must be available implicitly for the module type `T` when
  * [[CacheableModule.apply]] is called. The idiomatic way to provide one is as an implicit member
  * of the companion object of the module class:
  *
  * {{{
  * class MyModule(count: Int) extends Module with CacheableModule {
  *   // ...
  * }
  *
  * object MyModule {
  *   implicit object cacheKey extends CacheableKey[MyModule] {
  *     override def cacheKey(args: Seq[Any]): Any = args
  *   }
  * }
  * }}}
  *
  * Definitions are shared between calls whose [[cacheKey]] values are equal, so this type class is
  * the mechanism by which parameterized modules avoid being re-elaborated.
  *
  * @tparam T the cacheable module class this key applies to
  */
abstract class CacheableKey[T <: CacheableModule.CacheableBaseModule] {

  /** Maps the arguments passed to [[CacheableModule.apply]] to the key that identifies the cached
    * definition.
    *
    * The module class itself is always part of the cache identity, so only the argument-dependent
    * part has to be supplied here. The default implementation returns `()`, which gives every call
    * for a given module class the same definition. Override it when the arguments change the
    * elaborated hardware.
    *
    * The returned value must have stable `equals` and `hashCode` for the duration of one
    * elaboration.
    *
    * @param args the trailing arguments passed to [[CacheableModule.apply]]
    */
  def cacheKey(args: Seq[Any]): Any = ()
}

/** A module whose definition is elaborated at most once and then shared by every instance that has
  * the same cache key.
  *
  * Mix this trait into a [[chisel3.Module]] and build the module body in
  * [[buildModule]]. The module's definition is elaborated the first time an instance is requested
  * with [[CacheableModule.apply]]; subsequent requests reuse it. `apply` evaluates the module
  * constructor once to build the shared definition and once per instance to bind that instance to
  * the definition, so the constructor can branch on the [[CacheableModule.ElaborationPhase]] it is
  * given.
  *
  * {{{
  * class MyModule extends Module with CacheableModule {
  *   val io = IO(new Bundle {
  *     val in  = Input(UInt(8.W))
  *     val out = Output(UInt(8.W))
  *   })
  *
  *   protected def buildModule(): Unit = {
  *     io.out := io.in
  *   }
  * }
  * }}}
  */
trait CacheableModule { self: BaseModule =>

  /** Elaborates the body of this module.
    *
    * This is called exactly once per cached definition, while the module is first being built
    * ([[CacheableModule.Defining]]). It is not called when additional instances of an already-built
    * definition are created, so logic placed here runs only once no matter how many instances
    * exist.
    */
  protected def buildModule(): Unit
}

/** Factory and supporting types for creating [[CacheableModule]] instances.
  *
  * Instances are created with [[apply]]. The first call for a given (module class,
  * [[CacheableKey]]) pair elaborates the module definition; later calls with an equal key reuse it
  * and only create a new instance that references the shared definition.
  */
object CacheableModule {

  /** A [[chisel3.experimental.BaseModule]] that also mixes in [[CacheableModule]]. */
  type CacheableBaseModule = BaseModule with CacheableModule

  private case class CacheEntry(
    circuit: ElaboratedCircuit,
    module:  CacheableBaseModule
  )

  private case class CacheKey(
    moduleClass: ClassTag[_],
    cacheKey:    Any
  ) extends BuilderContextCache.Key[CacheEntry]

  /** The phase in which a cacheable module's constructor is being evaluated.
    *
    * The constructor supplied to [[apply]] is invoked for both phases, so it can branch on the
    * phase when building the definition and creating an instance require different work.
    */
  sealed trait ElaborationPhase

  /** The module's definition is being elaborated for the first time.
    *
    * [[CacheableModule.buildModule]] is called once the constructor returns, to emit the module
    * body.
    */
  case object Defining extends ElaborationPhase

  /** The constructor is being evaluated to create an additional instance of a definition that has
    * already been elaborated.
    *
    * Only the module's externally visible interface is elaborated in this phase;
    * [[CacheableModule.buildModule]] is not called, and the instance is tied to `fullDefinition`.
    *
    * @param fullDefinition the cached definition this instance refers to
    */
  final case class Instancing(
    fullDefinition: CacheableBaseModule
  ) extends ElaborationPhase

  private def getDynamicContext: DynamicContext = {
    val context = Builder.captureContext()
    val res = new DynamicContext(
      firrtl.seqToAnnoSeq(Nil),
      context.throwOnFirstError,
      context.useLegacyWidth,
      context.includeUtilMetadata,
      context.useSRAMBlackbox,
      context.warningFilters,
      context.sourceRoots,
      Some(context.globalNamespace),
      context.loggerOptions,
      context.definitions,
      context.contextCache,
      context.layerMap,
      context.inlineTestIncluder,
      context.suppressSourceInfo,
      context.elideLayerBlocks,
      context.elaborationTrace
    )
    res.inDefinition = true
    res
  }

  private def elaborateWrapper[
    T <: CacheableBaseModule
  ](bc: (ElaborationPhase) => T)(implicit sourceInfo: SourceInfo): T = {
    Builder.readyForModuleConstr = true
    Builder.elaborationTrace.pushModule()
    val savedPrefixStack = Builder.getModulePrefixStack
    val module = Builder.State.guard(Builder.State.default) {
      val module: T = bc(Defining)
      module.generateComponent().foreach(Builder.components += _)
      if (module.localModulePrefix.isDefined)
        Builder.popModulePrefix()
      if (module.ignoreParentPrefix)
        Builder.setModulePrefixStack(savedPrefixStack)
      module
    }
    Builder.elaborationTrace.popModule(module.desiredName)
    module.moduleBuilt()
    module
  }

  /** Creates an instance of the cacheable module produced by `bc`, elaborating its definition if it
    * has not been built yet.
    *
    * The definition is cached per (module class, [[CacheableKey]]) pair. The first call for a given
    * key evaluates `bc` with [[Defining]] and then calls [[CacheableModule.buildModule]] to build
    * the shared definition. Every call, including the first, then evaluates `bc` again with
    * [[Instancing]] to produce the returned instance and ties it to the definition.
    *
    * Hardware and metadata exposures recorded while building the cached definition are forwarded
    * to each created instance. Metadata exposures remain metadata; hardware exposures become
    * cached port exposures using the original port type and name.
    *
    * @param bc   the module constructor; it is evaluated once per phase
    * @param args trailing arguments, forwarded to the [[CacheableKey]] to select the definition
    * @tparam T   the cacheable module type
    * @return the newly created instance
    */
  def apply[
    T <: CacheableBaseModule: ClassTag: CacheableKey
  ](bc: (ElaborationPhase) => T, args: Any*)(implicit sourceInfo: SourceInfo): T = {
    val key = CacheKey(implicitly[ClassTag[T]], implicitly[CacheableKey[T]].cacheKey(args))
    val dynctx = getDynamicContext
    val entry = Builder.contextCache.getOrElseUpdate(
      key, {
        val (ir, module) = Builder.build(
          elaborateWrapper { phase =>
            val mod = bc(phase)
            mod.buildModule()
            mod
          },
          dynctx
        )
        Builder.components ++= ir._circuit.components
        Builder.annotations ++= ir._circuit.annotations
        Builder.layers ++= dynctx.layers
        Builder.options ++= dynctx.options
        Builder.domains ++= dynctx.domains
        dynctx.definitions.foreach(Builder.addDefinition)
        module._circuit = Builder.currentModule
        CacheEntry(ir, module)
      }
    )
    // Instantiate the module
    Builder.readyForModuleConstr = true
    val module = Builder.State.guard(Builder.State.default) {
      // Elaborate the public accessible interfaces only
      val module: T = bc(Instancing(entry.module))
      // Forward exposures from the shared definition to this instance. Metadata stays metadata,
      // while port-backed exposures are rebuilt from their cached port type.
      entry.module.exposures.foreach {
        case item: ModuleExposure.Metadata =>
          module.exposures += item
        case item: ModuleExposure.Real =>
          module.exposures += ModuleExposure.Cached(
            item.tag,
            item.isUpward,
            chiselTypeOf(item.data),
            item.data.earlyName,
            item.sourceInfo
          )
        case item: ModuleExposure.Cached =>
          module.exposures += item
      }
      module.generateComponent()
      module
    }
    Builder.pushCommand(ir.DefInstanceFrom(sourceInfo, module, module._component.get.ports, entry.module))
    module.initializeInParent() // Connect Clock/Reset
    module.propagateExposure()
    module
  }

  /** Creates an instance of a cacheable module whose constructor does not depend on the elaboration
    * phase.
    *
    * `bc` is evaluated in both the [[Defining]] and [[Instancing]] phases, so it must be safe to run
    * more than once. See the phase-aware overload for the general form.
    */
  def apply[
    T <: CacheableBaseModule: ClassTag: CacheableKey
  ](bc: => T, args: Any*)(implicit sourceInfo: SourceInfo): T = {
    apply(_ => bc, args: _*)
  }
}
