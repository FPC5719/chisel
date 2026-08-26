package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.{doNotDedup, BaseModule, SourceInfo}
import chisel3.internal.{Builder, BuilderContextCache, DynamicContext}
import chisel3.internal.firrtl.ir
import chisel3.internal.throwException
import scala.reflect.ClassTag

abstract class CacheableKey[T <: BaseModule with CacheableModule] {
  def cacheKey(args: Seq[Any]): Any = ()
}

trait CacheableModule { self: BaseModule =>
  protected def buildModule(): Unit
}

object CacheableModule {
  private case class CacheEntry(
    circuit: ElaboratedCircuit,
    module:  BaseModule
  )

  private case class CacheKey(
    moduleClass: ClassTag[_],
    cacheKey:    Any
  ) extends BuilderContextCache.Key[CacheEntry]

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
    T <: BaseModule with CacheableModule
  ](bc: => T)(implicit sourceInfo: SourceInfo): T = {
    Builder.readyForModuleConstr = true
    Builder.elaborationTrace.pushModule()
    val savedPrefixStack = Builder.getModulePrefixStack
    val module = Builder.State.guard(Builder.State.default) {
      val module: T = bc
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

  def apply[
    T <: BaseModule with CacheableModule: ClassTag: CacheableKey
  ](bc: => T, args: Any*)(implicit sourceInfo: SourceInfo): T = {
    val key = CacheKey(implicitly[ClassTag[T]], implicitly[CacheableKey[T]].cacheKey(args))
    val dynctx = getDynamicContext
    val entry = Builder.contextCache.getOrElseUpdate(
      key, {
        val (ir, module) = Builder.build(
          elaborateWrapper {
            val mod = bc
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
      val module: T = bc
      module.generateComponent()
      module
    }
    Builder.pushCommand(ir.DefInstanceFrom(sourceInfo, module, module._component.get.ports, entry.module))
    module.initializeInParent() // Connect Clock/Reset
    module
  }
}
