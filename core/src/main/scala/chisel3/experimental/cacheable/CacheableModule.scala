package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.hierarchy._
import chisel3.experimental.SourceInfo
import chisel3.internal.Builder
import chisel3.internal.BuilderContextCache
import chisel3.internal.firrtl.ir

abstract class CacheableModuleBase extends Module {
  import CacheableModuleBase.currentEnv

  protected final class NonCacheable() {
    def unwrap(eff: SideEffect[Unit], name: String = ""): Unit = {
      require(
        !currentEnv.cacheable,
        "Must not unwrap in a cacheable environment"
      )
      eff.evaluate()
    }

    def unwrap[T <: Data](eff: SideEffect[T], gen: T, name: String): T = {
      require(
        !currentEnv.cacheable,
        "Must not unwrap in a cacheable environment"
      )
      val wire = Wire(gen)
      wire := eff.evaluate()
      wire.suggestName(name)
    }
  }

  private val nonCacheableScope = new NonCacheable

  /** Cache discriminator for the synthetic cacheable definition.
    *
    * Override this when constructor parameters or external configuration alter `cacheable()`.
    * Values must have stable `equals`/`hashCode` during one elaboration.
    */
  protected def cacheKey: Any = getClass

  protected final def nonCacheable[T](body: NonCacheable => T): T = {
    require(
      !currentEnv.cacheable,
      "Must not enter a non-cacheable scope from a cacheable environment"
    )
    body(nonCacheableScope)
  }

  private final def elaborateCacheable(implicit sourceInfo: SourceInfo): Unit = {
    require(
      currentEnv.cacheable,
      "cacheable() must be elaborated in a cacheable environment"
    )
    val preCacheableIds = _ids.toIndexedSeq
    val shape = preCacheableIds.map {
      case data: Data =>
        data.typeName + ":" + getRecursiveFields.noPath(data).map(_.typeName).mkString("[")
      case other => other.getClass.getName
    }
    val key = CacheableModuleBase.CacheKey(cacheKey, Module.currentModulePrefix, Builder.elideLayerBlocks, shape)
    Builder.contextCache.get(key) match {
      case Some(cached) =>
        CachePlan.instantiate(cached, preCacheableIds)
      case None =>
        val placeholder = new ir.Placeholder(sourceInfo)
        val block = Builder.currentBlock.get
        val state = Builder.State.save
        Builder.State.guard(state) {
          block.appendToPlaceholder(placeholder) {
            cacheable()
          }
        }
        val (_, commands) = ir.Placeholder.unapply(placeholder).get
        val plan = CachePlan.capture(preCacheableIds.toSet, _ids.toSet, commands)
        val cached = CachePlan.cache(plan, preCacheableIds)
        Builder.contextCache.put(key, cached)
        CachePlan.instantiate(cached, preCacheableIds)
    }
  }

  def cacheable(): Unit
}

object CacheableModuleBase {
  private final case class CacheKey(
    key:               Any,
    modulePrefix:      String,
    elideLayerBlocks:  Boolean,
    preCacheableShape: Seq[String]
  ) extends BuilderContextCache.Key[CachePlan.CachedPlan]

  private case class Env(cacheable: Boolean)

  private val envStack = new ThreadLocal[List[Env]] {
    override def initialValue() = List[Env]()
  }

  private def currentEnv: Env = envStack.get().headOption.getOrElse {
    throw new IllegalStateException(
      "ModuleEff must be constructed with ModuleEff(...)"
    )
  }

  private def inEnv[T](env: Env)(body: => T): T = {
    val old = envStack.get()
    envStack.set(env :: old)
    try body
    finally envStack.set(old)
  }

  private[cacheable] def instantiate[T <: CacheableModuleBase](bc: => T): T = Module {
    inEnv(Env(cacheable = false)) {
      val module = bc
      inEnv(Env(cacheable = true)) {
        module.elaborateCacheable
      }
      module
    }
  }
}

abstract class CacheableModule extends CacheableModuleBase

object CacheableModule {
  def apply[T <: CacheableModule](bc: => T): T =
    CacheableModuleBase.instantiate(bc)
}
