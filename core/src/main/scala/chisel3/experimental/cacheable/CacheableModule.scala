package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.hierarchy._
import chisel3.experimental.SourceInfo
import chisel3.experimental.BaseModule
import chisel3.internal.Builder
import chisel3.internal.BuilderContextCache
import chisel3.internal.firrtl.ir

trait CacheableModuleBase { self: Module =>
  import CacheableModuleBase.{currentEnv, CacheKey}

  protected object NonCacheable {
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

  protected final def nonCacheable[T](body: NonCacheable.type => T): T = {
    require(
      !currentEnv.cacheable,
      "Must not enter a non-cacheable scope from a cacheable environment"
    )
    body(NonCacheable)
  }

  private final def elaborateCacheable(implicit sourceInfo: SourceInfo): Unit = {
    require(
      currentEnv.cacheable,
      "cacheable() must be elaborated in a cacheable environment"
    )
    val beforeIds = _ids.toIndexedSeq
    val key = CacheKey(getClass, cacheKey)
    val cached = Builder.contextCache.get(key).getOrElse {
      val placeholder = new ir.Placeholder(sourceInfo)
      val state = Builder.State.save
      Builder.State.guard(state) {
        Builder.currentBlock.get.appendToPlaceholder(placeholder) {
          cacheable()
        }
      }
      val (_, commands) = ir.Placeholder.unapply(placeholder).get
      val captured = CachePlan.capture(beforeIds, _ids.toIndexedSeq, commands)
      val cached = CachePlan.cache(captured)
      Builder.contextCache.put(key, cached)
      cached
    }
    CachePlan.instantiate(cached, beforeIds)
  }

  /** Additional cache discriminator for the synthetic cacheable definition.
   *
    * The module class is always part of the cache identity. Override this when constructor
    * parameters or external configuration alter `cacheable()`.
    * Values must have stable `equals`/`hashCode` during one elaboration.
    */
  protected def cacheKey: Any = ()

  protected def cacheable(): Unit
}

object CacheableModuleBase {
  private case class CacheKey(
    moduleClass: Class[_],
    key:         Any
  ) extends BuilderContextCache.Key[CachePlan.CachedRegion]

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

  private[cacheable] def instantiate[
    T <: BaseModule with CacheableModuleBase
  ](bc: => T): T = Module {
    inEnv(Env(cacheable = false)) {
      val module = bc
      inEnv(Env(cacheable = true)) {
        module.elaborateCacheable
      }
      module
    }
  }
}

trait CacheableModule extends CacheableModuleBase { self: Module => }

object CacheableModule {
  def apply[
    T <: BaseModule with CacheableModule
  ](bc: => T): T = CacheableModuleBase.instantiate(bc)
}
