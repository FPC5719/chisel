package chisel3.experimental.cacheable

import chisel3._

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

  protected final def nonCacheable[T](body: NonCacheable => T): T = {
    require(
      !currentEnv.cacheable,
      "Must not enter a non-cacheable scope from a cacheable environment"
    )
    body(nonCacheableScope)
  }

  private final def elaborateCacheable(): Unit = {
    require(
      currentEnv.cacheable,
      "cacheable() must be elaborated in a cacheable environment"
    )
    cacheable()
  }

  def cacheable(): Unit
}

object CacheableModuleBase {
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
        module.elaborateCacheable()
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
