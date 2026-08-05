package chisel3.experimental.cacheable

import chisel3._
import chisel3.experimental.hierarchy._
import chisel3.experimental.SourceInfo
import chisel3.internal.Builder
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

  private var _cachePlan: Option[CachePlan] = None

  private[cacheable] final def cachePlan: Option[CachePlan] = _cachePlan

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
    val placeholder = new ir.Placeholder(sourceInfo)
    val block = Builder.currentBlock.get
    val beforeIds = _ids.toSet
    val state = Builder.State.save
    Builder.State.guard(state) {
      block.appendToPlaceholder(placeholder) {
        cacheable()
      }
    }
    val (_, commands) = ir.Placeholder.unapply(placeholder).get
    _cachePlan = Some(CachePlan.capture(beforeIds, _ids.toSet, commands))
    // Keep the first implementation behaviorally complete until the synthetic-module materializer
    // consumes this plan.  The placeholder is still detached while capture runs, so append it
    // after analysis to retain the region in the enclosing module.
    block.addCommand(placeholder)
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
