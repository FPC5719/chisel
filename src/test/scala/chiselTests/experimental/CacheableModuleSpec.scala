// SPDX-License-Identifier: Apache-2.0

package chiselTests
package experimental

import chisel3._
import chisel3.experimental.cacheable.{CacheableModule, SideEffect}
import chisel3.testing.scalatest.FileCheck
import circt.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CacheableModuleSpec extends AnyFlatSpec with Matchers with FileCheck {

  private object CacheCounter {
    var cacheableRuns = 0
  }

  private object CacheKeyCounter {
    var firstRuns = 0
    var secondRuns = 0
  }

  private object PathCounter {
    var cacheableRuns = 0
  }

  private class EffectModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    // This thunk must not run because the effect is never unwrapped.
    private val unused = SideEffect.pure {
      val wire = Wire(UInt(8.W)).suggestName("unusedEffect")
      wire := io.in
      wire
    }

    private val result = nonCacheable { scope =>
      scope.unwrap(
        SideEffect
          .pure(io.in)
          .map(_ + 1.U)
          .flatMap(value => SideEffect.pure(value ^ "h3c".U)),
        UInt(8.W),
        "effectResult"
      )
    }

    def cacheable(): Unit = {
      io.out := result
    }
  }

  private class UnitEffectModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(Bool())
      val out = Output(Bool())
    })

    nonCacheable { scope =>
      scope.unwrap(SideEffect.pure { io.out := io.in })
    }

    def cacheable(): Unit = {}
  }

  private class CacheablePassModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      io.out := io.in
    }
  }

  private class CacheablePassTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    val child = CacheableModule(new CacheablePassModule)

    child.io.in := io.in
    io.out := child.io.out
  }

  private class CacheableLocalModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      val local = Wire(UInt(8.W))
      local := io.in + 1.U
      io.out := local
    }
  }

  private class CacheableLocalTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    val child = CacheableModule(new CacheableLocalModule)

    child.io.in := io.in
    io.out := child.io.out
  }

  private class CachedTwiceModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      CacheCounter.cacheableRuns += 1
      io.out := io.in + 1.U
    }
  }

  private class CachedTwiceTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out0 = Output(UInt(8.W))
      val out1 = Output(UInt(8.W))
    })
    val first = CacheableModule(new CachedTwiceModule)
    val second = CacheableModule(new CachedTwiceModule)

    first.io.in := io.in
    second.io.in := io.in
    io.out0 := first.io.out
    io.out1 := second.io.out
  }

  private class FirstKeyModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    override protected def cacheKey: Any = 0

    def cacheable(): Unit = {
      CacheKeyCounter.firstRuns += 1
      io.out := io.in
    }
  }

  private class SecondKeyModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    override protected def cacheKey: Any = 0

    def cacheable(): Unit = {
      CacheKeyCounter.secondRuns += 1
      io.out := io.in + 1.U
    }
  }

  private class CacheKeyTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val first = Output(UInt(8.W))
      val second = Output(UInt(8.W))
    })
    val first = CacheableModule(new FirstKeyModule)
    val second = CacheableModule(new SecondKeyModule)

    first.io.in := io.in
    second.io.in := io.in
    io.first := first.io.out
    io.second := second.io.out
  }

  private class PathBoundModule(addLeadingState: Boolean) extends Module with CacheableModule {
    if (addLeadingState) {
      val unrelated = Wire(UInt(8.W)).suggestName("unrelated")
      unrelated := 0.U
    }

    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      PathCounter.cacheableRuns += 1
      io.out := io.in
    }
  }

  private class PathBoundTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out0 = Output(UInt(8.W))
      val out1 = Output(UInt(8.W))
    })
    val first = CacheableModule(new PathBoundModule(addLeadingState = false))
    val second = CacheableModule(new PathBoundModule(addLeadingState = true))

    first.io.in := io.in
    second.io.in := io.in
    io.out0 := first.io.out
    io.out1 := second.io.out
  }

  private class MixedCaptureModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    private val state = Wire(UInt(8.W)).suggestName("state")

    def cacheable(): Unit = {
      state := io.in
      io.out := state
    }
  }

  private class PlainPassModule extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    io.out := io.in
  }

  private class NonDataLocalModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      val nested = Module(new PlainPassModule)
      nested.io.in := io.in
      io.out := nested.io.out
    }
  }

  private class InnerCacheableModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })

    def cacheable(): Unit = {
      io.out := io.in
    }
  }

  private class OuterCacheableModule extends Module with CacheableModule {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    val inner = CacheableModule(new InnerCacheableModule)
    inner.io.in := io.in

    private val result = nonCacheable { scope =>
      scope.unwrap(SideEffect.pure(inner.io.out), UInt(8.W), "outerResult")
    }

    def cacheable(): Unit = {
      io.out := result
    }
  }

  private class NestedCacheableTop extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    val child = CacheableModule(new OuterCacheableModule)

    child.io.in := io.in
    io.out := child.io.out
  }

  private class IllegalCacheableScopeModule extends Module with CacheableModule {
    def cacheable(): Unit = {
      nonCacheable(_ => ())
    }
  }

  "SideEffect" should "evaluate pure, map, and flatMap effects only when unwrapped" in {
    ChiselStage
      .emitCHIRRTL(new Module {
        val io = IO(new Bundle {
          val in = Input(UInt(8.W))
          val out = Output(UInt(8.W))
        })
        val child = CacheableModule(new EffectModule)

        child.io.in := io.in
        io.out := child.io.out
      })
      .fileCheck("--implicit-check-not=unusedEffect")(
        """|CHECK-LABEL: module EffectModule
           |CHECK:       wire effectResult : UInt<8>
           |CHECK:       add(io.in, UInt<1>(0h1))
           |CHECK:       xor({{.*}}, UInt<6>(0h3c))
           |CHECK:       connect effectResult, {{.*}}
           |CHECK:       inst CachePlanModule of CachePlanModule
           |CHECK:       connect io.out, CachePlanModule.cacheable_0_write
           |CHECK:       connect CachePlanModule.cacheable_1_read, effectResult
           |""".stripMargin
      )
  }

  it should "unwrap unit effects without creating a data wire" in {
    ChiselStage
      .emitCHIRRTL(new Module {
        val io = IO(new Bundle {
          val in = Input(Bool())
          val out = Output(Bool())
        })
        val child = CacheableModule(new UnitEffectModule)

        child.io.in := io.in
        io.out := child.io.out
      })
      .fileCheck("--implicit-check-not=wire")(
        """|CHECK-LABEL: module UnitEffectModule
           |CHECK:       connect io.out, io.in
           |""".stripMargin
      )
  }

  "CacheableModule" should "run cacheable after construction when instantiated through its factory" in {
    ChiselStage
      .emitCHIRRTL(new CacheablePassTop)
      .fileCheck()(
        """|CHECK-LABEL: module CachePlanModule
           |CHECK:       input cacheable_1_read
           |CHECK:       output cacheable_0_write
           |CHECK:       connect cacheable_0_write, cacheable_1_read
           |CHECK-LABEL: module CacheablePassModule
           |CHECK:       inst CachePlanModule of CachePlanModule
           |CHECK:       connect io.out, CachePlanModule.cacheable_0_write
           |CHECK:       connect CachePlanModule.cacheable_1_read, io.in
           |CHECK-LABEL: module CacheablePassTop
           |CHECK:       inst child of CacheablePassModule
           |CHECK:       connect child.io.in, io.in
           |CHECK:       connect io.out, child.io.out
           |""".stripMargin
      )
  }

  it should "rebind locals into the synthetic definition" in {
    ChiselStage
      .emitCHIRRTL(new CacheableLocalTop)
      .fileCheck()(
        """|CHECK-LABEL: module CachePlanModule
           |CHECK:       wire _WIRE
           |CHECK:       add(cacheable_0_read, UInt<1>(0h1))
           |CHECK:       connect _WIRE, {{.*}}
           |CHECK:       connect cacheable_1_write, _WIRE
           |CHECK-LABEL: module CacheableLocalModule
           |CHECK:       inst CachePlanModule of CachePlanModule
           |CHECK:       connect CachePlanModule.cacheable_0_read, io.in
           |CHECK:       connect io.out, CachePlanModule.cacheable_1_write
           |""".stripMargin
      )
  }

  it should "lower the synthetic definition and instance" in {
    ChiselStage.emitSystemVerilog(new CacheableLocalTop) should include("module CachePlanModule")
  }

  it should "reuse one cacheable definition without rerunning cacheable" in {
    CacheCounter.cacheableRuns = 0
    ChiselStage
      .emitCHIRRTL(new CachedTwiceTop)
      .fileCheck()(
        """|CHECK-LABEL: module CachedTwiceModule
           |CHECK:       inst CachePlanModule of CachePlanModule
           |CHECK:       inst CachePlanModule of CachePlanModule
           |""".stripMargin
      )
    CacheCounter.cacheableRuns shouldBe 1
  }

  it should "scope custom cache keys to the cacheable module class" in {
    CacheKeyCounter.firstRuns = 0
    CacheKeyCounter.secondRuns = 0
    ChiselStage.emitCHIRRTL(new CacheKeyTop)

    CacheKeyCounter.firstRuns shouldBe 1
    CacheKeyCounter.secondRuns shouldBe 1
  }

  it should "bind cached captures by structural path rather than construction order" in {
    PathCounter.cacheableRuns = 0
    ChiselStage.emitCHIRRTL(new PathBoundTop)

    PathCounter.cacheableRuns shouldBe 1
  }

  it should "reject captures that are both read and written" in {
    val error = the[IllegalArgumentException] thrownBy {
      ChiselStage.emitCHIRRTL(new Module {
        val child = CacheableModule(new MixedCaptureModule)
      })
    }

    error.getMessage should include("both read and written")
  }

  it should "reject non-Data local definitions" in {
    val error = the[IllegalArgumentException] thrownBy {
      ChiselStage.emitCHIRRTL(new Module {
        val child = CacheableModule(new NonDataLocalModule)
      })
    }

    error.getMessage should include("only support Data local definitions")
  }

  it should "restore the construction environment after a nested cacheable module" in {
    ChiselStage
      .emitCHIRRTL(new NestedCacheableTop)
      .fileCheck()(
        """|CHECK-LABEL: module InnerCacheableModule
           |CHECK:       inst CachePlanModule of CachePlanModule
           |CHECK:       connect io.out, CachePlanModule.cacheable_0_write
           |CHECK:       connect CachePlanModule.cacheable_1_read, io.in
           |CHECK-LABEL: module OuterCacheableModule
           |CHECK:       inst inner of InnerCacheableModule
           |CHECK:       wire outerResult : UInt<8>
           |CHECK:       connect outerResult, inner.io.out
           |CHECK:       inst CachePlanModule_1 of CachePlanModule_1
           |CHECK:       connect io.out, CachePlanModule_1.cacheable_0_write
           |CHECK:       connect CachePlanModule_1.cacheable_1_read, outerResult
           |""".stripMargin
      )
  }

  it should "reject non-cacheable scopes while running cacheable" in {
    val error = the[IllegalArgumentException] thrownBy {
      ChiselStage.emitCHIRRTL(new Module {
        val child = CacheableModule(new IllegalCacheableScopeModule)
      })
    }

    error.getMessage should include("Must not enter a non-cacheable scope from a cacheable environment")
  }
}
