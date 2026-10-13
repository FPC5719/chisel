// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.paramchoice

import chisel3._
import chisel3.internal.Builder
import chisel3.internal.firrtl.ir
import chisel3.internal.binding.ParamInstanceChoiceBinding
import chisel3.properties.Property
import chisel3.experimental.{BaseModule, SourceInfo}

/** Instantiates one of several interchangeable module implementations and selects among them with
  * a run-time `Property` value.
  *
  * This is the parameterized counterpart of [[chisel3.choice.ModuleChoice]]: instead of a
  * compile-time [[chisel3.choice.Group]], the selection is a [[chisel3.properties.Property]] whose
  * type is a [[ChoiceDomain.Case]]. The default implementation and every alternative must expose
  * type-equivalent IO bundles, and the choice is lowered to a FIRRTL `paraminstchoice` statement
  * that downstream tools resolve to a concrete implementation.
  *
  * {{{
  * object Platform extends ChoiceDomain {
  *   val FPGA = Case("FPGA")
  *   val ASIC = Case("ASIC")
  * }
  *
  * class Top extends Module {
  *   val select = IO(Input(Property[Platform.Case]()))
  *   val impl = ParamModuleChoice(select, new DefaultImpl, Seq(
  *     Platform.FPGA -> new FpgaImpl,
  *     Platform.ASIC -> new AsicImpl
  *   ))
  * }
  * }}}
  */
object ParamModuleChoice {

  /** Builds the implementations of `default` and `choices` and emits a parameterized instance
    * choice over `select`.
    *
    * The default module and each alternative are elaborated with [[chisel3.Module.evaluate]]; a
    * diagnostic is reported if any alternative's IO is not type-equivalent to the default's IO.
    * The returned IO is bound so that the selected implementation is transparent to the enclosing
    * module, which connects to it exactly as it would to a single module instance.
    *
    * @param select  the property that selects the active implementation
    * @param default the module instantiated when the selection is not one of `choices`
    * @param choices the mapping from choice cases to lazily constructed module generators
    * @tparam T     the IO bundle type shared by every implementation
    * @tparam CaseT the [[ChoiceCase]] type of the selection property
    * @return the IO bundle of the selected implementation, for use in the enclosing module
    */
  def apply[T <: Data, CaseT <: ChoiceCase](
    select:  Property[CaseT],
    default: => FixedIOBaseModule[T],
    choices: Seq[(CaseT, () => FixedIOBaseModule[T])]
  )(implicit sourceInfo: SourceInfo): T = {
    val instDefaultModule = Module.evaluate(default)

    val choiceModules = choices.map { case (choice, module) =>
      val instModule = Module.evaluate(module())
      if (!instModule.io.typeEquivalent(instDefaultModule.io)) {
        Builder.error("Error: choice module IO bundles are not type equivalent")
      }
      (choice, instModule)
    }

    val binding = instDefaultModule.io.cloneTypeFull
    binding.bind(ParamInstanceChoiceBinding(Builder.forcedUserModule, Builder.currentBlock))

    Builder.pushCommand(
      ir.DefParamInstanceChoice(
        sourceInfo,
        binding,
        instDefaultModule,
        select,
        choiceModules.map { case (choice, instModule) =>
          (choice.name, instModule)
        }
      )
    )

    binding
  }
}
