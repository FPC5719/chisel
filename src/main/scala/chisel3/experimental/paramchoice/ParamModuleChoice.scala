// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.paramchoice

import chisel3._
import chisel3.internal.Builder
import chisel3.internal.firrtl.ir
import chisel3.internal.binding.ParamInstanceChoiceBinding
import chisel3.properties.Property
import chisel3.experimental.{BaseModule, SourceInfo}

object ParamModuleChoice {
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
