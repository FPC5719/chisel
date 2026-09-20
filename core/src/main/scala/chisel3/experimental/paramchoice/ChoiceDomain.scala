// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.paramchoice

import chisel3._
import chisel3.internal.Builder
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.util.simpleClassName
import scala.collection.mutable
import chisel3.properties.ChoiceTypeProvider

/** Base for the path-dependent ChoiceDomain.Case */
trait ChoiceCase {
  def name:   String
  def domain: ChoiceDomain
}

abstract class ChoiceDomain {
  private[chisel3] val name: String =
    Builder.globalNamespace.name(simpleClassName(this.getClass()))

  final class Case private[chisel3] (
    val name:   String,
    val domain: ChoiceDomain
  ) extends ChoiceCase {
    def ->[T <: BaseModule](that: => T): (Case, () => T) = {
      (this, () => that)
    }
  }

  object Case {
    implicit object ChoiceCaseProvider extends ChoiceTypeProvider[Case](name)

    def apply(name: String): Case = {
      val res = new Case(name, ChoiceDomain.this)
      cases += res
      res
    }
  }

  private[chisel3] val cases = mutable.ArrayBuffer.empty[Case]

  Builder.choiceDomains += this
}
