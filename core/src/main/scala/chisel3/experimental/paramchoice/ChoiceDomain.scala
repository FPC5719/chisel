// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.paramchoice

import chisel3._
import chisel3.internal.Builder
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.util.simpleClassName
import scala.collection.mutable
import chisel3.properties.ChoiceTypeProvider

/** Common base for the path-dependent cases of a [[ChoiceDomain]].
  *
  * A `ChoiceCase` identifies a single choice within a domain. It is implemented by members of
  * [[ChoiceDomain.Case]], and is the bound used when a choice value is turned into a `Property`
  * and handed to [[ParamModuleChoice]].
  *
  * @see [[ChoiceDomain]] for how to declare a domain and its cases
  */
trait ChoiceCase {

  /** The name of this case, unique within its [[domain]]. */
  def name: String

  /** The [[ChoiceDomain]] this case belongs to. */
  def domain: ChoiceDomain
}

/** A named set of choices that [[ParamModuleChoice]] can select among at run time.
  *
  * Extend this class (typically as a Scala `object`) to declare a group of mutually exclusive
  * choices. The domain resolves its own name in the circuit's global namespace when it is
  * constructed, so the emitted domain is unique even when several domains share a class name.
  *
  * Each choice is created with `Case(name)`. A `Property` whose type is one of the domain's cases
  * can then drive module selection in [[ParamModuleChoice]]:
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
abstract class ChoiceDomain {

  /** This domain's name, made unique within the circuit's global namespace. */
  private[chisel3] val name: String =
    Builder.globalNamespace.name(simpleClassName(this.getClass()))

  /** A single choice belonging to this domain.
    *
    * Cases are created with `Case(name)` and are unique within the domain. The `->` operator
    * pairs a case with a module generator so that [[ParamModuleChoice]] can build the
    * corresponding implementation.
    *
    * @param name   the name of this case
    * @param domain the domain this case belongs to
    */
  final class Case private[chisel3] (
    val name:   String,
    val domain: ChoiceDomain
  ) extends ChoiceCase {

    /** Pairs this case with a lazily constructed module implementation.
      *
      * `that` is captured by name and only evaluated if [[ParamModuleChoice]] instantiates this
      * case, mirroring the `->` syntax of [[chisel3.choice.Case]].
      *
      * @param that the module to associate with this case
      * @tparam T   the module type produced by `that`
      * @return this case together with a generator that produces `that`
      */
    def ->[T <: BaseModule](that: => T): (Case, () => T) = {
      (this, () => that)
    }
  }

  /** Companion holding this domain's case factory and type provider. */
  object Case {

    /** Provides the implicit [[chisel3.properties.ChoiceTypeProvider]] that lets a
      * `Property[Case]` be typed as a choice of this domain, so a case value can be used as a
      * `Property` literal.
      */
    implicit object ChoiceCaseProvider extends ChoiceTypeProvider[Case](name)

    /** Creates and registers a new case in this domain.
      *
      * @param name the name of the case, unique within the domain
      * @return the newly created case
      */
    def apply(name: String): Case = {
      val res = new Case(name, ChoiceDomain.this)
      cases += res
      res
    }
  }

  /** All cases declared in this domain, in declaration order. */
  private[chisel3] val cases = mutable.ArrayBuffer.empty[Case]

  Builder.choiceDomains += this
}
