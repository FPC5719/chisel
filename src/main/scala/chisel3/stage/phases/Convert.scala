// SPDX-License-Identifier: Apache-2.0

package chisel3.stage.phases

import chisel3.ElaboratedCircuit
import chisel3.stage.ChiselCircuitAnnotation
import firrtl.{annoSeqToSeq, seqToAnnoSeq, AnnotationSeq}
import firrtl.options.{Dependency, Phase}

/** This prepares a [[ChiselCircuitAnnotation]] for compilation with FIRRTL by extracting annotations
  * created during elaboration.
  *
  * Legacy FIRRTL IR conversion is intentionally not performed. The converted circuit was not used
  * by the CIRCT pipeline and the converter no longer supports all Chisel IR constructs.
  */
@deprecated("Converting to FIRRTL IR is deprecated, use ElaboratedCircuit instead.", "Chisel 7.11.0")
class Convert extends Phase {

  override def prerequisites = Seq(Dependency[Elaborate])
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Seq.empty
  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations.flatMap {
    case a: ChiselCircuitAnnotation =>
      Some(a) ++
        /* Extract all Chisel Annotations for downstream FIRRTL consumers. */
        a.elaboratedCircuit.annotations
    case a => Some(a)
  }

}

object Convert {

  /** Explicitly convert an elaborated circuit to the deprecated FIRRTL IR. */
  @deprecated("Use ElaboratedCircuit.serialize instead.", "Chisel 7.14.0")
  def legacyCircuit(elaboratedCircuit: ElaboratedCircuit): firrtl.ir.Circuit =
    chisel3.internal.firrtl.Converter.convert(elaboratedCircuit._circuit)
}
