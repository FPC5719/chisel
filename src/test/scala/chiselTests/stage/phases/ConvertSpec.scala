// SPDX-License-Identifier: Apache-2.0

package chiselTests.stage.phases

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.stage.phases.{Convert, Elaborate}
import circt.stage.ChiselStage

import firrtl.{annoSeqToSeq, seqToAnnoSeq, AnnotationSeq}
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.Phase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class ConvertSpecFirrtlAnnotation(name: String) extends NoTargetAnnotation

class ConvertSpecFoo extends RawModule {
  override val desiredName: String = "foo"

  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))

  chisel3.experimental.annotate()(Seq(ConvertSpecFirrtlAnnotation("bar")))
}

class ConvertSpec extends AnyFlatSpec with Matchers {

  class Fixture { val phase: Phase = new Convert }

  behavior.of(classOf[Convert].toString)

  it should "extract annotations without converting the Chisel Circuit" in new Fixture {
    val annos: AnnotationSeq = Seq(ChiselGeneratorAnnotation(() => new ConvertSpecFoo))

    val annosx = Seq(new Elaborate, phase)
      .foldLeft(annos)((a, p) => p.transform(a))

    info("legacy FIRRTL circuit is not generated")
    annosx.exists(_.isInstanceOf[firrtl.stage.FirrtlCircuitAnnotation]) should be(false)

    info("FIRRTL annotations generated")
    annosx.collect { case a: ConvertSpecFirrtlAnnotation => a.name }.toSeq should be(Seq("bar"))

  }

  it should "extract annotations in the standard CHIRRTL pipeline" in {
    ChiselStage.emitCHIRRTL(new ConvertSpecFoo) should include("ConvertSpecFirrtlAnnotation")
  }

}
