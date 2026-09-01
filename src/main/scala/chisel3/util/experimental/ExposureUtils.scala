package chisel3.util.experimental

import chisel3._
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal.{throwException, Builder}
import scala.reflect.ClassTag

object ExposureUtils {
  def expose(tag: ExposureTag, x: Data)(implicit sourceInfo: SourceInfo): Unit = {
    Builder.currentModule.get.expose(tag, x)
  }

  def collect[T <: ExposureTag: ClassTag](): Seq[(T, Data)] = {
    val buf = Vector.newBuilder[(T, Data)]
    Builder.currentModule.get.exposures.filterInPlace {
      case item: ModuleExposure.Real =>
        item.tag match {
          case tag: T =>
            buf += ((tag, item.data))
            false
          case _ => true
        }
      case _: ModuleExposure.Cached =>
        throwException("Should not collect a cached exposure.")
    }
    buf.result()
  }
}
