// SPDX-License-Identifier: Apache-2.0

package chisel3.util.experimental

import chisel3._
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal.{throwException, Builder}
import scala.reflect.ClassTag

/** Utilities for exposing values out of a module through extra ports on its interface.
  *
  * Exposing a value requests an additional port on the current module: an output that carries the
  * value out, or an input that carries a value in. A parent module can then collect the exposure by
  * tag and connect the resulting value as it sees fit. Exposures that are never collected are
  * reported as an error when they reach the top level.
  */
object ExposureUtils {

  /** Requests that `x` be exposed through a new port on the current module.
    *
    * The port is added at the end of the module's body, once the whole body has been elaborated.
    * When `isUpward` is true the port is an [[chisel3.Output]] driven by `x`; otherwise it is an
    * [[chisel3.Input]] that drives `x`. Either way the port is tagged with `tag` so that a parent
    * module can retrieve it with [[collect]].
    *
    * @param tag      user-defined [[chisel3.ExposureTag]] identifying this exposure
    * @param isUpward whether `x` is exposed as an output (true) or as an input (false)
    * @param x        the value to expose
    */
  def expose(tag: ExposureTag, isUpward: Boolean, x: Data)(implicit sourceInfo: SourceInfo): Unit = {
    Builder.currentModule.get.expose(tag, isUpward, x)
  }

  /** Removes and returns all exposures of the current module whose tag is of type `T`.
    *
    * Every returned element pairs the tag with the value that was passed to [[expose]]. Exposures
    * with a tag of any other type are left in place to be collected later.
    *
    * @tparam T the [[chisel3.ExposureTag]] subtype to collect
    * @return the collected `(tag, value)` pairs, in the order they were exposed
    */
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
      // Cached exposures are only created by CacheableModule after the module is built, and are
      // converted to real exposures during propagation, so user code never collects them. This
      // throw is only an internal guard.
      case _: ModuleExposure.Cached =>
        throwException("Should not collect a cached exposure.")
    }
    buf.result()
  }
}
