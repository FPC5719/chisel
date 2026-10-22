// SPDX-License-Identifier: Apache-2.0

package chisel3.util.experimental

import chisel3._
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal.{throwException, Builder}
import scala.reflect.ClassTag

/** Utilities for propagating values and metadata out of a module hierarchy.
  *
  * A hardware exposure requests an additional port on the current module: an output that carries
  * the value out, or an input that carries a value in. A metadata exposure records only a tag and
  * does not affect the module interface. A parent module can collect either kind of exposure by
  * tag. Exposures that are never collected are reported as an error when they reach the top level.
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

  /** Records `tag` on the current module without adding a port to its interface.
    *
    * Metadata exposures propagate through the module hierarchy in the same way as hardware
    * exposures. A parent can retrieve the tags with [[collectMetadata]]. Calling `exposeMetadata`
    * repeatedly with the same tag records multiple exposures. A tag can contain arbitrary Scala
    * data, but that data is not elaborated into hardware.
    *
    * Metadata and hardware exposures have separate collection methods: [[collect]] leaves metadata
    * in place, and `collectMetadata` leaves hardware exposures in place.
    *
    * @param tag user-defined [[chisel3.ExposureTag]] identifying this metadata
    */
  def exposeMetadata(tag: ExposureTag)(implicit sourceInfo: SourceInfo): Unit = {
    Builder.currentModule.get.exposeMetadata(tag)
  }

  /** Removes and returns all exposures of the current module whose tag is of type `T`.
    *
    * Every returned element pairs the tag with the value that was passed to [[expose]]. Exposures
    * with a tag of any other type, including metadata-only exposures, are left in place to be
    * collected later.
    *
    * @tparam T the [[chisel3.ExposureTag]] subtype to collect
    * @return the collected `(tag, value)` pairs, in the order they were exposed
    */
  def collect[T <: ExposureTag: ClassTag](): Seq[(T, Data)] = {
    val buf = Vector.newBuilder[(T, Data)]
    Builder.currentModule.get.exposures.filterInPlace {
      case item: ModuleExposure.Metadata => true
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

  /** Removes and returns all metadata tags of type `T` from the current module.
    *
    * Every returned element is the tag passed to [[exposeMetadata]]. Metadata with a tag of any
    * other type, and all hardware exposures, are left in place. The result contains one element
    * per recorded metadata exposure, in the order the exposures were recorded.
    *
    * @tparam T the [[chisel3.ExposureTag]] subtype to collect
    * @return the collected metadata tags, in the order they were exposed
    */
  def collectMetadata[T <: ExposureTag: ClassTag](): Seq[T] = {
    val buf = Vector.newBuilder[T]
    Builder.currentModule.get.exposures.filterInPlace {
      case item: ModuleExposure.Metadata =>
        item.tag match {
          case tag: T =>
            buf += tag
            false
          case _ => true
        }
      case _ => true
    }
    buf.result()
  }
}
