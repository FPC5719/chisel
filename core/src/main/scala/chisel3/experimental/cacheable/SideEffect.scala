package chisel3.experimental.cacheable

sealed trait SideEffect[T] {
  def map[U](f: T => U): SideEffect[U] = MapEffect(this, f)

  def flatMap[U](f: T => SideEffect[U]): SideEffect[U] = FlatMapEffect(this, f)

  private[cacheable] def evaluate(): T
}

object SideEffect {
  def pure[T](value: => T): SideEffect[T] = PureEffect(() => value)
}

private final case class PureEffect[T](
  private val thunk: () => T
) extends SideEffect[T] {
  private[cacheable] def evaluate(): T = thunk()
}

private final case class MapEffect[S, T](
  private val source: SideEffect[S],
  private val f:      S => T
) extends SideEffect[T] {
  private[cacheable] def evaluate(): T = f(source.evaluate())
}

private final case class FlatMapEffect[S, T](
  private val source: SideEffect[S],
  private val f:      S => SideEffect[T]
) extends SideEffect[T] {
  private[cacheable] def evaluate(): T = f(source.evaluate()).evaluate()
}
