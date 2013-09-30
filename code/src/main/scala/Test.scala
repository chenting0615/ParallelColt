object Test extends App {
  Macros.hello

  val fooInt = Foo[Int]
  val fooAny = Foo[Any]

  println(fooInt)
  println(fooAny)
  def foo[X] = Foo[X]
  println(foo[Float])
}
