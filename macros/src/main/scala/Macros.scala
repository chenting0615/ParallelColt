import language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = c.universe.reify(println("hello world!"))
  def hello = macro impl
}

trait Foo[-A]
class IntFoo() extends Foo[Int]
class AnyFoo() extends Foo[Any]

object Foo {
  def apply[A]: Foo[A] = macro applyImpl[A]

  def applyImpl[A](c: Context)(t: c.WeakTypeTag[A]): c.Expr[Foo[A]] = {
    import c.universe._
    val aTpe    = t.tpe
    val prefix  = if (aTpe =:= typeOf[Int]) "Int" else "Any"
    val clazz   = newTypeName(s"${prefix}Foo")
    c.Expr(q"new $clazz()")
  }
}
//
//object X {
//
//  def box[A] (c: Context)(t: c.WeakTypeTag[A]): c.Expr[String] = {
//    import c.universe._
//    val aTpe    = t.tpe
//    val prefix  = if (aTpe =:= typeOf[Int]) "Integer"
//    else if (aTpe <:< typeOf[AnyVal]) aTpe.typeSymbol.name.decoded
//    else "Object"
//    c.Expr[String](q"java.lang.${prefix}")
//  }
//
//  def apply[A]: Foo[A] = macro box[A]
//
//  def applyImpl[A](c: Context)(t: c.WeakTypeTag[A]): c.Expr[Foo[A]] = {
//    import c.universe._
//    val aTpe    = t.tpe
//    val prefix  = if (aTpe =:= typeOf[Int]) "Int" else "Any"
//    val clazz   = newTypeName(s"${prefix}Foo")
//    c.Expr(q"new $clazz()")
//  }
//}

//object MacroExample {
//  trait Foo
//  trait Bar
//  def newInstance[A <: Foo]: A with Bar = macro newInstance_impl[A]
//
//  def newInstance_impl[A <: Foo](c: Context)(A: c.WeakTypeTag[A]) = {
//    import c.universe._
//
//    c.Expr[A with Bar](q"new $A with Bar")
//  }
//}
