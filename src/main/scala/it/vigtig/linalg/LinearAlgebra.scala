package it.vigtig.linalg

import scala.virtualization.lms.common._
import language.implicitConversions
import language.higherKinds

trait LinearAlgebra extends Base with Dsl {
  // Concepts
  type Vector[T]
  def vector_scale[T: Manifest: Numeric](v: Rep[Vector[T]], s: Rep[T]): Rep[Vector[T]]
	def vector_add[T: Manifest : Numeric](v:Rep[Vector[T]],u:Rep[Vector[T]]):Rep[Vector[T]]
	  
  // Concrete syntax
  implicit class VectorOps[T: Manifest: Numeric](v: Rep[Vector[T]]) {
    def *(s: Rep[T]) = vector_scale(v, s)
    def +(u: Rep[Vector[T]]) = vector_add(v,u)
  }

}

trait Interpreter extends Base {
  override type Rep[+A] = A
  override protected def unit[A: Manifest](a: A) = a
}

trait LinearAlgebraInterpreter extends LinearAlgebra with Interpreter {
  
  override type Vector[T] = Seq[T]
  override def vector_scale[T: Manifest](v: Seq[T], k: T)(implicit num: Numeric[T]) = v map (x => num.times(x, k))
}

trait ScalaGenLinearAlgebra extends ScalaGenBase with DslGen {
  // This code generator works with IR nodes defined by the LinearAlgebraExp trait
  val IR: LinearAlgebraExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case VectorScale(v, k) => {
      emitValDef(sym, quote(v) + ".map(x => x * " + quote(k) + ")")
    }
    case _ => super.emitNode(sym, node)
  }
}

trait LinearAlgebraExpOpt extends LinearAlgebraExp {
  override def vector_scale[T: Manifest: Numeric](v: Exp[Vector[T]], k: Exp[T]) = k match {
    case Const(1.0) => v
    case _ => super.vector_scale(v, k)
  }
}


trait Prog extends LinearAlgebra {
  def f(v: Rep[Vector[Double]]): Rep[Vector[Double]] = v * 12.34d
  def g(v: Rep[Vector[Double]]): Rep[Vector[Double]] = v * 1d
  def h(v: Rep[Vector[Int]]): Rep[Vector[Int]] = v * 2
  def i(v: Rep[Vector[Int]]): Rep[Vector[Int]] = v + v
}

object Main extends App {
/*
  val prog = new Prog with LinearAlgebraInterpreter
  println(prog.f(Seq(1.0, 2.0))) // prints “Seq(12.34, 24.68)”  
*/

  val progIR = new Prog with LinearAlgebraExp with EffectExp with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
  }
  progIR.codegen.emitSource(progIR.h, "H", new java.io.PrintWriter(System.out))

  val prog2 = new Prog with LinearAlgebraExpOpt with EffectExp with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
  }
  val f = prog2.compile(prog2.f)
  val i = prog2.compile(prog2.i)
  println(f(Array(1d,2d,3d))) 
  val x = Array(1,2,3)
  println(i(x).mkString(","))
  prog2.codegen.emitSource(prog2.i,"I",new java.io.PrintWriter(System.out))
}
