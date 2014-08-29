package it.vigtig.linalg

import scala.virtualization.lms.common._
import language.implicitConversions
import language.higherKinds
import scala.reflect.SourceContext
import scala.language.reflectiveCalls

trait LinearAlgebraExp extends LinearAlgebra with BaseExp with DslExp with LinAlgFWTransform {

  // Reification of the concept of scaling a vector `v` by a factor `k`
  case class VectorScale[T: Manifest: Numeric](v: Exp[Vector[T]], s: Exp[T]) extends Def[Vector[T]] {
    val mT = manifest[T]
    val nT = implicitly[Numeric[T]]
  }
  case class VectorAdd[T:Manifest:Numeric](v:Exp[Vector[T]],u:Exp[Vector[T]]) extends Def[Vector[T]]{
    val mT = manifest[T]
    val nT = implicitly[Numeric[T]]
  }

  override def vector_scale[T: Manifest: Numeric](v: Exp[Vector[T]], s: Exp[T]) = toAtom(VectorScale(v, s))
	//override def vector_add[T: Manifest : Numeric](v:Rep[Vector[T]],u:Rep[Vector[T]]) = toAtom(VectorAdd(v,u))

  override type Vector[T] = Array[T]

}

trait LinAlgFWTransform extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>
  
  class MyWorklistTransformer extends WorklistTransformer { val IR: self.type = self }
  
  // ---------- Exp api
  
  implicit def toAfter[A:Manifest](x: Def[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) = new { def atPhase(t: MyWorklistTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t. 
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?
  
  def transformAtPhase[A](x: Exp[A])(t: MyWorklistTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }
    
  
  def onCreate[A:Manifest](s: Sym[A], d: Def[A]): Exp[A] = s

  // ----------
  
  override def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    onCreate(s,d)(s.tp)
    super.createDefinition(s,d)
  }

}

trait LinAlg2Loops extends LinAlgFWTransform with LinearAlgebraExp {

  implicit def any2rep[T: Manifest](t: T) = unit(t)
  /*
    we enrich Vectors (now, Arrays) with foreach and zipWith.
    This is virtual code because we work on Rep[T]s and
    works because we extend BaseExp+DslExp which in turn
    contain AST representations of while loops, array-constructors, 
    etc...
  */
  implicit class enrichArray[T:Manifest:Numeric](a: Rep[Array[T]]) {
    def foreach(f: Rep[T] => Rep[Unit]):Rep[Unit] = {
      var i=0; while(i<a.length) { f(a(i)); i+= 1 } 
    }
    def zipWith(b:Rep[Array[T]])(f: (Rep[T],Rep[T]) => Rep[T]) = {
      val res = NewArray[T](a.length min b.length)
      var i = 0;while(i<res.length) { res(i) = f(a(i),b(i)); i+=1 }
      res
    }
    def map[U:Manifest](f:(Rep[T]) => Rep[U]) = {
      val res = NewArray[U](a.length)
      var i = 0;while(i<res.length) { res(i) = f(a(i)); i+=1}
      res
    }
    def *(u:Rep[T]) = {
      val res = NewArray[T](a.length)
      var i = 0;while(i<res.length) { res(i) = a(i)*u; i+=1}
      res
    } 
  }

  def vscale_loopform[T:Manifest:Numeric](a:Rep[Vector[T]],s:Rep[T]) = a * s

  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
    case v@VectorScale(v,scalar) => s.atPhase(xform) { vscale_loopform(xform(v),xform(scalar))(v.mT,v.nT).asInstanceOf[Exp[A]] }
    case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]

  val xform = new MyWorklistTransformer

}



