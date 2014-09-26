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
  case class VectorSum[T:Manifest:Numeric](v:Exp[Vector[T]]) extends Def[T]{
    val mT = manifest[T]
    val nT = implicitly[Numeric[T]]
  }


  override def vector_scale[T: Manifest: Numeric](v: Exp[Vector[T]], s: Exp[T]) = toAtom(VectorScale(v, s))
	override def vector_add[T: Manifest : Numeric](v:Rep[Vector[T]],u:Rep[Vector[T]]) = toAtom(VectorAdd(v,u))
	override def vector_sum[T: Manifest : Numeric](v:Rep[Vector[T]]) = toAtom(VectorSum(v))

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

trait LinAlg2Loops extends LinAlgFWTransform with LinearAlgebraExp with LinearAlgebraExpOpt with PrimitiveOps{

  //implicit def any2rep[T: Manifest](t: T) = unit(t)
  /*
    we enrich Vectors (now, Arrays) with foreach and zipWith.
    This is virtual code because we work on Rep[T]s and
    works because we extend BaseExp+DslExp which in turn
    contain AST representations of while loops, array-constructors, 
    etc...
  */
  val UNROLL = 8
  implicit class enrichArray[T:Manifest](a: Rep[Array[T]])(implicit val num:Numeric[T]) {
    def foreach(f: Rep[T] => Rep[Unit]):Rep[Unit] = {
      var i=0; while(i<a.length) { f(a(i)); i+= 1 } 
    }
    /*def zipWith(b:Rep[Array[T]])(f: (Rep[T],Rep[T]) => Rep[T]) = {
      val res = NewArray[T](a.length min b.length)
      var i = 0;while(i<res.length) { res(i) = f(a(i),b(i)); i+=1 }
      res
    }*/
    def zipWith(b:Rep[Array[T]])(f: (Rep[T],Rep[T]) => Rep[T]) = {
      val L = a.length min b.length
      val res = NewArray[T](L)
      var i = 0
      val LIMIT = L-UNROLL
      while(i<=LIMIT) { //Unroll based on 'UNROLL' parameter
         ((0 until UNROLL):Range).foreach(x => {res(i+x) = f(a(i+x),b(i+x))}) //Range is static, foreach call can be unrolled
         i+=UNROLL
      }
      //For non-powers of 2. This range is Rep[Range] and will not be unrolled (still inlined though!)
      ((0 until L % UNROLL)).foreach(x => {val xx= (L/UNROLL)*UNROLL+x; res(xx) = f(a(xx),b(xx))} )
      res
    }
    
    def foldLeft(zero:Rep[T])(f:(Rep[T],Rep[T])=>Rep[T]) = {
      val L = a.length
      var i = 0
      val LIMIT = L-UNROLL
      var acc = zero
      while(i<=LIMIT) { //Unroll based on 'UNROLL' parameter
         var temp = num.zero
         ((0 until UNROLL):Range).foreach(x => {temp = f(temp,a(i+x))}) //Range is static, foreach call can be unrolled
         acc = f(acc,temp)
         i+=UNROLL
      }
      //For non-powers of 2. This range is Rep[Range] and will not be unrolled (still inlined though!)
      ((0 until L % UNROLL)).foreach(x => {acc = f(acc,a((L/UNROLL)*UNROLL+x))} )
      acc
    }

    def sum:Rep[T] = foldLeft(num.zero)(_+_)

    def map[U:Manifest](f:(Rep[T]) => Rep[U]) = {
      val L = a.length
      val res = NewArray[U](L)
      var i = 0
      val LIMIT = L-UNROLL
      while(i<=LIMIT) { //Unroll based on 'UNROLL' parameter
         ((0 until UNROLL):Range).foreach(x => {res(i+x) = f(a(i+x))}) //Range is static, foreach call can be unrolled
         i+=UNROLL
      }
      //For non-powers of 2. This range is Rep[Range] and will not be unrolled (still inlined though!)
      ((0 until L % UNROLL)).foreach(x => {res((L/UNROLL)*UNROLL+x) = f(a((L/UNROLL)*UNROLL+x))} )
      res
    }
    def *(u:Rep[T]) = map(_*u) 
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case vec@VectorScale(v,scalar) => vector_scale(f(v),f(scalar))(vec.mT,vec.nT)
    case vec@VectorAdd(a,b) => vector_add(f(a),f(b))(vec.mT,vec.nT)
    case vec@VectorSum(v) => vector_sum(f(v))(vec.mT,vec.nT)
    case afs@ArrayFromSeq(xs) => array_obj_fromseq(f(xs))(afs.m)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case vec@VectorScale(v,scalar) => vector_scale(f(v),f(scalar))(vec.mT,vec.nT)
    case vec@VectorAdd(a,b) => vector_add(f(a),f(b))(vec.mT,vec.nT)
    case vec@VectorSum(v) => vector_sum(f(v))(vec.mT,vec.nT)
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??

  def vscale_loopform[T:Manifest:Numeric](a:Rep[Vector[T]],s:Rep[T]) = a * s
  def vadd_loopform[T:Manifest:Numeric](a:Rep[Vector[T]],b:Rep[Vector[T]]) = a.zipWith(b)(_+_)
  def vsum_loopform[T:Manifest:Numeric](a:Rep[Vector[T]]) = a.sum

  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
    case vec@VectorScale(v,scalar) => s.atPhase(xform) { vscale_loopform(xform(v),xform(scalar))(vec.mT,vec.nT).asInstanceOf[Exp[A]] }
    case vec@VectorAdd(a,b) => s.atPhase(xform) { vadd_loopform(xform(a),xform(b))(vec.mT,vec.nT).asInstanceOf[Exp[A]] }
    case vec@VectorSum(a) => s.atPhase(xform) { vsum_loopform(xform(a))(vec.mT,vec.nT).asInstanceOf[Exp[A]] }
    case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]

  val xform = new MyWorklistTransformer

}



