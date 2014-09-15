package it.vigtig.linalg

import scala.virtualization.lms.common._
import language.implicitConversions
import language.higherKinds
import java.io.{PrintWriter,StringWriter,FileOutputStream}

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

trait Impl extends EffectExp with CompileScala with LinAlg2Loops { 
  self =>
    /*
      Inject the worklisttransformer to the code generator
    */
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra  { 
      val IR: self.type = self 
      override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = 
        super.emitSource(args,xform.run(body),className,out)
    }    
    def f(v: Rep[Vector[Double]]): Rep[Vector[Double]] 

    /*
    codegen.withStream(new PrintWriter(System.out)) {
        val b1 = reifyEffects(f(Array(1d,2d,3d)))
        val b2 = xform.run(b1)
        codegen.emitBlock(b2)
    }*/
}

object Main extends App {

  def time(b: =>Unit) = {
    val s = System.currentTimeMillis
    b
    System.currentTimeMillis-s
  }

  def naiveF[T:Manifest](scalar:T)(v:Array[T])(implicit nt:Numeric[T]):Array[T] = v.map( nt.times(_ ,scalar) ).toArray

  def benchmark(f: Array[Double] => Array[Double]) {
    val bench = ((0 until rounds) map { _ => time(f(array)) }).sum/rounds.toDouble
    println(s"Average time taken = $bench milliseconds")
  }

  val N = 8000000
  val rounds = 400
  val rnd = new util.Random(System.currentTimeMillis)
  val array = Array.fill(N) { rnd.nextDouble }

  
  for(N<-List(1,2,4,8)){
    println(s"Unrolling $N times...")
    val p2 = new Prog with Impl { override val UNROLL = N } //Unroll loop N times
    val cf:Array[Double] => Array[Double] = p2.compile(p2.f)
    //p2.codegen.emitSource(p2.f,"F",new java.io.PrintWriter(System.out))
    benchmark(cf)
  }

  
  println("Benchmark naive f with numeric typeclass and FP...")
  benchmark(naiveF(12.34d))


}
