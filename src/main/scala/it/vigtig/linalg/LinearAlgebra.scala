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

trait Impl extends LinAlg2Loops with EffectExp with CompileScala { 
  self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra  { val IR: self.type = self }    
    def f(v: Rep[Vector[Double]]): Rep[Vector[Double]] 
    
    codegen.withStream(new PrintWriter(System.out)) {
        val b1 = reifyEffects(f(Array(1d,2d,3d)))
        codegen.emitBlock(b1)
        codegen.stream.flush
        val b2 = xform.run(b1)
        codegen.emitBlock(b2)
    }
}

object Main extends App {

  def time(b: =>Unit) = {
    val s = System.currentTimeMillis
    b
    System.currentTimeMillis-s
  }

  val progIR = new Prog with LinearAlgebraExp with EffectExp with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
  }
  progIR.codegen.emitSource(progIR.h, "H", new java.io.PrintWriter(System.out))

  val p2 = new Prog with Impl 
  val cf:Array[Double] => Array[Double] = p2.compile(p2.f)
  
  val N = 100000
  val rounds = 200
  val rnd = new util.Random(System.currentTimeMillis)
  val array = Array.fill(N) { rnd.nextDouble }

  def naiveF(v:Array[Double]):Array[Double] = v map (_*12.34d)

  def benchmark(f: Array[Double] => Array[Double]) {
    val bench = time {
      for(i<-0 until rounds){
        f(array)
      }
    }
    println(s"Time taken = $bench")
  }
  

  benchmark(cf)
  benchmark(naiveF)

  /*val f = prog2.compile(prog2.f)
  println(f(Array(1d,2d,3d)).mkString(",")) 
  val x = Array(1,2,3)
  prog2.codegen.emitSource(prog2.f,"F",new java.io.PrintWriter(System.out))
  val b = reifyEffects(prog2.f(Array(1,2,3)))*/
/*
  prog2.codegen.withStream(new PrintWriter(System.out)){

    println("### first")
    prog2.codegen.emitBlock(prog2.f(Array(1d,2d,3d)))

  }
*/
}
