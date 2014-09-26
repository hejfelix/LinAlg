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
	def vector_sum[T: Manifest : Numeric](v:Rep[Vector[T]]):Rep[T]  
  // Concrete syntax
  implicit class VectorOps[T: Manifest: Numeric](v: Rep[Vector[T]]) {
    def *(s: Rep[T]) = vector_scale(v, s)
    def +(u: Rep[Vector[T]]) = vector_add(v,u)
    def sum = vector_sum(v)
  }

}

trait Doubleerpreter extends Base {
  override type Rep[+A] = A
  override protected def unit[A: Manifest](a: A) = a
}

trait LinearAlgebraDoubleerpreter extends LinearAlgebra with Doubleerpreter {
  
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
  def f(v: Rep[Vector[Float]]): Rep[Vector[Float]] = v * 12.34f
  def g(v: Rep[Vector[Float]]): Rep[Vector[Float]] = v * 1f
  def h(v: Rep[Vector[Double]]): Rep[Vector[Double]] = v * 2d
  def i(v: Rep[Vector[Double]]): Rep[Vector[Double]] = v + v
  def j(v:Rep[Vector[Double]]):Rep[Double] = v.sum
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
    def f(v: Rep[Vector[Float]]): Rep[Vector[Float]] 
    def i(v: Rep[Vector[Double]]): Rep[Vector[Double]]
    def j(v:Rep[Vector[Double]]):Rep[Double]
    /*
    codegen.withStream(new PrintWriter(System.out)) {
        val b1 = reifyEffects(f(Array(1d,2d,3d)))
        vgeal b2 = xform.run(b1)
        codegen.emitBlock(b2)
    }*/
}

object Main extends App {

  def time(b: =>Unit) = {
    val s = System.currentTimeMillis
    b
    System.currentTimeMillis-s
  }

  import math.Numeric.Implicits.infixNumericOps//High level stuff
  def naiveF[T:Numeric](v:Array[T]):T = v.sum
  def naiveLoop(v:Array[Double]):Double = {
    var sum = 0d
    var i = 0
    val L = v.length
    while(i<L){
       sum += v(i)
       i += 1
    }
    sum
  }  
  def naiveLoopT[T](v:Array[T])(implicit num:Numeric[T]):T = {
    var sum = num.zero
    var i = 0
    val L = v.length
    while(i<L){
       sum = num.plus(sum,v(i))
       i += 1
    }
    sum
  }

  //TODO: handunroll twice
  def handUnrolled2(v:Array[Double]):Double = {
    var sum = 0d
    var i = 0
    val L = v.length
    val LIMIT = (L/2)*2
    while(i<LIMIT){
       sum += v(i)+v(i+1)
       i += 2
    }
    if(L%2==1)
      sum += v(L-1)
    sum
  }  

  //TODO: handunroll twice
  def handUnrolled4(v:Array[Double]):Double = {
    var sum = 0d
    var i = 0
    val L = v.length
    val UNROLL = 4
    val LIMIT = (L/UNROLL)*UNROLL
    while(i<LIMIT){
       sum += v(i)+v(i+1)+v(i+2)+v(i+3)
       i += UNROLL
    }
    if(L%4!=0){
      var i = 0
      while(i<L%4){
        sum += v(LIMIT+i)
        i+=1
      }
    }
    sum
  }  

  def benchmark[T](f: Array[Double] => T) = {
    val bench = ((0 until rounds) map { _ => time(f(array)) }).sum/rounds.toFloat
    println(s"Average time taken = $bench milliseconds")
    bench
  }

  val N = 79190000
  val rounds = 100
  val rnd = new util.Random(System.currentTimeMillis)
  val array = Array.fill(N) { rnd.nextDouble }

  implicit class StructuredArray[T](a:Array[T]) {
     def ===(b:Array[T]) = (a,b).zipped.forall(_==_)
  }


  println("Benchmark naive f with numeric typeclass and FP...")
  val unit = benchmark(naiveF[Double])

  for(N<-List(1,2,4,8,16).reverse){
    println(s"Unrolling $N times...")
    val p2 = new Prog with Impl { override val UNROLL = N } //Unroll loop N times
    val cf = p2.compile(p2.j)
    //p2.codegen.emitSource(p2.j,"J",new java.io.PrintWriter(System.out))
    /*if(array.sum != cf(array)){
      println(s"""naive result: ${array.sum}""")
      println(s"""staged result: ${cf(array)}""")
    }*/
    println(s"  with speedup ${unit/benchmark(cf)}")
    println()
  }
 
  println("Benchmark naive f with loop...")
  println(s"  with speedup ${unit/benchmark(naiveLoop)}")
  println()
  
  println("Benchmark naive f with 2 times hand-unrolled loop...")
  println(s"  with speedup ${unit/benchmark(handUnrolled2)}")
  println()   
 
  println("Benchmark naive f with 4 times hand-unrolled loop...")
  println(s"  with speedup ${unit/benchmark(handUnrolled4)}")
  println()  
  
  println("Benchmark naive f with loop and typeclass...")
  println(s"  with speedup ${unit/benchmark(naiveLoopT[Double])}")
  println()




}
