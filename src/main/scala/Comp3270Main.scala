import scala.io.Source
import java.io._
import Array._


object Comp3270Main extends App {



  val algorithm1 = (X : Array[Int])  =>  {
    var maxSoFar : Int = 0
    for (l <- 0 until X.length) {
      for (u <- l until X.length) {
        var sum = 0
        for (i <- l to u)
          sum = sum + X(i)
        maxSoFar = Math.max(maxSoFar,sum)
      }
    }
    maxSoFar
  }


  val algorithm2 = (X : Array[Int]) => {
    var maxSoFar : Int = 0
    for (l <- 0 until X.length) {
      var sum = 0
      for (u <- l until X.length)  {
        sum = sum + X(u)
        maxSoFar = Math.max(maxSoFar,sum)
      }
    }
    maxSoFar
  }


  val algorithm3 = (X : Array[Int]) => {
    MaxSum(X,0,X.length-1)
  }


  def MaxSum(X : Array[Int], l : Int, u : Int): Int = {
    var result : Int  = 0
    if (l  == u) {result = Math.max(0,X(l))}
    else if (l > u) {result = 0}
    else {
      var M : Int = (l + u) / 2
      var sum : Int = 0
      var maxToLeft : Int = 0
      for (i <- M to l by -1) {
        sum = sum + X(i)
        maxToLeft = Math.max(maxToLeft,sum)
      }

      sum = 0
      var maxToRight : Int = 0
      for (i <- M+1 to u) {
        sum = sum + X(i)
        maxToRight = Math.max(maxToRight,sum)
      }

      var maxCrossing : Int = maxToLeft + maxToRight

      var maxInA : Int = MaxSum(X,l,M)
      var maxInB : Int = MaxSum(X,M+1,u)

      result = Math.max(maxCrossing, Math.max(maxInA,maxInB))
    }
    result
  }



  val algorithm4 = (X : Array[Int]) => {
    var maxSoFar : Int  = 0
    var maxEndingHere : Int = 0

    for (i <- 0 until X.length) {
      maxEndingHere = Math.max(0,maxEndingHere+X(i))
      maxSoFar = Math.max(maxSoFar,maxEndingHere)
    }
    maxSoFar
  }

  var ts = System.currentTimeMillis()

  val writer = new PrintWriter(new File("test.txt" ))

  writer.println("1,2,-4,3,-5,2,0")
  writer.println("1,2,3,4,5,6")
  writer.println("-1,-2,-3,-4,-5,-6")

  writer.close()

  var algEnsemble : Array[(Array[Int]) => Int] = new Array[(Array[Int]) => Int](4)
  algEnsemble(0) = algorithm1
  algEnsemble(1) = algorithm2
  algEnsemble(2) = algorithm3
  algEnsemble(3) = algorithm4

  val fileIter = Source.fromFile("test.txt" ).getLines()

  while (fileIter.hasNext) {
    var s : String = fileIter.next()
    val inp = s.split(",").map(_.trim())
    var a : Array[Int] = new Array[Int](inp.length)
    var c =0
    for (m <- inp) {
      print(m + " ")
      a(c) = m.toInt
      c = c+ 1
    }
    println()
    println("algorithm-1: " + algEnsemble(0)(a))
    println("algorithm-2: " + algEnsemble(1)(a))
    println("algorithm-3: " + algEnsemble(2)(a))
    println("algorithm-4: " + algEnsemble(3)(a))
  }


  var myMatrix : Array[Array[Int]] = new Array[Array[Int]](19)
  val r = scala.util.Random
  var index =0
  for (j <- 10 to 100 by 5) {
    myMatrix(index) = new Array[Int](j)
    for (k <-0 until j) {
      var num = r.nextInt(15)
      var sign = r.nextDouble
      if (sign <= 0.5) myMatrix(index)(k)=num
      else myMatrix(index)(k)= 0-num
    }

    index = index + 1
  }


  var time = Array.ofDim[Long](19,4)


  var inp : Int = 0
  for (inp <- 0 until 19) {
    for (alg <- 0 to 3) {
      for (t <- 0 until 100) {
        var t1 = System.nanoTime()
        algEnsemble(alg)(myMatrix(inp))
        var t2 = System.nanoTime()
        time(inp)(alg) = time(inp)(alg) + (t2 - t1)
      }
      time(inp)(alg) = time(inp)(alg) / 100
    }
  }

  println("Alg1" + "\t" + "Alg2" + "\t" + "Alg3" + "\t" + "Alg4")
  println("----------------------------------------")
  for (inp <- 0 until 19) {
    for (alg <- 0 until 4)
      print(time(inp)(alg) + "\t")
    println()
  }

  var te = System.currentTimeMillis()

  println("program time (in miliseconds) " + (te-ts))


}