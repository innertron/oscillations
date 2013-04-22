package edu.uccs

import breeze.plot._
import breeze.stats.distributions._
import scala.annotation.tailrec
import scala.util.Random

object RatDataSim {

  // class for object that contains parameters for makeDist method
  case class DistParams (
    n: Int,  // number of points to create
    slope: Double,  // slope of distribution
    xCenter: Double,  // x axis center
    xSpread: Double,  // spread in x direction
    yCenter: Double,  // y axis center
    ySpread: Double,  // spread in y direction
    label: String)

  case class RatParams (
    norm: DistParams,
    arti: DistParams,
    seiz: DistParams,
    seiz2: DistParams)

  // take a value and shift it randomly up or down by up to 10%
  def randShift(inval: Double) = {
    val rand = new Random()
    val shiftRange = 0.20 * inval
    val shift = (rand.nextDouble() * shiftRange) - (shiftRange / 2)
    inval + shift
  }

  // create ratParams objects for normal, artifact, seizure, seizure2
  // parameters for a single rat, based on random alterations of
  // standard parameters
  def makeRatParams() = {
    // use randShift to randomly alter slope, center and spread slightly
    RatParams(
    DistParams(
      100,
      randShift(0.20),
      randShift(2.5),
      randShift(1.0),
      randShift(0.0),
      randShift(0.18),
      "normal"),
    DistParams(
      100,
      randShift(0.30),
      randShift(8.0),
      randShift(1.5),
      randShift(0.33),
      randShift(0.25),
      "artifact"),
    DistParams(
      20,
      randShift(1.25),
      randShift(4.0),
      randShift(2.0),
      randShift(1.0),
      randShift(1.0),
      "seizure"),
    DistParams(
      15,
      randShift(0.66),
      randShift(4.0),
      randShift(2.0),
      randShift(1.0),
      randShift(1.0),
      "seizure2"))
  }

  // make a vector containing random sequence of 2 to 6 DistParams objects
  // using an rParams object containing parameters representing a single rat
  def makeRandParamSeq(rParams: RatParams) = {
    val paramList = Vector(rParams.norm, rParams.arti, rParams.seiz, rParams.seiz)
    val rand = new Random()
    val nParams = rand.nextInt(4) + 3 // get random number between 2 and 6
    val outVec = Vector()
    // recursive loop to build vector of nParams random DistParams objects
    @tailrec
    def loop(n: Int, acc: Vector[DistParams]): Vector[DistParams] = {
      if (n <= 0) acc
      else {
        val nextParam = paramList(rand.nextInt(paramList.length))
        // if nextParam has label "seizure", add seizure2 param to vector
        if ((nextParam.label == "seizure") && 
            (rand.nextInt(100) < 33) && 
            (n >= 2))
          loop(n - 2, acc :+ nextParam :+ rParams.seiz2)
        else loop(n - 1, acc :+ nextParam)
      }
    }
    loop(nParams, outVec)
  }

  // make a vector containing a set sequence of DistParams objects
  // using an rParams object with parameters representing a single rat
  def makeStdParamSeq(rParams: RatParams) = {
    Vector(rParams.arti, rParams.norm, rParams.seiz, rParams.seiz2)
  }

  // make a distribution: a vector of (x, y, label) tuples
  // conststructed using parameters contained in a DistParams object
  def makeDist(dp: DistParams) = {
    @tailrec // confirm recursive call below is in tail position
    // make a single (x, y, label) tuple
    def makeXY: (Double, Double, String) = {
      val x = Gaussian(dp.xCenter, dp.xSpread).sample()
      val y = (x * dp.slope) + Gaussian(dp.yCenter, dp.ySpread).sample()
      val label = dp.label
      if (x >= 0 && y >= 0) (x, y, label) // return if x and y are both >= 0
      else makeXY // else try again via recursive call
    }
    // fill vector of coordinates by calling makeXY dp.n times
    Vector.fill(dp.n)(makeXY)
  }

  // re-sequence a vector of (x, y, label) tuples in ascending order of x + y
  // then mutate the sorted vector
  def sequenceDist(dist: Vector[(Double, Double, String)]) = {
    val nmutations = (dist.length * 0.20).toInt // number of mutations to make
    val rand = new Random()
    // sort the dist in ascending x + y
    val sortedDist = dist.sortWith{
      case ((x1, y1, l1), (x2, y2, l2)) => (x1 + y1) < (x2 + y2)
    }
    // recursive loop to mutate the sorted dist n times: cut the distribution
    // into 3 segments at random points, reverse the middle segment, reassemble 
    @tailrec
    def loop(n: Int, newdist: Vector[(Double, Double, String)]):
    Vector[(Double, Double, String)] = {
      if (n <= 0) newdist // if n mutations return the current dist
      else { // else cut dist into 3 segments and flip the middle one
        val firstn = rand.nextInt(newdist.length - 1)
        val seg1 = newdist.take(firstn)
        val segtemp = newdist.drop(firstn)
        val nextn = 1 + rand.nextInt(segtemp.length - 1)
        val seg2 = segtemp.take(nextn)
        val seg3 = segtemp.drop(nextn)
        loop (n - 1, seg1 ++ seg2.reverse ++ seg3)
      }
    }
    loop(nmutations, sortedDist)
  }
 
  // make a master distribution by iterating through
  // a random sequence of DistParams objects, calling makeDist
  // using each, sort each via sortDist and concatenate
  def makeMasterDist(paramSeq: Vector[DistParams]) = {
    println(paramSeq.length + " Frames:")
    paramSeq.foreach(println)
    val outDist = paramSeq
      .map(dParams => sequenceDist(makeDist(dParams)))
      .flatten
    println("Points: " + outDist.length)
    outDist
  }

  // print a distribution
  def printDist(dist: Vector[(Double, Double, String)]) {
    for ((x, y, label) <- dist) println(x + ", " + y + ":\t" + label)
  }

  // plot a distribution
  def plotDist(dist: Vector[(Double, Double, String)], 
               pTitle: String = "Simulated Rat Data") {
    val (x, y, label) = dist.unzip3  // extract x,y coord seqs; ignore labels
    val f = Figure()
    val p = f.subplot(0)
    def pSize = (i: Int) => 0.05 // size function required by breeze scatter
    p += plot(x, y)
    p += scatter(x, y, pSize) // add scatter plot
    p.xlabel = "x"
    p.ylabel = "y"
    p.title = pTitle
  }
    
  // write a distribution to a csv
  def saveDist(dist: Vector[(Double, Double, String)], outfile: String) {
    val writer = new java.io.FileWriter(outfile)
    writer.write("\"x\",\"y\",\"label\"\n")
    for ((x, y, label) <- dist)
      writer.write(x.toString + "," + y.toString + ",\"" + label + "\"\n")
    writer.close()
  }
 
  // main function
  def main(args: Array[String]) {
    val nRats = 2 // number of rats to simulate
    val nDists = 2 // number of distributions to make per rat
    // loop to make nRats rats
    for (i <- 1 to nRats) {
      // get parameters for this rat
      val rParams = makeRatParams()
      // loop to make nDists distributions
      for (j <- 1 to nDists) {
        // make a distribution pased on the parmeter sequence
        //val dist = makeMasterDist(makeRandParamSeq(rParams))
        val dist = makeMasterDist(makeStdParamSeq(rParams))
        val distInfo = "rat" + i + "-dist" + j
        //printDist(dist)
        plotDist(dist, distInfo)
        saveDist(dist, distInfo + ".csv") 
      }
    }
  }
}
