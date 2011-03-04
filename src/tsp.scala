
// Ant.scala
// Eric Rollins 2008

/*
   Written for Scala   http://www.scala-lang.org/

   This program generates a random array of distances between cities, then uses
   Ant Colony Optimization to find a short path traversing all the cities -- 
   the Travelling Salesman Problem.

   In this version of Ant Colony Optimization each ant starts in a random city.
   Paths are randomly chosed with probability inversely proportional to to the
   distance to the next city.  At the end of its travel the ant updates the 
   pheromone matrix with its path if this path is the shortest one yet found.
   The probability of later ants taking a path is increased by the pheromone 
   value on that path.  Pheromone values evaporate (decrease) over time.

   In this impementation weights between cities actually represent 
   (maxDistance - dist), so we are trying to maximize the score.

   Usage: ant [serial]
     By default runs 4-way parallel.  Serial executes 4 times sequentially.

   Compiled-in parameters
     seed         seed for random number generator (1,2,3...).
                  This seed controls the city distance array.  Remote 
                  executions have their seed values fixed (1,2) so each will 
                  produce a different result.
     boost        pheromone boost for best path.  5 appears good.  
                  0 disables pheromones, providing random search.
     iterations   number of ants to be run.
     cities       number of cities.
*/

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random

case class Result(path: List[Int], pathLength: Double)

object Ant 
{
  def main(args: Array[String]) {
    val startTime = System.currentTimeMillis();

    val isSerial = 
      if((args.length > 0) && (args(0).equals("serial"))) {
        true
      } else {
        false
      }

    val numAnts = 4
    val ants = new Array[AntActor](numAnts)

    val resultGatherer = 
      if(isSerial) {
        null
      } else {
        new ResultGatherer(startTime, ants.length)
      }

    var i: Int = 0

    while (i < ants.length) {
      ants(i) = new AntActor(i, resultGatherer)
      i += 1
    }

    if (isSerial) {
      println("running serial")

      i = 0

      while (i < ants.length) {
        ants(i).act
        i += 1
      }

      val stopTime = System.currentTimeMillis();
      print("serial runtime in millis = ")
      println(stopTime - startTime)
    } else {
      println("running parallel")
      resultGatherer.start

      i = 0

      while (i < ants.length) {
        ants(i).start
        i += 1
      }

      // resultGatherer will block on completion, and print times
    }
  }
}

class ResultGatherer(startTime: Long, antCount: Int) extends Actor {
  def act() {
    var msgCount = 0

    while (msgCount < antCount) {
      receive {
        case Result(path, pathLength) =>
          println("==Gathered==")
          print(path)
          print(" : ")
          println(pathLength)
          msgCount += 1
      }
    }       

    val stopTime = System.currentTimeMillis();
    print("parallel runtime in millis = ")
    println(stopTime - startTime)
  } 
}

class AntActor(seed: Int, resultGatherer: ResultGatherer) extends Actor { 
  type Matrix = Array[Array[Double]]
  type Path = List[Int]
  type CitySet = scala.collection.mutable.HashSet[Int]

  def randomMatrix(n: Int, upperBound: Int, seed: Int): Matrix = {
    var m = new Matrix(n, n)
    var gen = new Random(seed)
    var r = 0

    while (r < m.length) {
      var c = 0

      while (c < m(r).length) {
        m(r)(c) = gen.nextDouble * upperBound
        c = c + 1
      }

      r = r + 1
    }

    m
  }

  def printMatrix(m: Matrix) {
    var r = 0

    while (r < m.length) {
      var c = 0

      while (c < m(r).length) {
        print(r)
        print(",")
        print(c)
        print(" : ")
        println(m(r)(c))
        c = c + 1
      }

      r = r + 1
    }
  }

  def wrappedPath(path: Path): Path = {
    path.tail :+ path.head
  }

  def pathLength(cities: Matrix, path: Path): Double = {
    val pairs = path.zip(wrappedPath(path))
    pairs.map{(x) => cities(x._1)(x._2)}.reduceLeft{(x, y) => x + y}
  }


  // Boosts pheromones for cities on path.
  def updatePher(pher: Matrix, path: Path, boost: Int) {
    val pairs = path.zip(wrappedPath(path))
    pairs.foreach{(x) => pher(x._1)(x._2) = pher(x._1)(x._2) + boost}
  }

  def evaporatePher(pher: Matrix, maxIter: Int, boost: Int) {
    var decr: Double = boost / maxIter.toDouble
    var r = 0

    while (r < pher.length) {
      var c = 0

      while (c < pher(r).length) {
        pher(r)(c) = 
          if (pher(r)(c) > decr)
            pher(r)(c) - decr
          else
            0.0
        c = c + 1
      }

      r = r + 1
    }
  }

  // Sum weights for all paths to cities adjacent to current.
  def doSumWeight(city: Int, 
                  cities: Matrix,
                  pher: Matrix, 
                  used: CitySet,
                  current: Int,
                  runningTotal: Double): Double = {
    if (city >= cities.length) {
      runningTotal
    } else {
      val incr =
        if (used.contains(city))
          0.0
        else
          cities(current)(city) * (1.0 + pher(current)(city))
      doSumWeight((city+1), cities, pher, used, current, (runningTotal+incr))
    }
  }

  // Returns city at soughtTotal.
  def findSumWeight(city: Int,
                    nextCity: Int,
                    cities: Matrix,
                    pher: Matrix, 
                    used: CitySet,
                    current: Int,
                    soughtTotal: Double,
                    runningTotal: Double): Int = {
    if ((city >= cities.length) ||
        ((!used.contains(city)) && (runningTotal >= soughtTotal))) {
      nextCity
    } else {
      var (incr, nextNextCity) =
        if (used.contains(city))
          (0.0, nextCity)
        else
          ((cities(current)(city) * (1.0 + pher(current)(city))), city)
      findSumWeight((city+1), nextNextCity, cities, pher, used, current, 
                    soughtTotal, (runningTotal+incr))
    }
  }

  def genPathRecurse(cities: Matrix,
                     pher: Matrix,
                     used: CitySet,
                     path: Path,
                     current: Int,
                     rGen: Random): Path = {
    if (used.size >= cities.length) {
      return path
    } else {
      val sumWeight = doSumWeight(0, cities, pher, used, current, 0.0)
      val rndValue = rGen.nextDouble * sumWeight
      val nextCity = findSumWeight(0, 0, cities, pher, used, current,
                                   rndValue, 0.0)
      val nextPath = path :+ nextCity
      used + nextCity
      genPathRecurse(cities, pher, used, nextPath, nextCity, rGen)
    }
  }                     

  def genPath(cities: Matrix, pher: Matrix, rGen: Random): Path = {
    val current = rGen.nextInt(cities.length)
    val used = new CitySet
    used + current
    val path = List[Int](current)
    genPathRecurse(cities, pher, used, path, current, rGen)
  }

  def bestPathRecurse(cities: Matrix,
                      pher: Matrix,
                      rGen: Random,
                      maxIter: Int,
                      remainingIter: Int,
                      bestPathSoFar: Path,
                      bestLength: Double,
                      boost: Int): Path = {
    if (remainingIter <= 0) {
      bestPathSoFar
    } else {
      val path = genPath(cities, pher, rGen)
      val pathLen = pathLength(cities, path)
      val (newBestPath, newBestLength) =
        if (pathLen > bestLength) {
          // Remember we are trying to maximize score.
          updatePher(pher, path, boost)
          (path, pathLen)
        } else {
          (bestPathSoFar, bestLength)
        }
      evaporatePher(pher, maxIter, boost)
      bestPathRecurse(cities, pher, rGen, maxIter, (remainingIter-1),
        newBestPath, newBestLength, boost)
    }
  }

  def bestPath(cities: Matrix, rSeed: Int, numIter: Int, boost: Int): Path = {
    val rGen = new Random(rSeed)
    val pher = new Matrix(cities.length, cities.length)
    val path = List[Int]()
    bestPathRecurse(cities, pher, rGen, numIter, numIter, path, 0.0, boost)
  }

  def act() {
    println("starting to act: " + seed)
    val boost = 5
    val iter = 1000
    val numCities = 200
    val cityDistanceSeed = 1
    val cities = randomMatrix(numCities, numCities, cityDistanceSeed)
    val theBestPath = bestPath(cities, seed, iter, boost)
    val theBestPathLength = pathLength(cities, theBestPath)

    if (resultGatherer != null) {
      resultGatherer ! Result(theBestPath, theBestPathLength)
    } else {
      print(theBestPath)
      print(" : ")
      println(theBestPathLength)
    }
  }
}

