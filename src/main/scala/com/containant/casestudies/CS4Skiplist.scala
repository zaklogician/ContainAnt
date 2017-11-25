package com.containant.casestudies

/** In this experiment, the goal is choosing a probability
 *  distribution for a skip list data structure that minimizes
 *  access times.
 */

import com.containant._
import com.containant.heuristics._
import com.containant.casestudies.util._

object CS4Skiplist {
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 100
  val _load: Int = 100             // test load of the skip list
  val _testseed: Int = 0xBFB09E2F  // seed for generating the test load
  
  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 0.25
    override val _evaporationRate: Double = 0.4
    override val _iterations: Int = 100
    override val _antNumber: Int = 1
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  }
  
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 100
    override val _length: Int = 6
    override val _maxChoice: Int = 7
    override val _tournamentSize = 5
    override val _generations = 5
    override val _recursionDepth = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = 100
    override val _recursionDepth = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "ran"
  }
  
  object Hmct extends MCTSHeuristic {
      override val _iterations = 100
      override val RNG: java.util.Random = new java.util.Random(_seed)
      override def toString: String = "mct"
  }

  //////////////////////////////////////////////////////////////////////
  // Problem Description
  
  /** Average lookup time for a uniformly filled skiplist */
  def uniformLoad(p: SkipList): Double = {
    val rng = new java.util.Random(_testseed)
    for(_ <- 1 to _load) p.insert(rng.nextInt(10*_load))
    val times = for( i <- 1 to _load ) yield p.findTime(rng.nextInt(_load))
    1 / (times.sum.toDouble/times.length)
  }

  object SkipModule extends Module {
    def arithmetic(increment: Double): List[Int] = {
      List.iterate(1,10)(x => (x+increment).toInt)
    }
    def geometric(slope: Double): List[Int] = {
      List.iterate(1,10)(x => (slope*x).toInt)
    }
    def combined(increment: Double, slope: Double): List[Int] = {
      List.iterate(1,10)(x => (slope*x+increment).toInt)
    }

    def simple(x: List[Int]): Prob = Prob(x)    
    def skipList(depth: Int, prob: Prob): SkipList = new SkipList(depth, prob)
    
    val i2: Int = 2
    val i3: Int = 3
    val i4: Int = 4
    val i5: Int = 5
    val i6: Int = 6
    val i7: Int = 7
    val i8: Int = 8
    val i9: Int = 9
    val i10: Int = 10
    
    val d1: Double = 1.1
    val d2: Double = 1.2
    val d3: Double = 1.3
    val d4: Double = 1.4
    val d5: Double = 1.5
    val d6: Double = 1.6
    val d7: Double = 1.7
    val d8: Double = 1.8
    val d9: Double = 1.9
    val d10: Double = 2.0
    val d11: Double = 2.1
    val d12: Double = 2.2
    val d13: Double = 2.3
    val d14: Double = 2.4
    val d15: Double = 2.5
    val d16: Double = 2.6
    val d17: Double = 2.7
    val d18: Double = 2.8
    val d19: Double = 2.9
    val d20: Double = 3.0
  }
 
  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    import com.containant.casestudies.Framework
    println("\n-----------------------------")
    println("Case Study 4: Skiplist Configuration")
    println("Runs: " + _runs)
    
    val comparison =
      Framework.experimentN[SkipList](Seq(Hmma, Hgre, Hmct),_runs, SkipModule, uniformLoad)
    
    val reference =
      Framework.experiment[SkipList](Hran, Hran, _runs, SkipModule, uniformLoad)
    
    println("heuristic,min,mean,max,var")
    for(i <- 0 until comparison.heuristics.size) {
      println(comparison.summaries(i))
    }
    println(reference.summaries(0))
    println("pvalues: " + comparison.pvalues)
    println(if (comparison.pvalues.max < 0.05) "significant" else "not significant")
    println()
  }
  
}
