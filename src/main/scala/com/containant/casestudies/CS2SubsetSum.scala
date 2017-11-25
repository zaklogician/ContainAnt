package com.containant.casestudies

/** The dependency injection problem can encode various combinatorial
 *  optimization problems easily and intuitively.
 *
 *  The following encodes the subset-sum problem.
 *
 *  The test instances are from
 *  https://people.sc.fsu.edu/~jburkardt/datasets/subset_sum/subset_sum.html
 */

import com.containant._
import com.containant.heuristics._

object CS2SubsetSum {
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 100
  // target fitness fn. evaluations ~ 100 for comparison with SMAC, which often
  //                                  terminates under 100 on this prob.
  
  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 10
    override val _evaporationRate: Double = 0.4
    override val _iterations: Int = 1000
    override val _antNumber: Int = 3
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 10
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  }
  
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 100
    override val _length: Int = 9
    override val _maxChoice: Int = 7
    override val _tournamentSize = 5
    override val _generations = 10
    override val _recursionDepth = 10
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = 100
    override val _recursionDepth = 10
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "ran"
  }
  
   object Hmct extends MCTSHeuristic {
      override val _iterations = 1000
      override val RNG: java.util.Random = new java.util.Random(_seed)
      override def toString: String = "mct"
  }

  //////////////////////////////////////////////////////////////////////
  // Problem Description

  type Solution = Set[Int]
  
  trait SubsetSumModule extends Module {
    val empty: Solution = Set()
    def add(i: Int, xs: Solution): Solution = xs + i
  }
  
  def subsetSum(target: Int)(solution: Solution): Double = {
    if (solution.sum == target) 2
    else 1.0/ Math.abs(solution.sum - target).toDouble
  }

  object P02 extends SubsetSumModule {
    val i267: Int = 267
    val i493: Int = 493
    val i869: Int = 869
    val i961: Int = 961
    val i1000: Int = 1000
    val i1153: Int = 1153
    val i1246: Int = 1246
    val i1598: Int = 1598
    val i1766: Int = 1766
    val i1922: Int = 1922
  } // target: 5842
  
  object P03 extends SubsetSumModule {
    val i518533: Int = 518533
    val i1037066: Int = 1037066
    val i2074132: Int = 2074132
    val i1648264: Int = 1648264
    val i796528: Int = 796528
    val i1593056: Int = 1593056
    val i686112: Int = 686112
    val i1372224: Int = 1372224
    val i244448: Int = 244448
    val i488896: Int = 488896
    val i977792: Int = 977792
    val i1955584: Int = 1955584
    val i1411168: Int = 1411168
    val i322336: Int = 322336
    val i644672: Int = 644672
    val i1289344: Int = 1289344
    val i78688: Int = 78688
    val i157376: Int = 157376
    val i314752: Int = 314752
    val i629504: Int = 629504
    val i1259008: Int = 1259008
  } // target: 2463098


  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    import com.containant.casestudies.Framework
    println("\n-----------------------------")
    println("Case Study 2: Subset Sum (P02)")
    println("Runs: " + _runs)
    
    {
      val comparison = Framework.experimentN[Solution](Seq(Hmma, Hgre, Hmct),_runs, P02, subsetSum(5842))
      val reference = Framework.experiment[Solution](Hran, Hran, _runs, P02, subsetSum(5842))
    
      println("heuristic,min,mean,max,var")
      for(i <- 0 until comparison.heuristics.size) {
        println(comparison.summaries(i))
      }
      println(reference.summaries(0))
      println("pvalues: " + comparison.pvalues)
      println(if (comparison.pvalues.max < 0.05) "significant" else "not significant")
      println()
    }

    println("\n-----------------------------")
    println("Case Study 2: Subset Sum (P03)")
    println("Runs: " + _runs)

    {
      val comparison = Framework.experimentN[Solution](Seq(Hmma, Hgre, Hmct),_runs, P03, subsetSum(2463098))
      val reference = Framework.experiment[Solution](Hran, Hran, _runs, P03, subsetSum(2463098))
    
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
  
}
