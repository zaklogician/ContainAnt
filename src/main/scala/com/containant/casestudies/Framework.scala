package com.containant.casestudies

/** The comparison framework:
 *  
 *  Do 100 runs on the given instances for each heuristic
 *  (while fixing the number of fitness function evaluations).
 * 
 *  Analyze the resulting data using the Wineberg-Christensen protocol.
 */


import scala.reflect.ClassTag

import org.apache.commons.math3.stat._
import org.apache.commons.math3.stat.inference._
import com.containant._
import com.containant.heuristics._

object Framework {

  case class RunResult[T](heuristic: Heuristic, solution: T, fitness: Double)
  /**
   * A run corresponds to a single execution of a given heuristic on a given
   * test instance with the goal of maximizing a fitness function.
   * 
   * The best of run is returned.
   */
  def run[T](
    heuristic: Heuristic,
    instance: Module,
    maximizing: T => Double
  )(implicit ev: ClassTag[T]): RunResult[T] = {
    object CA extends ContainAnt(heuristic)
    val created = CA create (instance, maximizing)
    RunResult[T](heuristic, created, maximizing(created))
  }
  
  
  trait DescriptiveSummary {
    val min: Double
    val mean: Double
    val max: Double
    val variance: Double
    override def toString: String = s"$min,$mean,$max,$variance"
  }
  
  case class ExperimentResult[T](
    heuristic1: Heuristic,
    heuristic2: Heuristic,
    results1: Seq[RunResult[T]],
    results2: Seq[RunResult[T]],
    mean1: Double,
    mean2: Double,
    pvalue: Double
  ) {
    object summary1 extends DescriptiveSummary {
      override val min: Double =
        StatUtils.min( results1.map(_.fitness).toArray )
      override val mean: Double =
        StatUtils.mean( results1.map(_.fitness).toArray )
      override val max: Double =
        StatUtils.max( results1.map(_.fitness).toArray )
      override val variance: Double =
        StatUtils.variance( results1.map(_.fitness).toArray )
      override def toString: String = s"$heuristic1,$min,$mean,$max,$variance"
    }
    object summary2 extends DescriptiveSummary {
      override val min: Double =
        StatUtils.min( results2.map(_.fitness).toArray ) 
      override val mean: Double =
        StatUtils.mean( results2.map(_.fitness).toArray )
      override val max: Double =
        StatUtils.max( results2.map(_.fitness).toArray )
      override val variance: Double =
        StatUtils.variance( results2.map(_.fitness).toArray )
      override def toString: String = s"$heuristic2,$min,$mean,$max,$variance"
    }
  }
  
  /** 
   * During a comparison experiment, two heuristics are compared to find which
   * one has a higher mean best of run on a single given test instance.
   * 
   * We use the Wineberg-Christensen protocol to determine the winner.
   */
  def experiment[T](
    heuristic1: Heuristic,
    heuristic2: Heuristic,
    runs: Int,
    instance: Module,
    maximizing: T => Double
  )(implicit ev: ClassTag[T]): ExperimentResult[T] = {
    // Perform all runs
    val results1 = for(r <- 1 to runs) yield
      run(heuristic1, instance, maximizing)
    val results2 = for(r <- 1 to runs) yield 
      run(heuristic2, instance, maximizing)
    
    // Merge results
    val merged = results1 ++ results2
    
    // Rank results
    val ranked = merged.sortBy(_.fitness).zipWithIndex
    
    // Average ranks after combining by fitness
    val combined = ranked.groupBy(_._1.fitness)
    val averaged = combined.values flatMap { xs =>
      val rank = xs.map(_._2).sum / (1.0*xs.size)
      xs.map(x => (x._1,rank))
    }
    
    // Break the ranks into groups
    val ranks1 = 
      averaged.filter(x => x._1.heuristic == heuristic1).map(_._2.toDouble)
    val ranks2 = 
      averaged.filter(x => x._1.heuristic == heuristic2).map(_._2.toDouble)
    
    // Perform a t-test on the rank groups
    val test = new TTest()
    val mean1 = StatUtils.mean(ranks1.toArray)
    val mean2 = StatUtils.mean(ranks2.toArray)
    val pvalue = test.pairedTTest(ranks1.toArray, ranks2.toArray)
    ExperimentResult( 
      heuristic1, heuristic2,
      results1, results2,
      mean1, mean2,
      pvalue
    )
  }

}
