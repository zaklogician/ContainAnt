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
  
  
  case class DescriptiveSummary(
    name: String,
    min: Double,
    mean: Double,
    max: Double,
    variance: Double
  ) {
    override def toString: String = s"$name,$min,$mean,$max,$variance"
  }
  
  case class ExperimentResult[T](
    heuristics: Seq[Heuristic],
    results: Seq[Seq[RunResult[T]]],
    pvalues: Seq[Double]
  ) {
    
    def summarize(heuristic: Heuristic, results: Seq[RunResult[T]]): DescriptiveSummary = 
      DescriptiveSummary(
        heuristic.toString,
        StatUtils.min( results.map(_.fitness).toArray ),
        StatUtils.mean( results.map(_.fitness).toArray ),
        StatUtils.max( results.map(_.fitness).toArray ),
        StatUtils.variance( results.map(_.fitness).toArray )
      )
    
    val summaries: Seq[DescriptiveSummary] = 
      (heuristics, results).zipped map { (x,y) => summarize(x,y) }
    
  }
  
  /** 
   * During a comparison experiment, two heuristics are compared to find which
   * one has a higher mean best of run on a single given test instance.
   * 
   * We use the Wineberg-Christensen protocol to determine the winner.
   */   
  def experimentN[T](
    heuristics: Seq[Heuristic],
    runs: Int,
    instance: Module,
    maximizing: T => Double
  )(implicit ev: ClassTag[T]): ExperimentResult[T] = {
    // Perform all runs
    val justResults: Seq[(Heuristic,Seq[RunResult[T]])] = heuristics.map { h =>
      (h, for (r <- 1 to runs) yield run(h, instance, maximizing))
    }
    val results: Map[Heuristic, Seq[RunResult[T]]] = Map() ++ justResults

    
    val pvalues = for {
      i <- 0   until heuristics.size
      j <- i+1 until heuristics.size
    } yield {
      val heuristic1 = heuristics(i)
      val heuristic2 = heuristics(j)
      val results1 = results( heuristic1 )
      val results2 = results( heuristic2 )
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
      test.pairedTTest(ranks1.toArray, ranks2.toArray)
    }
    
    ExperimentResult( 
      heuristics,
      justResults.map(_._2),
      pvalues
    )
  }
  
  def experiment[T](
    heuristic1: Heuristic,
    heuristic2: Heuristic,
    runs: Int,
    instance: Module,
    maximizing: T => Double
  )(implicit ev: ClassTag[T]): ExperimentResult[T] =
    experimentN(Seq(heuristic1,heuristic2),runs,instance,maximizing)

}
