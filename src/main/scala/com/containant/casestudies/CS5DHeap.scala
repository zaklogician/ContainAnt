package com.containant.casestudies

/** This example shows how ContainAnt encompasses all of 
 *  the Hoos-Hsu' Programming-by-Optimization examples, by
 *  solving their DHeap optimization problem. 
 */

import com.containant._
import com.containant.heuristics._
import com.containant.casestudies.util._

object CS5DHeap {
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 10

  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 50000
    override val _evaporationRate: Double = 0.4
    override val _iterations: Int = 1000
    override val _antNumber: Int = 1
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  }
  
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 100
    override val _length: Int = 7
    override val _maxChoice: Int = 15
    override val _tournamentSize = 5
    override val _generations = 10
    override val _recursionDepth = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = 1000
    override val _recursionDepth = 5
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


  def target(heap: DHeap): Double = heap.measureAccess //1.0/heap.measureAccess.toDouble
  
  class A(val value: Int) // arity
  class E(val value: Int) // expansion_factor
  class I(val value: Int) // initial_size
  
  object HeapModule extends Module {
    def dheap(arity: A, expansion_factor: E, initial_size: I): DHeap =
      new DHeap(arity.value, expansion_factor.value, initial_size.value)
    
    val a2 = new A(2)
    val a3 = new A(3)
    val a4 = new A(4)
    val a5 = new A(5)
    val a6 = new A(6)
    val a7 = new A(7)
    val a8 = new A(8)
    
    val e2 = new E(2)
    val e3 = new E(3)
    val e4 = new E(4)
    val e5 = new E(5)
    val e6 = new E(6)
    val e7 = new E(7)
    val e8 = new E(8)
    val e9 = new E(9)
    val e10 = new E(10)
    val e11 = new E(11)
    val e12 = new E(12)
    val e13 = new E(13)
    val e14 = new E(14)
    val e15 = new E(15)
    val e16 = new E(16)
    
    val i2 = new I(2)
    val i4 = new I(4)
    val i8 = new I(8)
    val i16 = new I(16)
    val i32 = new I(32)
    val i64 = new I(64)
    val i128 = new I(128)
    val i256 = new I(256)
    val i512 = new I(512)
    val i1024 = new I(1024)
    val i2048 = new I(2048)
    val i4096 = new I(4096)
    val i8192 = new I(8192)
    val i16384 = new I(16384)
    val i32768 = new I(32768)
    
    val i9 = new I(9)
    val i27 = new I(27)
    val i81 = new I(81)
    val i243 = new I(243)
    val i729 = new I(729)
    val i2187 = new I(2187)
    val i6561 = new I(6561)
    val i19683 = new I(19683)
    val i59049 = new I(59049)
  }
 
  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    import com.containant.casestudies.Framework
    println("\n-----------------------------")
    println("Case Study 5: Dynamic Heap")
    println("Runs: " + _runs)
    
    val comparison =
      Framework.experimentN[DHeap](Seq(Hmma, Hgre, Hmct),_runs, HeapModule, target)
    
    val reference =
      Framework.experiment[DHeap](Hran, Hran, _runs, HeapModule, target)
    
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
