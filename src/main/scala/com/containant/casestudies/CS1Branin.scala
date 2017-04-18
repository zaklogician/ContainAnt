package com.containant.casestudies

/** In this experiment, the goal is minimizing the well-known
 *  Branin function.
 */

import com.containant._
import com.containant.heuristics._

object CS1Branin {
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 100
  // target fitness fn. evaluations ~ 100 for comparison with SMAC, which often
  //                                  terminates under 100 on this prob.
  
  object Hmma extends AntHeuristic {
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
    override val _length: Int = 3
    override val _maxChoice: Int = 152
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

  //////////////////////////////////////////////////////////////////////
  // Problem Description
  
  case class X(value: Double)
  case class Y(value: Double)
  case class Branin(x1: Double, x2: Double) {
    val value: Double = 
      Math.pow(x2 - (5.1 / (4 * Math.PI * Math.PI)) *x1*x1 + 
      (5 / (Math.PI)) *x1 -6, 2) +
      10*(1- (1 / (8 * Math.PI))) * Math.cos(x1) + 10
  }
  
  def target(branin: Branin): Double = 1.0/branin.value
  
  object BraninModule extends Module {
    def branin(x: X, y: Y): Branin = Branin(x.value, y.value)
    
    val x1 = X(-5.0)
    val x2 = X(-4.9)
    val x3 = X(-4.8)
    val x4 = X(-4.7)
    val x5 = X(-4.6)
    val x6 = X(-4.5)
    val x7 = X(-4.4)
    val x8 = X(-4.3)
    val x9 = X(-4.2)
    val x10 = X(-4.1)
    val x11 = X(-4.0)
    val x12 = X(-3.9)
    val x13 = X(-3.8)
    val x14 = X(-3.7)
    val x15 = X(-3.6)
    val x16 = X(-3.5)
    val x17 = X(-3.4)
    val x18 = X(-3.3)
    val x19 = X(-3.2)
    val x20 = X(-3.1)
    val x21 = X(-3.0)
    val x22 = X(-2.9)
    val x23 = X(-2.8)
    val x24 = X(-2.7)
    val x25 = X(-2.6)
    val x26 = X(-2.5)
    val x27 = X(-2.4)
    val x28 = X(-2.3)
    val x29 = X(-2.2)
    val x30 = X(-2.1)
    val x31 = X(-2.0)
    val x32 = X(-1.9)
    val x33 = X(-1.8)
    val x34 = X(-1.7)
    val x35 = X(-1.6)
    val x36 = X(-1.5)
    val x37 = X(-1.4)
    val x38 = X(-1.3)
    val x39 = X(-1.2)
    val x40 = X(-1.1)
    val x41 = X(-1.0)
    val x42 = X(-0.9)
    val x43 = X(-0.8)
    val x44 = X(-0.7)
    val x45 = X(-0.6)
    val x46 = X(-0.5)
    val x47 = X(-0.4)
    val x48 = X(-0.3)
    val x49 = X(-0.2)
    val x50 = X(-0.1)
    val x51 = X(0.0)
    val x52 = X(0.1)
    val x53 = X(0.2)
    val x54 = X(0.3)
    val x55 = X(0.4)
    val x56 = X(0.5)
    val x57 = X(0.6)
    val x58 = X(0.7)
    val x59 = X(0.8)
    val x60 = X(0.9)
    val x61 = X(1.0)
    val x62 = X(1.1)
    val x63 = X(1.2)
    val x64 = X(1.3)
    val x65 = X(1.4)
    val x66 = X(1.5)
    val x67 = X(1.6)
    val x68 = X(1.7)
    val x69 = X(1.8)
    val x70 = X(1.9)
    val x71 = X(2.0)
    val x72 = X(2.1)
    val x73 = X(2.2)
    val x74 = X(2.3)
    val x75 = X(2.4)
    val x76 = X(2.5)
    val x77 = X(2.6)
    val x78 = X(2.7)
    val x79 = X(2.8)
    val x80 = X(2.9)
    val x81 = X(3.0)
    val x82 = X(3.1)
    val x83 = X(3.2)
    val x84 = X(3.3)
    val x85 = X(3.4)
    val x86 = X(3.5)
    val x87 = X(3.6)
    val x88 = X(3.7)
    val x89 = X(3.8)
    val x90 = X(3.9)
    val x91 = X(4.0)
    val x92 = X(4.1)
    val x93 = X(4.2)
    val x94 = X(4.3)
    val x95 = X(4.4)
    val x96 = X(4.5)
    val x97 = X(4.6)
    val x98 = X(4.7)
    val x99 = X(4.8)
    val x100 = X(4.9)
    val x101 = X(5.0)
    val x102 = X(5.1)
    val x103 = X(5.2)
    val x104 = X(5.3)
    val x105 = X(5.4)
    val x106 = X(5.5)
    val x107 = X(5.6)
    val x108 = X(5.7)
    val x109 = X(5.8)
    val x110 = X(5.9)
    val x111 = X(6.0)
    val x112 = X(6.1)
    val x113 = X(6.2)
    val x114 = X(6.3)
    val x115 = X(6.4)
    val x116 = X(6.5)
    val x117 = X(6.6)
    val x118 = X(6.7)
    val x119 = X(6.8)
    val x120 = X(6.9)
    val x121 = X(7.0)
    val x122 = X(7.1)
    val x123 = X(7.2)
    val x124 = X(7.3)
    val x125 = X(7.4)
    val x126 = X(7.5)
    val x127 = X(7.6)
    val x128 = X(7.7)
    val x129 = X(7.8)
    val x130 = X(7.9)
    val x131 = X(8.0)
    val x132 = X(8.1)
    val x133 = X(8.2)
    val x134 = X(8.3)
    val x135 = X(8.4)
    val x136 = X(8.5)
    val x137 = X(8.6)
    val x138 = X(8.7)
    val x139 = X(8.8)
    val x140 = X(8.9)
    val x141 = X(9.0)
    val x142 = X(9.1)
    val x143 = X(9.2)
    val x144 = X(9.3)
    val x145 = X(9.4)
    val x146 = X(9.5)
    val x147 = X(9.6)
    val x148 = X(9.7)
    val x149 = X(9.8)
    val x150 = X(9.9)
    val x151 = X(10.0)
    val y1 = Y(0.0); 
    val y2 = Y(0.1); 
    val y3 = Y(0.2); 
    val y4 = Y(0.3); 
    val y5 = Y(0.4); 
    val y6 = Y(0.5); 
    val y7 = Y(0.6); 
    val y8 = Y(0.7); 
    val y9 = Y(0.8); 
    val y10 = Y(0.9); 
    val y11 = Y(1.0); 
    val y12 = Y(1.1); 
    val y13 = Y(1.2); 
    val y14 = Y(1.3); 
    val y15 = Y(1.4); 
    val y16 = Y(1.5); 
    val y17 = Y(1.6); 
    val y18 = Y(1.7); 
    val y19 = Y(1.8); 
    val y20 = Y(1.9); 
    val y21 = Y(2.0); 
    val y22 = Y(2.1); 
    val y23 = Y(2.2); 
    val y24 = Y(2.3); 
    val y25 = Y(2.4); 
    val y26 = Y(2.5); 
    val y27 = Y(2.6); 
    val y28 = Y(2.7); 
    val y29 = Y(2.8); 
    val y30 = Y(2.9); 
    val y31 = Y(3.0); 
    val y32 = Y(3.1); 
    val y33 = Y(3.2); 
    val y34 = Y(3.3); 
    val y35 = Y(3.4); 
    val y36 = Y(3.5); 
    val y37 = Y(3.6); 
    val y38 = Y(3.7); 
    val y39 = Y(3.8); 
    val y40 = Y(3.9); 
    val y41 = Y(4.0); 
    val y42 = Y(4.1); 
    val y43 = Y(4.2); 
    val y44 = Y(4.3); 
    val y45 = Y(4.4); 
    val y46 = Y(4.5); 
    val y47 = Y(4.6); 
    val y48 = Y(4.7); 
    val y49 = Y(4.8); 
    val y50 = Y(4.9); 
    val y51 = Y(5.0); 
    val y52 = Y(5.1); 
    val y53 = Y(5.2); 
    val y54 = Y(5.3); 
    val y55 = Y(5.4); 
    val y56 = Y(5.5); 
    val y57 = Y(5.6); 
    val y58 = Y(5.7); 
    val y59 = Y(5.8); 
    val y60 = Y(5.9); 
    val y61 = Y(6.0); 
    val y62 = Y(6.1); 
    val y63 = Y(6.2); 
    val y64 = Y(6.3); 
    val y65 = Y(6.4); 
    val y66 = Y(6.5); 
    val y67 = Y(6.6); 
    val y68 = Y(6.7); 
    val y69 = Y(6.8); 
    val y70 = Y(6.9); 
    val y71 = Y(7.0); 
    val y72 = Y(7.1); 
    val y73 = Y(7.2); 
    val y74 = Y(7.3); 
    val y75 = Y(7.4); 
    val y76 = Y(7.5); 
    val y77 = Y(7.6); 
    val y78 = Y(7.7); 
    val y79 = Y(7.8); 
    val y80 = Y(7.9); 
    val y81 = Y(8.0); 
    val y82 = Y(8.1); 
    val y83 = Y(8.2); 
    val y84 = Y(8.3); 
    val y85 = Y(8.4); 
    val y86 = Y(8.5); 
    val y87 = Y(8.6); 
    val y88 = Y(8.7); 
    val y89 = Y(8.8); 
    val y90 = Y(8.9); 
    val y91 = Y(9.0); 
    val y92 = Y(9.1); 
    val y93 = Y(9.2); 
    val y94 = Y(9.3); 
    val y95 = Y(9.4); 
    val y96 = Y(9.5); 
    val y97 = Y(9.6); 
    val y98 = Y(9.7); 
    val y99 = Y(9.8); 
    val y100 = Y(9.9); 
    val y101 = Y(10.0); 
    val y102 = Y(10.1); 
    val y103 = Y(10.2); 
    val y104 = Y(10.3); 
    val y105 = Y(10.4); 
    val y106 = Y(10.5); 
    val y107 = Y(10.6); 
    val y108 = Y(10.7); 
    val y109 = Y(10.8); 
    val y110 = Y(10.9); 
    val y111 = Y(11.0); 
    val y112 = Y(11.1); 
    val y113 = Y(11.2); 
    val y114 = Y(11.3); 
    val y115 = Y(11.4); 
    val y116 = Y(11.5); 
    val y117 = Y(11.6); 
    val y118 = Y(11.7); 
    val y119 = Y(11.8); 
    val y120 = Y(11.9); 
    val y121 = Y(12.0); 
    val y122 = Y(12.1); 
    val y123 = Y(12.2); 
    val y124 = Y(12.3); 
    val y125 = Y(12.4); 
    val y126 = Y(12.5); 
    val y127 = Y(12.6); 
    val y128 = Y(12.7); 
    val y129 = Y(12.8); 
    val y130 = Y(12.9); 
    val y131 = Y(13.0); 
    val y132 = Y(13.1); 
    val y133 = Y(13.2); 
    val y134 = Y(13.3); 
    val y135 = Y(13.4); 
    val y136 = Y(13.5); 
    val y137 = Y(13.6); 
    val y138 = Y(13.7); 
    val y139 = Y(13.8); 
    val y140 = Y(13.9); 
    val y141 = Y(14.0); 
    val y142 = Y(14.1); 
    val y143 = Y(14.2); 
    val y144 = Y(14.3); 
    val y145 = Y(14.4); 
    val y146 = Y(14.5); 
    val y147 = Y(14.6); 
    val y148 = Y(14.7); 
    val y149 = Y(14.8); 
    val y150 = Y(14.9); 
    val y151 = Y(15.0);
    
  }
  
  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    import com.containant.casestudies.Framework
    println("\n-----------------------------")
    println("Case Study 1: Branin Function")
    println("Runs: " + _runs)
    
    val comparison =
      Framework.experiment[Branin](Hmma, Hgre, _runs, BraninModule, target)

    val reference =
      Framework.experiment[Branin](Hran, Hran, _runs, BraninModule, target)
    
    println("heuristic,min,mean,max,var")
    println(comparison.summary1)
    println(comparison.summary2)
    println(reference.summary1)
    println("p: " + comparison.pvalue)
    println()
  }
  
}
