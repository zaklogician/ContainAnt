package com.containant.heuristics

import scala.reflect.ClassTag
import scala.collection.mutable

import com.containant.LBNF

trait RandomHeuristic extends Heuristic {
  val _iterations: Int = 100
  val _recursionDepth: Int = 10
  val RNG: java.util.Random = new java.util.Random(0xDEADBEEF)
  
  def apply(tgrammar: LBNF)(
    tfitness: tgrammar.SyntaxTree => Double,
    target: tgrammar.Sort 
  ): Option[tgrammar.SyntaxTree] = {
    val process = new Process {
      override val grammar = tgrammar
      override val fitness = tfitness.asInstanceOf[grammar.SyntaxTree => Double]
    }
    process.iterate(target.asInstanceOf[process.grammar.Sort]).asInstanceOf[Option[tgrammar.SyntaxTree]]
  }
  
  trait Process {
    val grammar: LBNF
    val fitness: grammar.SyntaxTree => Double
     
    private type Sort = grammar.Sort
    private type Label = grammar.Label
    
    def choose(sort: Sort): Label = {
      val labels = grammar.labels(sort)
      labels(RNG.nextInt(labels.size))
    }
    
    def chooseDeep(sort: Sort): Label = {
      val labels = grammar.labels(sort).filter( l => grammar.rule(l).length <= 1 )
      if (labels.size == 0) throw new Exception("Recursion depth exceeded, cannot continue on " + sort)
      labels(RNG.nextInt(labels.size))
    }

    def step(depth: Int, sort: Sort): grammar.SyntaxTree = {
      val label = if (depth < _recursionDepth) choose(sort) else chooseDeep(sort)
      val subtrees = grammar.rule(label).map { s =>
        step(depth+1,s)
      }
      grammar.SyntaxTree(label, subtrees)
    }
    
    
    def iterate(sort: Sort): Option[grammar.SyntaxTree] = {
      var best: Option[grammar.SyntaxTree] = None
      var fbest: Double = 0
      
      for(i <- 1 to _iterations) {
        val current = step(0,sort)
        val fcurrent = fitness(current)
        if( fcurrent > fbest ) {
          best = Some(current)
          fbest = fcurrent
        }
      }
      best
    }
  
  } // end process
  
}
