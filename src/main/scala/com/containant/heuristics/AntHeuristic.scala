package com.containant.heuristics

import scala.collection.mutable

import com.containant.LBNF

trait AntHeuristic extends Heuristic {
  val _maxPheromone: Double = 1
  val _evaporationRate: Double = 0.4
  val _iterations: Int = 100
  val _recursionDepth: Int = 10
  val _antNumber: Int = 3
  val _minimumFraction: Double = 0.10
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
    
    
    type Quality = Double
    type QMap = mutable.Map[ Sort, mutable.Map[Label,Quality] ]
    val qmap: QMap = mutable.Map()
    def Q(sort: Sort): mutable.Map[Label, Quality] = 
      qmap.getOrElseUpdate(sort, mutable.Map())
    def Q(sort: Sort, label: Label): Quality = 
      Q(sort).getOrElseUpdate(label, maxQuality)
    def Qmax(sort: Sort): Quality = {
      val qualities = grammar.labels(sort).map { label => 
        Q(sort, label)
      }
      qualities.max
    }
    def setQ(sort: Sort, label: Label)(value: Quality): Unit = {
      val boundedValue = Math.max(minQuality, Math.min(value, maxQuality) )
      Q(sort).put(label, boundedValue)
    }
    
    var maxQuality: Quality = _maxPheromone
    def minQuality: Quality = _minimumFraction*maxQuality
    
    ////////////////////////////////////////////////////////////////////////////
    // CHOICE:
    
    // quality-proportional choice
    def choose(sort: Sort): Label = {
      val qualities = grammar.labels(sort).map { label => 
        (label, RNG.nextDouble*Q(sort, label))
      }
      qualities.maxBy(_._2)._1
    }
    
    def chooseNoFork(sort: Sort): Label = {
      val qualities = grammar.labels(sort).filter( l => grammar.rule(l).length <= 1 ).map { label => 
        (label, RNG.nextDouble*Q(sort, label))
      }
      if (qualities.size == 0) throw new Exception("Recursion depth exceeded, cannot continue on " + sort)
      qualities.maxBy(_._2)._1
    }
    
    def deposit(reward: Quality, tree: grammar.SyntaxTree): Unit = {
      tree.subtrees.foreach { subtree => deposit(reward, subtree) }
      val label  = tree.label
      val source = grammar.sort(label)
      setQ(source,label) { Q(source,label) + reward }
    }

    def evaporate: Unit = {
      grammar.sorts.foreach { sort =>
        Q(sort).foreach { label =>
          setQ(sort, label._1) { Q(sort,label._1) * _evaporationRate }
        }
        //Q(sort).mapValues(v => v*_evaporationRate)
      }
    }
    
    def substep(depth: Int, sort: Sort): grammar.SyntaxTree = {
      // choose next label
      val label = if (depth < _recursionDepth) choose(sort) else chooseNoFork(sort)
      // construct subtrees
      val subtrees = grammar.rule(label).map { s =>
        substep(depth+1,s)
      }
      grammar.SyntaxTree(label, subtrees)
    }
    
    def construct(sort: Sort): (grammar.SyntaxTree, Quality) = {
      val result = substep(0,sort)
      (result, fitness(result))
    }
    
    def iterate(sort: Sort): Option[grammar.SyntaxTree] = {
      var best: grammar.SyntaxTree = construct(sort)._1
      var fbest: Double = fitness(best)
      maxQuality = fitness(best)
      
      for(i <- 1 to _iterations) {
        val ants = for( a <- 0 to _antNumber ) yield construct(sort)
        val iterationBest = ants.maxBy(_._2)
        deposit(iterationBest._2, iterationBest._1)
        if( iterationBest._2 > fbest ) {
          best = iterationBest._1
          fbest = iterationBest._2
          maxQuality = fbest
        }
        evaporate
      }
      Some(best)
    }
  
  } // end process
  
}
