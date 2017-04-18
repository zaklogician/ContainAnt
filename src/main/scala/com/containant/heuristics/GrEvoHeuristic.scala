package com.containant.heuristics

import scala.collection.mutable

import com.containant.LBNF

trait GrEvoHeuristic extends Heuristic {
  val _population: Int = 10
  val _length: Int = 10
  val _maxChoice: Int = 10
  val _tournamentSize = 5
  val _generations = 10
  val _recursionDepth = 8
  val RNG: java.util.Random = new java.util.Random(0xDEADBEEF)
  
  def apply(tgrammar: LBNF)(
    tfitness: tgrammar.SyntaxTree => Double,
    ttarget: tgrammar.Sort 
  ): Option[tgrammar.SyntaxTree] = {
    val process = new Process {
      override val grammar = tgrammar
      override val fitness = tfitness.asInstanceOf[grammar.SyntaxTree => Double]
      override val target = ttarget.asInstanceOf[grammar.Sort]
    }
    Some( process.perform.asInstanceOf[tgrammar.SyntaxTree] )
  }
  
  trait Process {
    val grammar: LBNF
    val fitness: grammar.SyntaxTree => Double
    val target: grammar.Sort
    
    private type Sort = grammar.Sort
    private type Label = grammar.Label
    
    
    type Individual = IndexedSeq[Int]
     
    /** Turns a given Individual into a tree with the given base Sort
     *  using a depth-first strategy, starting from the given index and depth.
     *
     *  @param sort The given base sort.
     *  @param ind The given individual.
     *  @param startIx The given starting index.
     *  @param depth The given starting depth.
     *
     *  @return the next starting index and the given tree.
     */
    def treeOf(sort: Sort, ind: Individual, startIx: Int, depth: Int): (Int, grammar.SyntaxTree) = {
      val labels = if (depth > _recursionDepth) 
        grammar.labels(sort).filter( l => grammar.rule(l).length < 1 )
      else grammar.labels(sort)
      if (labels.size == 0) throw new Exception("Recursion depth exceeded, cannot continue on " + sort)
      var ix = startIx % ind.size               // prevent index from growing too big
      val label = labels(ind(ix) % labels.size) // choose label by index
      val subtrees = grammar.rule(label).map { s =>
        val result = treeOf(s,ind,ix+1,depth+1)
        ix = result._1
        result._2
      }
      (ix, grammar.SyntaxTree(label, subtrees))
    }
    
    type Cache = mutable.Map[Individual, Double]
    var cache: Cache = mutable.Map()
    def fitnessOf(ind: Individual): Double = {
      cache.getOrElseUpdate( ind, fitness(treeOf(target,ind,0,0)._2) )
    }
    
    /** Performs one-point crossover on the given parent individuals.
     *
     *  @param a The first parent.
     *  @param b The second parent.
     */
    def crossover( a: Individual, b: Individual ): (Individual, Individual) = {
      val xpoint = RNG.nextInt(a.length)
      val aS = a.splitAt(xpoint)
      val bS = b.splitAt(xpoint)
      (aS._1 ++ bS._2, bS._1 ++ aS._2)
    }

    /** Performs a one-point mutation on the given individual.
     *
     *  @param a The given individual.
     */
    def mutate( a: Individual ): Individual = {
      val mpoint = RNG.nextInt(a.length)
      val result = a.updated(mpoint, RNG.nextInt(_maxChoice))
      result
    }

    /** Performs a tournament selection on a random sample of the given
     *  population.
     *
     *  @param population The given population.
     *
     *  @returns The winner of the tournament.
     */
    def tournamentSelect(population: List[Individual]): Individual = {
      val tournament = (1 to _tournamentSize).map { i => 
        population( RNG.nextInt(population.size) )
      }
      tournament.reduce { (champion,contender) =>
        if (fitnessOf(champion) > fitnessOf(contender)) champion else contender
      }
    }

    /** Performs a single iteration (generation) of the Grammatical
     *  Evolution algorithm, starting from the given population.
     *
     *  @param population The given population.
     *
     *  @returns The population of the next generation.
     */
    def performIteration(population: List[Individual]): List[Individual] = {
      (1 to population.length/2).toList flatMap { i =>
        val parent1 = tournamentSelect(population)
        val parent2 = tournamentSelect(population)
        val child = crossover(parent1,parent2)
        List( mutate(child._1), mutate(child._2) )
      }
    }

    /** Creates a new random individual. */
    def newIndividual: Individual = (1 to _length).map( i => RNG.nextInt(_maxChoice) )

    /** Creates a new random population. */
    def newPopulation: List[Individual] = {
      (1 to _population).map( _ => newIndividual ).toList
    }

    /** Performs the Grammatical Evolution algorithm.
     *
     *  @returns The tree of the fittest individual found.
     */
    def perform: grammar.SyntaxTree = {
      var population = newPopulation
      var best = population.sortBy(fitnessOf).last
      for (g <- 1 to _generations) {
        cache = mutable.Map() // clear fitness cache
        val nextPopulation = performIteration(population)
        population = (population ++ nextPopulation).sortBy(fitnessOf).reverse.take(population.size)
        best = population.sortBy(fitnessOf).head
      }
      treeOf(target,best,0,0)._2
    }
    
  } // end process
  
}
