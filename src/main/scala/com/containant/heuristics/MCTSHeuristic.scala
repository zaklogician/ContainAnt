package com.containant.heuristics

import scala.collection.mutable
import com.containant.LBNF
import scala.annotation._

trait MCTSHeuristic extends Heuristic {
  val _iterations: Int = 100
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
    
    private def sequence[T](l : Seq[Option[T]]) = 
      if (l.contains(None)) None else Some(l.flatten)
    
    // partial syntax trees
    sealed trait PST {
      def toSyntaxTree: grammar.SyntaxTree
      def applyLabel(label: Label): Option[PST]
      def rolloutStep: Option[PST]
      
      @tailrec final def rollout: grammar.SyntaxTree = this.rolloutStep match {
        case None       => {
          val result = this.toSyntaxTree
          assert(this match {
            case MissingLeaf(s) => grammar.sort(result.label) == s
            case Node(label,_)  => grammar.sort(result.label) == grammar.sort(label)
          })
          result
        }
        case Some(that) => that.rollout
      }
      
      def leftmostLabels: Option[Seq[Label]]
    }
    case class MissingLeaf(s: Sort) extends PST {
      override def toSyntaxTree: grammar.SyntaxTree = throw new Exception("Missing leaf" + s)
      override def applyLabel(label: Label): Option[PST] = 
        if (grammar.sort(label) == s)
          Some(Node(label, grammar.rule(label).map(MissingLeaf)))
        else None
      
      override def rolloutStep: Option[PST] = {
        val labels = grammar.labels(s)
        if (labels.isEmpty)
          throw new Exception("The sort " + s + " has no associated labels.")
        val label: Label = labels(RNG.nextInt(labels.size))
        assert( grammar.sort(label) == s )
        Some { Node( label, grammar.rule(label).map(MissingLeaf) ) }
      }
      
      override def leftmostLabels: Option[Seq[Label]] = {
        Some( grammar.labels(s) )
      }
    }
    
    case class Node(label: Label, subtrees: Seq[PST]) extends PST {
      override def toSyntaxTree: grammar.SyntaxTree = 
        grammar.SyntaxTree(label, subtrees.map(_.toSyntaxTree) )
      
      override def applyLabel(label: Label): Option[PST] = {
        var success = false
        def rec(subtrees: Seq[PST]): Seq[PST] = 
          if( subtrees.isEmpty ) Seq.empty
          else {
            subtrees.head.applyLabel(label) match {
              case None => subtrees.head +: rec( subtrees.tail )
              case Some(y) => { success = true; y +: subtrees.tail }
	    }
          }
        val newSubtrees = rec( subtrees )
        if( success ) Some(Node(this.label,newSubtrees)) else None
      }
      
      override def rolloutStep: Option[PST] = {
        var success = false
        def rec(subtrees: Seq[PST]): Seq[PST] =
          if( subtrees.isEmpty ) Seq.empty
          else {
            subtrees.head.rolloutStep match {
              case None => subtrees.head +: rec( subtrees.tail )
              case Some(y) => { success = true; y +: subtrees.tail }
            }
          }
        val newSubtrees = rec( subtrees )
        if( success ) Some( Node(label,newSubtrees) ) else None
      }
      
      override def leftmostLabels: Option[Seq[Label]] = {
        def rec(subtrees: Seq[PST]): Option[Seq[Label]] = {
          if( subtrees.isEmpty ) None
          else {
            subtrees.head.leftmostLabels match {
              case None => rec( subtrees.tail )
              case Some(y) => Some(y)
            }
          }
        }
        rec(subtrees)
      }
      
    }

    class GameNode(val gameState: PST) {      
      var children: Seq[GameNode] = Seq.empty
      var visits: Int = 0
      var value: Double = 1
      
      val epsilon: Double = 0.00001
      
      def selectAction: (grammar.SyntaxTree, Quality) = {
        val visited: scala.collection.mutable.ListBuffer[GameNode] = scala.collection.mutable.ListBuffer.empty[GameNode]
        var current: GameNode = this
        while( !current.children.isEmpty ) {
          visited.append( current )
          current = current.select // bias-proportional selection
        }
        if( current.gameState.leftmostLabels.isEmpty ) {
          val created = current.gameState.rollout
          val value = fitness(created)
          for (v <- visited) v.backpropagate(value)
          (created,value)
        } else {
          current.expand
          val newNode = current.select // bias-proportional selection
          visited.append( newNode )
          val created = newNode.gameState.rollout
	  val value = fitness(created)
          for (v <- visited) v.backpropagate(value)
          (created,value)
        }
      }
      
      def select: GameNode = {
        if(children.isEmpty) throw new Exception("No children in " + this.gameState)
        children.maxBy { c =>
          c.value / (c.visits + epsilon) + math.sqrt(  math.log(visits+1) / (visits + epsilon)  ) +
          (RNG.nextDouble * epsilon)
        }
      }
      
      def backpropagate(bpValue: Double): Unit = {
        this.visits += 1
        this.value += bpValue 
      }
      
      def expand: Unit = {
        val actions = this.gameState.leftmostLabels.get
        children = actions.map( a => new GameNode( this.gameState.applyLabel(a).get ) )
      }
      
    }
    
    def iterate(target: Sort): Option[grammar.SyntaxTree] = {
      val gameNode: GameNode = new GameNode( MissingLeaf(target) )
      var (best,fbest) = gameNode.selectAction
      for( i <- 1 to _iterations ) {
        val (result, fresult) = gameNode.selectAction
        if (fresult > fbest) {
          best = result
          fbest = fresult
        }
      }
      Some(best)
    }
  
  } // end process
  
}
