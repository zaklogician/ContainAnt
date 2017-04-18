package com.containant

import scala.reflect.ClassTag
import com.containant.heuristics._

class ContainAnt(heuristic: Heuristic) {
  
  def create[T](module: Module, optimizeFor: T => Double)(implicit ev: ClassTag[T]): T = {
    val grammar = new ModuleGrammar(module)
    def fitness(tree: grammar.SyntaxTree): Double = grammar.construct(tree) match {
      case (t: T) => optimizeFor(t)
      case _      => 0
    }
    val target: grammar.Sort = ev.runtimeClass
    val result: T = heuristic(grammar)( fitness, target ) match {
      case None => throw new Exception("Could not satisfy constraint")
      case Some(x) => grammar.construct(x).asInstanceOf[T]
    }
    result
  }
  
}
