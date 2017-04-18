package com.containant.heuristics

import com.containant.LBNF

trait Heuristic {
  def apply(grammar: LBNF)(
    fitness: grammar.SyntaxTree => Double,
    target: grammar.Sort 
  ): Option[grammar.SyntaxTree]
}
