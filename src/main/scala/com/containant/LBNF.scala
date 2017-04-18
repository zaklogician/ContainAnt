package com.containant


// Dictionary:
//  LBNF  | Type Theory | Programming
// -----------------------------------------
//  Sort  | Type        | Scala class
//  Label | Constructor | Provider function

/** A grammar in Labeled Backus-Naur form. */
trait LBNF {
  
  /** The type of sorts (i.e. left-hand sides) of grammar rules. */
  type Sort
  
  /** The type of unique labels of grammar rules.*/
  type Label
  
  /** A syntax tree in the grammar.
   *  
   * @param label The rule used to construct the tree.
   * @param subtrees The subtrees containing the rule's arguments.
   */
  case class SyntaxTree(label: Label, subtrees: Seq[SyntaxTree])
  
  /** The sort associated with the given label. */
  def sort(l: Label): Sort
  
  /** The finite list of all sorts in the grammar. */
  def sorts: Seq[Sort]
  
  /** Returns all labels of the grammar compatible with the given sort.
   */
  def labels(c: Sort): Seq[Label]
  
  /** Returns the body (arguments) of the rule with the given label. */
  def rule(label: Label): Seq[Sort]
}
