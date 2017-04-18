package com.containant.casestudies.util

case class Prob(values: List[Int]) {
  def apply(backlevel: Int): Double = if (backlevel == 0) 1 else if (values.length > backlevel) values(backlevel) else 0
}

class SkipList(val bottom: Int, prob: Prob) {
  val rng: java.util.Random = new java.util.Random(0xDEADBEEF)
  
  override def toString: String = "SkipList with " + (bottom+1) + " levels,  prob: " + (prob.values.take(bottom+1) mkString ",")
  
  def printContents: Unit = {
    for (level <- 0 to bottom) {
      printf("Level %d: ",level)
      var current = head
      while( current.next(level) != null ) {
        printf("%d ", current.value)
        current = current.next(level)
      }
      println(current.value)
    }
  }
  
  
  class SkipNode() {
    var value: Int = Integer.MIN_VALUE
    var next: Array[SkipNode] = Array.ofDim(bottom+1)
    for( x <- 0 until next.length ) next.update(x, null)
  }
  
  var head: SkipNode = new SkipNode()
  
  def insert(value: Int): Unit = {
    var level: Int = 0
    var lastVisited: Array[SkipNode] = Array.ofDim(bottom+1)
    for( x <- 1 until lastVisited.length ) lastVisited(x) = head
    var current: SkipNode = head
    
    
    // find insertion point
    while(level < bottom) {
      // advance to insertion point
      while( current.next(level) != null && current.next(level).value < value ) {
        current = current.next(level)
      }
      // move down
      lastVisited.update(level, current)
      level = level + 1
    }
    while( current.next(level) != null && current.next(level).value < value ) {
      current = current.next(level)
    }
    lastVisited.update(level, current)
    
    // if the value is indeed new
    if (current.next(level) == null || value != current.next(level).value) {
      // create new node
      val newNode = new SkipNode()
      newNode.value = value
      
      // update links
      var backlevel = 0
      val threshold = rng.nextDouble()
      while (backlevel <= bottom && threshold < 1/prob(backlevel)) {
        current = lastVisited(level)
        newNode.next.update(level, current.next(level))
        current.next.update(level, newNode)
        backlevel = backlevel + 1
        level = level - 1
      }
      
      // end insert
    }
  }
  
  
  def find(value: Int): Boolean = {
    var level: Int = 0
    var current: SkipNode = head
    while(level < bottom) {
      // advance to detection point
      while( current.next(level) != null && current.value < value ) {
        current = current.next(level)
      }
      // move down a level
      level = level + 1
    }
    current.value == value
  }
  
  def findTime(value: Int): Int = {
    var iterations = 0
    var level: Int = 0
    var current: SkipNode = head
    while(level < bottom) {
      // advance to detection point
      while( current.next(level) != null && current.value < value ) {
        current = current.next(level)
        iterations = iterations + 1
      }
      // move down a level
      iterations = iterations + 1
      level = level + 1
    }
    iterations
  }
  
}
