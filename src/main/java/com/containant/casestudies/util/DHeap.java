package com.containant.casestudies.util;

// This is the DHeap Programming by Optimization example by Hoos-Hsu

//-------------------------------------------------------
// Modified by Quinn Hsu
// Demonstrates how PbO can be used to expose optimization
//		parameters.  
// In this case
//		##default_arity
//		initial_size
//		expansion_factor
//
// Version 1.0
//-------------------------------------------------------

public class DHeap {
	private int[] heap;
	private int size;
	
  private int arity;
  private int expansion_factor;
  private int initial_size;
  
  // # of accesses for the fitness function
  private int access_count = 0;
	
	public DHeap(int arity, int expansion_factor, int initial_size) {
		this.arity = arity;
		this.expansion_factor = expansion_factor;
    this.initial_size = initial_size;
    this.heap = new int[initial_size];
		this.size = 0;
	}
	
	public boolean isEmpty() {
		return size == 0;
	}

	public int findMin() {
		if (size == 0) { throw new java.lang.IllegalStateException("Empty Heap"); }
		
		return heap[0];
	}
	
	public void insert(int toBeInserted) {
		if (size==0) {
      access_count++;
			heap[0] = toBeInserted;
			size++;
			return;
		}
		
		// Check to see if the heap can hold another element
		// If not, expand the array
		
		if (size == heap.length) {
			int[] newArray = new int[size*expansion_factor];
			for(int i=0; i<size; i++) {
        access_count++;
				newArray[i] = heap[i];
			}
			
			heap = newArray;
		}
		
		
		// Start at the bottom, and search to find where
		// we should insert the new element
		//
		// Stop when we find a parent node that is small
		// than the element to be inserted, or we reach the top
		
		int i = size;
		
		for(; heap[(i-1)/arity] > toBeInserted; i=(i-1)/arity) {
			// if we've reached the top, do the assignment
			if (i==0) break;
			
			// otherwise swap down the parent
			access_count++;
      heap[i] = heap[(i-1)/arity];
		}
		access_count++;
		heap[i] = toBeInserted;
		size++;
		
	}
	
	public int deleteMin() {
		if (size == 0) { throw new java.lang.IllegalStateException("Empty Heap"); }
		
		int toReturn = heap[0];
		
		int lastElement = heap[size-1];
		
		int minChild;
		
		int i=0;
		
		for(; (i*arity)+1 < size; i=minChild) {
			// Assume initially that the smallest child is
			// the first child
			minChild = (i*arity)+1;
			
			// There are no children for this node
			if (minChild > size) { break; }
			
			// Search through all the children for the
			// smallest value
			int j=1, currentSmallestChild = minChild;
			for(; j<arity; j++) {
				if (minChild+j == size) break;
        access_count++;
        access_count++;
				if(heap[currentSmallestChild] > heap[minChild+j])
					currentSmallestChild = minChild+j;
			}
			
			minChild = currentSmallestChild;
			
			// if the minChild that we found is smaller
			// than the last element, we should percolate
			// up the child to the parent and keep searching
			// for a suitable place to put the last element
			if (lastElement > heap[minChild]) {
        access_count++;
				heap[i] = heap[minChild];
			} else {
				break;
			}
		}
		access_count++;
		heap[i] = lastElement;
		size--;
		return toReturn;
	}
	
  
  // A trivial modification to the main function, measures execution time
	public int measureAccess() {
		DHeap heap = this;
    heap.access_count = 0;
		int i, j;
		int maxSize = 1000;
		for(i=0, j=maxSize/2; i<maxSize; i++, j=(j+71)%maxSize) {
			heap.insert(j);
		}
		for(j=0; j<maxSize; j++) {
			if (heap.deleteMin() != j) {
				System.out.println("Error in deleteMin: "+j);
			}
		}
		// System.out.println("Done...");
    return heap.access_count;
	}
  
  public long measureTime() {
    long start = System.currentTimeMillis();
		DHeap heap = this;
		int i, j;
		int maxSize = 10000;
		for(i=0, j=maxSize/2; i<maxSize; i++, j=(j+71)%maxSize) {
			heap.insert(j);
		}
		for(j=0; j<maxSize; j++) {
			if (heap.deleteMin() != j) {
				System.out.println("Error in deleteMin: "+j);
			}
		}
		// System.out.println("Done...");
    long elapsed = System.currentTimeMillis() - start;
    return elapsed;
	}
  
  @Override public String toString() {
    return "a" + arity + " e" + expansion_factor + " i" + initial_size;
  }

}
