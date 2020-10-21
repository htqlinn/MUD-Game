package mud

import scala.collection.mutable

class MyBuffer[A] extends mutable.Buffer[A]{
  
  private var default:A = _
  private class Node(var data: A, var prev:Node, var next: Node)
  private val end: Node = new Node(default, null, null)
  end.prev = end
  end.next = end
  private var numElems = 0
  
  def +=(elem:A):MyBuffer.this.type = {
    val newnode = new Node(elem, end.prev, end)
    end.prev.next = newnode
    end.prev = newnode
    numElems += 1
    this
  }
  
  def +=:(elem:A):MyBuffer.this.type = {
    val newnode = new Node(elem, end, end.next)
    end.next.prev = newnode
    end.next = newnode
    numElems += 1
    this
  }
    
  def apply(n:Int):A = {
    require(n >= 0 && n < numElems)
    var rover = end.next
    for(i <- 1 to n) rover = rover.next
    rover.data
  }
  
  def clear():Unit = {
    end.next = end
    end.prev = end
    numElems = 0
  }
  
  def insertAll(n:Int, elems: collection.Traversable[A]):Unit = {
    require(n >= 0 && n < numElems + 1)
    if(elems.nonEmpty) {
      var rover = end.next
      for(i <- 0 until n) rover = rover.next
      for(e <- elems) {
        val newnode = new Node(e, rover.prev, rover)
        rover.prev.next = newnode
        rover.prev = newnode
        numElems += 1
      }
    }
  }
  
  def update(n:Int, newelem:A):Unit = {
    require(n >= 0 && n < numElems)
    var rover = end.next
    for(i <- 1 to n) rover = rover.next
    rover.data = newelem
  }
  
  def remove(n:Int): A = {
    require(n >= 0 && n < numElems)
    numElems -= 1
    var rover = end.next
    for(i <- 0 until n) rover = rover.next
    val ret = rover.data
    rover.prev.next = rover.next
    rover.next.prev = rover.prev
    ret
  }
  
  def iterator: Iterator[A] = new Iterator[A]{
    var rover = end.next
    def hasNext:Boolean = rover != end
    def next():A = {
      val ret = rover.data
      rover = rover.next
      ret
    }
  }
  
  def length:Int = numElems
}

object MyBuffer{
  def apply[A](elems: A*): MyBuffer[A] = {
    val ret =  new MyBuffer[A]
    if(elems.isEmpty) ret
    else{
      elems.foreach(ret += _)
      ret
    }
  }
}