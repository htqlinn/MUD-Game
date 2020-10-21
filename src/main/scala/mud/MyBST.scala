package mud

import scala.collection.mutable
import scala.collection.mutable.ArrayStack

class MyBST [K,V](comp:(K,K) => Int) extends mutable.Map[K, V]{
  class Node(var key:K, var value:V, var left:Node, var right:Node)
  
  private var root:Node = null
  
  def +=(kv:(K,V)) = {
    val (key, value) = kv
    def recur(n:Node):Node = {
      if(n == null) new Node(key, value, null, null)
      else{
        val c = comp(key, n.key)
        if(c == 0){
          n.value = value
        }
        else if(c < 0){
          n.left = recur(n.left)
        }
        else{
          n.right = recur(n.right)
        }
        n
      }
    }
    root = recur(root)
    this
  }
  
  def -=(key:K) = {
   def recur(n:Node):Node = {
      if(n == null) null
      else{
        val c = comp(key, n.key)
        if(c == 0){
          if(n.left == null) n.right
          else if(n.right == null) n.left
          else{
            val (k, v, node) = removeMax(n.left)
            n.left = node
            n.key = k
            n.value = v
            n
          }
        }
        else if(c < 0){
          n.left = recur(n.left)
          n
        }
        else{
          n.right = recur(n.right)
          n
        }
      }
    }
    def removeMax(n:Node):(K,V,Node) = {
      if(n.right == null){
        (n.key, n.value, n.left)
      }
      else{
        val (k, v, node) = removeMax(n.right)
        n.right = node
        (k, v, n)
      }
    }
    root = recur(root)
    this
  }
  
  def get(key:K) = {
    var rover = root
    var c = if(rover != null) comp(key, rover.key) else 0
    while(rover != null && c != 0){
      rover = if(c < 0) rover.left else rover.right
      c = if(rover != null) comp(key, rover.key) else 0
    }
    if(rover == null) None else Some(rover.value)
    None
  }
  
  def iterator = new Iterator[(K,V)] {
    val stack = new ArrayStack[Node]
    pushAllLeft(root)
    
    def next:(K,V) = {
      val ret = stack.pop()
      pushAllLeft(ret.right)
      (ret.key -> ret.value)
    }
    
    def hasNext = !stack.isEmpty
    
    def pushAllLeft(n:Node){
      if(n != null) stack.push(n)
      pushAllLeft(n.left)
    }
  }
  
  private def inorder(visit:V => Unit){
    def recur(n:Node){
      if(n != null){
        recur(n.left)
        visit(n.value)
        recur(n.right)
      }
    }
    recur(root)
  }
  
}