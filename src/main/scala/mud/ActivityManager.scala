package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props


class ActivityManager extends Actor{
  
  import ActivityManager._
  
  private var counter = 0
  val activityQ = new MyPQ[(Any, Int)]((t1, t2) => (t1._2 <= t2._2))
  
  def receive = {
    case ActivityManager.update =>
      counter += 1
      if(!activityQ.isEmpty){
        checkQ
      }
    case ActivityManager.queueMove => activityQ.enqueue((ActivityManager.queueMove -> (counter + 50)))
    case ActivityManager.moveNPC =>
      NpcManager.npcManager ! NPC.move
    case ActivityManager.kill(attacker, victim, damage, speed) =>
      victim ! ActivityManager.attacked(attacker)
      self ! ActivityManager.queueAttack(attacker, victim, damage, speed)
    case queueAttack(attacker, victim, damage, speed) =>
      activityQ.enqueue(ActivityManager.queueAttack(attacker, victim, damage, speed) -> (counter + (90.0/speed).toInt))
    case ActivityManager.retaliate(attacker, damage, speed) =>
      activityQ.enqueue(ActivityManager.queueAttack(sender, attacker, damage, speed) -> (counter + (90.0/speed).toInt))
    case checkAlive(attacker, defender, damage, speed) =>
      attacker ! checkAlive(attacker,defender, damage, speed)
      
  }
  
  
  
  def checkQ():Unit = {
    if(activityQ.peek._2 <= counter){
      activityQ.dequeue match{
        case (ActivityManager.queueMove, _) => self ! ActivityManager.moveNPC
        case (ActivityManager.queueAttack(attacker, victim, damage, speed), _) =>
          victim ! ActivityManager.gotHit(damage)
          attacker ! ActivityManager.hit(damage, victim)
          
          //check if alive/still here
          self ! checkAlive(attacker, victim, damage, speed)
          
        case(_,_) => Console.out.println("Big oopsies happened.")
      }
    
    if(!activityQ.isEmpty) checkQ
    }
  }
  
  
}

object ActivityManager{
  case object update
  case object moveNPC
  case object queueMove
  case class queueAttack(attacker:ActorRef, defender:ActorRef, damage:Int, speed:Int)
  case class kill(attacker:ActorRef, victim:ActorRef, damage:Int, speed:Int)
  case class attacked(attacker:ActorRef)
  case class retaliate(opponent:ActorRef, damage:Int, speed:Int)
  case class gotHit(dmg:Int)
  case class hit(dmg:Int, victim: ActorRef)
  case class checkAlive(attacker:ActorRef, victim:ActorRef, damage:Int, speed:Int)
  case class killedOpponent(victim: ActorRef)
  val activityManager = RoomManager.system.actorOf(Props[ActivityManager], "ActivityManager")
}