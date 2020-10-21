package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.util.Random
import akka.actor.Props
import scala.concurrent.ExecutionContext

class NpcManager extends Actor{
  
  val startRoom:ActorRef = null
    
  val name = Array[String]("ghost", "scarecrow", "witch", "bat", "rat", "vampire", "demon", "fairy", "cat", "crow")
  
  def receive = {
    case NpcManager.newNPC =>
      for(i <- 0 to name.length - 1){
        val npc = context.actorOf(Props(new NPC(name(i), startRoom)), name(i))
        npc ! Player.startingRoom
      }
//      Console.out.println("NPC " + cnt + " created")
    case NPC.move =>
      context.children.foreach(_ ! NPC.move)
    case Player.kill(victim, damage, speed) =>
      val tmp = context.children.find(_.path.name == victim).get
      ActivityManager.activityManager ! ActivityManager.kill(sender, tmp, damage, speed)
  }

  
}

object NpcManager{
  case object newNPC
  val npcManager = RoomManager.system.actorOf(Props[NpcManager], "NPCManager")
}