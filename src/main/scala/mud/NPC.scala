package mud

import scala.util.Random
import akka.actor.Actor
import akka.actor.ActorRef

class NPC (val name:String, private var location:ActorRef) extends Actor{
  
  //combat stats here
  private var hp:Int = 20
  private var damage:Int = 5
  private var speed:Int = 5
  private var opponent:ActorRef = null
  
  val roomNames = Array[String]("Cellar", "Street", "Garage", "Courtyard", "Toolshed", "Hallway", "Bedroom", "LivingRoom", "Kitchen", "Pantry")
  
  def receive = {
    case Player.startingRoom => RoomManager.roomManager ! NPC.returnNPCRoom(roomNames(Random.nextInt(9)))
    case Player.setRoom(room) =>
      location ! Player.leave(name, location)
      location = room
      location ! NPC.NPCgo(name, location)
    case NPC.move => if(opponent == null) location ! Room.getExit(Random.nextInt(5))
    case Player.takeExit(roomopt) => 
      if(roomopt.nonEmpty){
        self ! NPC.NPCSetRoom(roomopt.get)
        location ! NPC.NPCleave(name,location) //leave message
      }
    case NPC.NPCSetRoom(room) =>
      location = room
      location ! NPC.NPCgo(name, location)
    case ActivityManager.gotHit(dmg) =>
      hp -= dmg
      if(hp <= 0){
//        sender ! ActivityManager.died
        location ! NPC.dies(name, location)
        context.stop(self)
      }
    case ActivityManager.attacked(attacker) =>
      opponent = attacker
      sender! ActivityManager.retaliate(attacker, damage, speed)
    case ActivityManager.checkAlive(attacker, defender, dmg, spd) =>
      if(attacker.path.name == context.self.path.name && hp > 0) defender ! ActivityManager.checkAlive(attacker, defender, dmg, spd)
      else if(defender.path.name == context.self.path.name && hp > 0) ActivityManager.activityManager ! ActivityManager.queueAttack(attacker, defender, dmg, spd)
      else opponent = null
  }
    
}

object NPC{
  case class NPCgo(NPCName:String, location: ActorRef)
  case class NPCleave(NPCName:String, location:ActorRef)
  case class returnNPCRoom(loc: String)
  case object move
  case class NPCSetRoom(roomRef: ActorRef)
  case object checkLoc
  case class dies(name: String, ref:ActorRef)
}