package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.ActorSystem

class RoomManager extends Actor{
  import RoomManager._
  
  val rooms = readRooms()
  for(room <- context.children) room ! Room.linkExits(rooms)
  
  def receive = {
//    case m => println()
    case returnRoom(room) =>
      sender ! Player.setRoom(rooms(room))
    case NPC.returnNPCRoom(room) =>
      sender ! NPC.NPCSetRoom(rooms(room))
      
  }
  
  def readRooms(): Map[String,ActorRef] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val roomMap = Array.fill(lines.next.trim.toInt)(parseRoom(lines)).toMap
    source.close()
    roomMap
  }
  
    def parseRoom(lines: Iterator[String]): (String, ActorRef) = {
    val keyword = lines.next
    val name = lines.next
    val desc = lines.next
    val items = {
      val tmp = MyBuffer[Item]()
      for(i <- 1 to lines.next.trim.toInt){
        tmp += {Item(lines.next, lines.next, lines.next.toInt, lines.next.toInt)}
      }
      tmp
    }
    val exits = lines.next.split(",").map(_.trim)
    keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
  }
    
}


object RoomManager {
  val system = ActorSystem("System")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
  val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  case object Start
  case class returnRoom(s:String)
}