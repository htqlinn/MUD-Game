package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.concurrent.ExecutionContext
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.PoisonPill

class PlayerManager extends Actor{
   import PlayerManager._
   import RoomManager._
   
   val startRoom:ActorRef = null
   
   val startingItems = MyBuffer(
       Item("flashlight", "Helps you see in the dark.", 10, 2),
       Item("water bottle", "Keeps you hydrated on your exciting adventures.", 8, 4))

  def receive = {
    case newPlayer(name, in, out, socket) =>
      if(context.child(name).isEmpty){
        val player = context.actorOf(Props(new Player(name, startingItems, startRoom, in, out, socket)), name)
        player ! Player.startingRoom
        out.println("Welcome, adventurer.")
      }
      else{
        out.println("Name is already in use, sorry.")
        socket.close()
      }
    case PlayerManager.update => context.children.foreach(_ ! Player.process)
    case Player.go(pName,roomRef) => context.children.filter(_.path.name != pName).foreach(_ ! Player.go(pName, roomRef))
    case Player.leave(pName,roomRef) => context.children.foreach(_ ! Player.leave(pName, roomRef))
    case Player.dies(pName, roomRef) => context.children.foreach(_ ! Player.dies(pName, roomRef))
    case NPC.dies(npcName, roomRef) => context.children.foreach(_ ! NPC.dies(npcName, roomRef))
    case Player.message(name, message, recipient, room) =>
      if(recipient == null) context.children.foreach(_ ! Player.message(name, message, recipient, room))
      else {
        if(context.child(recipient).isEmpty) sender ! Player.nonexistant(recipient)
        else context.child(recipient).get ! Player.message(name, message, recipient, room)
      }
    case Player.kill(victim, damage, speed) =>
      val tmp = context.children.find(_.path.name == victim).get
      ActivityManager.activityManager ! ActivityManager.kill(sender, tmp, damage, speed)
    case Player.exit => sender ! PoisonPill
  }
  
}

object PlayerManager {
  case class newPlayer(name:String, in:BufferedReader, out:PrintStream, socket:Socket)
  case class getRoom(ref:ActorRef)
  case object update
}