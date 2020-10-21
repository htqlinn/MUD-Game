package mud

import scala.io.StdIn._
import akka.actor.ActorRef
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Actor
import java.net.Socket
import akka.actor.PoisonPill
import scala.util.Random
 
class Player(
  val name: String,
  private var inv: MyBuffer[Item],
  private var location: ActorRef,
  in:BufferedReader,
  out:PrintStream,
  socket:Socket) extends Actor{
  
  import Player._
  
  private var exits:ActorRef = null
  
  //combat stats stuff goes here
  private var hp:Int = 50
  val fists:Item = Item("fists", "You can style on other people like Connor McGregor", 5, 3)
  private var weapon:Item = fists
  private var opponent: ActorRef = null
  
  def receive = {
    case Player.startingRoom => RoomManager.roomManager ! RoomManager.returnRoom("Courtyard")
    case setRoom(room) =>
      location = room
      location ! Room.getDesc
      location ! go(name, location)
    case printMessage(string) =>
      out.println(string)
    case takeItem(itemopt) =>
      if(itemopt.isEmpty){
        out.println("Sorry, item doesn't exist.")
      }
      else{
        addToInv(itemopt.get)
        out.println("Picked up " + itemopt.get.name + ".")
      }
    case dropItem(name) => {
      if(inv.find(_.name == name).isEmpty) out.println("You don't currently have a(n) " + name + ".")
      else {
        location ! Room.dropItem(inv.find(_.name == name).get)
        inv -= inv.find(_.name == name).get
        out.println("You've dropped your " + name + ".")
      }
    }
    case takeExit(roomopt) =>
      if(roomopt.isEmpty){
        out.println("Nothing to see here.")
      }
      else if(opponent != null) out.println("You can't do that while in combat. Try fleeing instead.")
      else{
        self ! setRoom(roomopt.get)
        location ! leave(name, location)
      }
    case Player.process => if(in.ready()) processCommand(in.readLine().trim)
    case flee(dir) =>
      if(opponent == null) out.println("Who are you fleeing from? You're not in combat!")
      else{
        location ! Player.flee(dir)
      }
    case takeFleeExit(roomopt) =>
      if(roomopt.isEmpty){
        out.println("You didn't successfully flee.")
      }
      else{
        opponent ! fledMessage(self.path.name)
        opponent = null
        out.println("You successfully fled to " + roomopt.get.path.name)
        self ! setRoom(roomopt.get)
        location ! leave(name, location)
        
      }
    case fledMessage(pName) =>
      opponent = null
      out.println(pName + " has fled the battle.")
    case go(str, roomRef) =>
      if(roomRef == location){
        out.println(str + " has entered this area.")
      }
    case leave(str, roomRef) =>
      if(roomRef == location){
        out.println(str + " has left this area.")
      }
    case nonexistant(rName) => out.println("Sorry, player \"" + rName + "\" does not exist.")
    case message(name, message, recipient, room) =>
      if(room == location) out.println(name + ": " + message)
    case Player.kill(victim, damage, speed) =>
      val tmp = context.children.find(_.path.name == victim).get
      opponent = tmp
      ActivityManager.activityManager ! ActivityManager.kill(sender, tmp, damage, speed)
    case ActivityManager.attacked(attacker) =>
      out.println("You are being attacked by " + attacker.path.name)
      opponent = attacker
      sender ! ActivityManager.retaliate(attacker, weapon.damage, weapon.speed)
    case ActivityManager.gotHit(dmg) =>
      hp -= dmg
      if(hp <= 0){
        inv.filter(i => i.name != "water bottle" || i.name != "flashlight").foreach(i => dropItem(i.name))
        out.println("You were killed in battle. Big oopsies.")
        location ! dies(name, location)
        RoomManager.playerManager ! Player.exit
        socket.close()
      }
      else out.println("You were hit for " + dmg + " damage and have " + hp + "hp left.")
    case ActivityManager.hit(dmg, victim) =>
      out.println("You hit " + victim.path.name + " for " + dmg + " damage.")
    case NPC.dies(npcName, roomRef) => if(roomRef == location) out.println(npcName + " has been killed.")
    case Player.dies(pName, roomRef) => if(roomRef == location) out.println(pName + " has been killed.")
    case ActivityManager.checkAlive(attacker, defender, dmg, spd) =>
//      Console.out.println(name + " attacks" + opponent.path.name)
      if(attacker.path.name == context.self.path.name && hp > 0) defender ! ActivityManager.checkAlive(attacker, defender, dmg, spd)
      else if(defender.path.name == context.self.path.name && hp > 0) ActivityManager.activityManager ! ActivityManager.queueAttack(attacker, defender, dmg, spd)
      else opponent = null
  }
  
  def processCommand(command:String):Unit = {
    if (!command.contains(" ")){
      command.toLowerCase() match{
        case "n" | "north" => move("n")
        case "e" | "east" => move("e")
        case "s" | "south" => move("s")
        case "w" | "west"=> move("w")
        case "u" | "up" => move("u")
        case "d" | "down" => move("d")
        case "look" => location ! Room.getDesc
        case "inv" | "inventory" => {
          if (inv.nonEmpty){
            inv.foreach(i => out.println("\n- " + i.name + ": " + i.desc + "\n\tdamage: " + i.damage + " \n\tspeed: " + i.speed))
          }
          else out.println("Your inventory is currently empty.")
        }
        case "exit" => {
          if(opponent == null){
            out.println("Farewell.")
            location ! leave(name, location)
            self ! PoisonPill
            socket.close()
          }
          else out.println("You can't exit during a fight.")
        }
        case "help" =>{
          out.println("- north/east/south/west/up/down: moves your character around rooms. Abbreviations are allowed(n,e,s,w,u,d).")
          out.println("- look: gives a description of the room you are currently in.")
          out.println("- inv/inventory: checks your own inventory.")
          out.println("- get *item*: picks up the specified *item* and add it to your inventory.")
          out.println("- drop *item*: drops the specified *item* in the room you're in.")
          out.println("- equip/unqeuip: equips or unequips an item as your weapon.")
          out.println("- kill *player*: attacks the specified player.")
          out.println("- flee: lets you run away during combat.")
          out.println("- exit: exits the game.")
        }
        case "flee" => self ! Player.flee(Random.nextInt(5))
        case _ => out.println("That's not a valid command. Type \"help\" to see the full list of commands.")
      }
    }
    else{
      val input = command.split(" ", 2).map(_.trim)
      input(0).toLowerCase match{
        case "drop" => self ! dropItem(input(1).toLowerCase)
        case "get" => location ! Room.getItem(input(1).toLowerCase)
        case "say" => context.parent ! message(name, input(1).trim.toLowerCase, null, location)
        case "tell" =>
          val msg = input(1).split(" ", 2)
          context.parent ! message(name, msg(1).trim, msg(0), location)
        case "kill" =>
          if(input(1) == name) out.println("Sorry, you can't kill yourself.")
          else location ! Player.kill(input(1), weapon.damage, weapon.speed)
        case "equip" =>
          if(inv.find(_.name == input(1).toLowerCase).isEmpty) out.println("You don't have a(n) " + input(1).toLowerCase)
          else {
            weapon =inv.find(_.name == input(1).toLowerCase).get
            out.println("You've equiped your " + input(1).toLowerCase + ".")
          }
        case "unequip" =>
          if(weapon != fists){
            weapon = fists
            out.println("You've unequiped your " + input(1).toLowerCase + ".")
          }
          else out.println("You can't unequip your fists.")
          
        case _ => out.println("That's not a valid command. Type in help to see the full list of commands.")
      }
    }
  }
    
  def addToInv(item:Item):Unit = {
    inv += item
  }
  
  def move(dir:String):Unit = {
    dir match{
      case "n" => location ! Room.getExit(0)
      case "e" => location ! Room.getExit(1)
      case "s" => location ! Room.getExit(2)
      case "w" => location ! Room.getExit(3)
      case "u" => location ! Room.getExit(4)
      case "d" => location ! Room.getExit(5)
      case _ => out.println("Sorry, that's not a valid direction.")
    }
  }
}

object Player {
  case class printMessage(message:String)
  case class takeExit(optRoom:Option[ActorRef])
  case class takeFleeExit(optRoom:Option[ActorRef])
  case class takeItem(optItem:Option[Item])
  case class dropItem(Item:String)
  case class setRoom(roomRef:ActorRef)
  case class go(nameS:String, roomRef:ActorRef)
  case class leave(nameS:String, roomRef:ActorRef)
  case class message(name:String, message:String, recipient:String, room:ActorRef)
  case class nonexistant(name:String)
  case class kill(name:String, damage:Int, speed:Int)
  case class fighting(opponent: ActorRef)
  case object process
  case object startingRoom
  case class dies(name: String, ref:ActorRef)
  case object exit
  case class flee(dir:Int)
  case class fledMessage(name:String)
}

