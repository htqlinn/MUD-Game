package mud

import akka.actor.Actor
import akka.actor.ActorRef

class Room(
    val name: String,
    val desc: String,
    private var _items: MyBuffer[Item],
    private val _exits: Array[String]) extends Actor{
    import Room._
  
    private var exit: Array[Option[ActorRef]] = null
    val playersHere: MyBuffer[String] = MyBuffer[String]()
    val NPCsHere: MyBuffer[String] = MyBuffer[String]()
    
    def receive = {
      case linkExits(roomsMap) =>
        exit = _exits.map(key => roomsMap.get(key))
      case Room.getDesc =>
        sender ! Player.printMessage(description())
      case getExit(dir) =>
        sender ! Player.takeExit(getExit(dir))
      case Player.flee(dir) =>
        sender ! Player.takeFleeExit(getExit(dir))
      case getItem(itemName) =>
        sender ! Player.takeItem(getItem(itemName))
        if(!getItem(itemName).isEmpty){
          _items -= getItem(itemName).get
        }
      case dropItem(item) =>
        addToRoom(item)
      case Player.go(pName, roomRef) =>
        playersHere += pName
        RoomManager.playerManager ! Player.go(pName, roomRef)
      case Player.leave(pName, roomRef) =>
        playersHere -= pName
        RoomManager.playerManager ! Player.leave(pName, roomRef)
      case NPC.NPCleave(nName, roomRef) => 
        NPCsHere -= nName
        RoomManager.playerManager ! Player.leave(nName, roomRef)
      case NPC.NPCgo(nName, roomRef) =>
        NPCsHere += nName
        RoomManager.playerManager ! Player.go(nName, roomRef)
      case Player.kill(victim, damage, speed) =>
        if(playersHere.contains(victim)){
          RoomManager.playerManager forward Player.kill(victim, damage, speed)
        }
        else if(NPCsHere.contains(victim)){
          NpcManager.npcManager forward Player.kill(victim, damage, speed)
        }
        else sender ! Player.nonexistant(victim)
      case Player.dies(pName, roomRef) =>
        playersHere -= pName
        RoomManager.playerManager ! Player.dies(pName, roomRef)
      case NPC.dies(nName, roomRef) => 
        NPCsHere -= nName
        RoomManager.playerManager ! NPC.dies(nName, roomRef)
        
      case m => println("Big oopsies happened :(")
    }
    
  def description(): String = {
    val itemString ={
      for(i <- _items.toArray) yield {
        "- " + i.name + ": " + i.desc + "\n\tdamage: " + i.damage + "\n\tspeed: " + i.speed + "\n"
      }
    }
    
    val exitString:String = {
      var tmp = "\n"
        if(_exits(0) != "no") tmp += "- North\n" else()
        if(_exits(1) != "no") tmp += "- East\n" else()
        if(_exits(2) != "no") tmp += "- South\n" else()
        if(_exits(3) != "no") tmp += "- West\n" else()
        if(_exits(4) != "no") tmp += "- Up\n" else()
        if(_exits(5) != "no") tmp += "- Down\n" else()
        if (tmp.length == 0) "None"
        tmp
      }
    
    "\n" + name + "\n" + desc + "\nExit(s): " + exitString +"Item(s):\n" + itemString.mkString + "Player(s): " + playersHere.mkString(", ") + "\nNPCs: " + NPCsHere.mkString(", ")
  }
  
  def getExit(dir: Int): Option[ActorRef] = {
    exit(dir)
  }
  
  def getItem(itemName: String): Option[Item] = {
    _items.find(_.name == itemName)
  }
  
  def addToRoom(item: Item): Unit = {
    _items += item
  }
  
}

object Room {
  case class linkExits(roomsMap:Map[String, ActorRef])
  case object getDesc
  case class getExit(exitNum:Int)
  case class getItem(itemName:String)
  case class dropItem(item:Item)

}
