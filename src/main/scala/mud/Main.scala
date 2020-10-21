package mud

import scala.io.StdIn._
import akka.actor.Props
import akka.actor.ActorSystem
import scala.concurrent.Future
import scala.concurrent.duration._
import java.net.ServerSocket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader


object Main {
	def main(args: Array[String]): Unit = {
	  
    val system = RoomManager.system
    val roomManager = RoomManager.roomManager
    val npcManager = NpcManager.npcManager
    val playerManager = RoomManager.playerManager
    val activityManager = ActivityManager.activityManager
    
    npcManager ! NpcManager.newNPC
    
    implicit val ec = system.dispatcher
    system.scheduler.schedule(0.seconds, 100.millis, playerManager, PlayerManager.update)
    
    val ss = new ServerSocket(4030)
    
    system.scheduler.schedule(0.1.seconds, 10.seconds, activityManager, ActivityManager.queueMove)
    system.scheduler.schedule(0.1.seconds, 100.millis, activityManager, ActivityManager.update)
  
    while(true){
      val playerSocket = ss.accept()
      val in = new BufferedReader(new InputStreamReader(playerSocket.getInputStream))
      val out = new PrintStream(playerSocket.getOutputStream)
  
		  out.print("Welcome, adventurer.\nPlease enter your name: ")
		  Future{
		    val name = in.readLine()
		    playerManager ! PlayerManager.newPlayer(name, in, out, playerSocket)
      }
    }
  }
}