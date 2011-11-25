import scala.collection.mutable.HashMap

object Tissue {
  var cells = new HashMap[String,Cell]
  (1 to 20).foreach(j => (1 to 20).foreach(i => cells(j.toString + i.toString) = new Cell(Array(j,i)))) 
  def update = {
    for ((key, value) <- Tissue.cells) value.update
  }
}


class Cell(points:Array[Int]){
  var x = points(0)
  var y = points(1)
  var state:String = "alive"
  def neighbours = {
    var neighbours = Array[Array[Int]]()
    if (x-1 > 0 && y-1 > 0) neighbours :+= Array(x-1,y-1)
    if (y-1 > 0) neighbours :+= Array(x,y-1)
    if (x+1 < 11 && y-1 > 0) neighbours :+= Array(x+1,y-1)
    if (x-1 > 0) neighbours :+= Array(x-1,y)
    if (x+1 < 11) neighbours :+= Array(x+1,y)
    if (x-1 > 0 && y+1 < 21) neighbours :+= Array(x-1,y+1)
    if (y+1 < 11) neighbours :+= Array(x,y+1)
    if (x+1 < 11 && y+1 < 21) neighbours :+= Array(x+1,y+1)
    neighbours
  }
  def live_neighbours = {
    var alive = 0
    neighbours.foreach(n => if ( Tissue.cells(n(0).toString + n(1).toString).state == "alive" ) alive += 1 ) 
    alive
  }
  def update = {
    if (this.live_neighbours < 2 || this.live_neighbours > 3) this.state = "dead"
    else if (this.state == "dead" && this.live_neighbours == 3) this.state = "alive"
  }
}


object ProcessingTest extends processing.core.PApplet {
  import processing.core._;

  override def setup() = {
    size(400, 400)
	frameRate(1)
	loop()
  }

  override def draw() = {
	for ((key, value) <- Tissue.cells) {
		if (value.state == "alive") fill(0,192,0)
		else if (value.state == "dead") fill(255,0,0) 
		rect((value.x -1) * 20,(value.y -1)*20,20,20)
		value.update
	}	
  }

  def main(args: Array[String]) = {
    var frame = new javax.swing.JFrame("Test")
    var applet = ProcessingTest
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  }
}