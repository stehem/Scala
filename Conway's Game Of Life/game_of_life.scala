import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.HashMap

object Tissue {
  var cells = new HashMap[String,Cell]
  (1 to 10).foreach(j => (1 to 10).foreach(i => cells(j.toString + i.toString) = new Cell(Array(j,i)))) 
  def update = {
    for ((key, value) <- Tissue.cells) value.update
    draw
  }
  def draw = {
    var life = ""
    var count = 1
    for ((key, value) <- Tissue.cells) {
      if (value.state == "alive"){life += "0 "}
      else if (value.state == "dead"){life += "X "}
      if (count % 10 == 0){life += "\n"} 
      if (count == 100){life += "\n"} 
      count += 1
    } 
    print(life)
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
    if (x-1 > 0 && y+1 < 11) neighbours :+= Array(x-1,y+1)
    if (y+1 < 11) neighbours :+= Array(x,y+1)
    if (x+1 < 11 && y+1 < 11) neighbours :+= Array(x+1,y+1)
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



class GameOfLifeTest extends FunSuite with BeforeAndAfter{
  trait Fixture {
    val rnd = new scala.util.Random
    val range = 1 to 10
    val x = range(rnd.nextInt(range length))
    val y = range(rnd.nextInt(range length))
    var c = new Cell(Array(x,y)) 
    var nbn = c.neighbours.size
  }
  before {
    for ((key, value) <- Tissue.cells) value.state= "alive"
  }
  test("A cell has x, y coordinates") {
    var cell = new Cell(Array(3,5))
    assert(cell.x === 3)
    assert(cell.y === 5)
  }
  test("A new cell is alive by default"){
    var cell2 = new Cell(Array(2,8)) 
    assert(cell2.state === "alive")
  }
  test("A new tissue contains 100 cells"){
    assert(Tissue.cells.size === 100) 
  }
  test("A cell in a corner has 3 neighbours"){
    var c = new Cell(Array(1,1))
    assert(c.neighbours.size === 3)
  }
  test("A cell in the middle has 8 neighbours"){
    var c = new Cell(Array(5,5))
    assert(c.neighbours.size === 8)
  }
  test("A cell on the side has 5 neighbours"){
    var c = new Cell(Array(1,5))
    assert(c.neighbours.size === 5)
  }
  test("Counts live neighbours correctly"){
  new Fixture{
    (0 to 2).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "dead"
    )
    assert(c.live_neighbours === (nbn - 3))
  }
  }
  test("Any live cell with fewer than two live neighbours dies"){
  new Fixture{
    (0 to (nbn-2)).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "dead"
    )
    c.update
    assert(c.state === "dead")
  }
  }
  test("Any live cell with more than three live neighbours dies"){
  new Fixture{
    c.update
    if (nbn > 3)
      assert(c.state === "dead")
    else
      assert(c.state === "alive")
  }
  }
  test("Any live cell with two or three live neighbours lives, unchanged, to the next generation."){
  new Fixture{
    (0 to (nbn-1)).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "dead"
    )
    (0 to 1).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "alive"
    )
    c.update
    assert(c.state === "alive")
    (0 to (nbn-1)).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "dead"
    )
    (0 to 2).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "alive"
    )
    c.update
    assert(c.state === "alive")
  }
  }
  test("Any dead cell with exactly three live neighbours becomes a live cell"){
  new Fixture{
    c.state = "dead"
    (0 to (nbn-1)).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "dead"
    )
    (0 to 2).foreach(
      n => Tissue.cells((c.neighbours(n))(0).toString + (c.neighbours(n))(1).toString).state = "alive"
    )
    c.update
    assert(c.state === "alive")
  }
  }
  test("It prints to the console"){
    // prints the first 3 gens
    //TODO update a single rendering
    Tissue.update
    Tissue.update
    Tissue.update
  }
}

