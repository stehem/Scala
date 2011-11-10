import org.scalatest.FunSuite

class Cell(points:Array[Int]){
  var x:Int = points(0)
  var y:Int = points(1)
  var state:String = "dead"
  def neighbours = {
    var neighbours = Array[Any]()
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
}

object Tissue {
  var cells = Array[Cell]()
  // not good, instead use a hash with coordinates as key for easy retrieval
  (1 to 10).foreach(j => (1 to 10).foreach(i => cells :+= new Cell(Array(j,i)))) 
}

class GameOfLifeTest extends FunSuite{
  test("A cell has x, y coordinates") {
    var cell = new Cell(Array(3,5))
    assert(cell.x === 3)
    assert(cell.y === 5)
  }
  test("A new cell is dead by default"){
    var cell2 = new Cell(Array(2,8)) 
    assert(cell2.state === "dead")
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

}

