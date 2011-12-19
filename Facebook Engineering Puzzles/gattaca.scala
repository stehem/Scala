// compile scalac -cp scalatest-1.5.1/scalatest-1.5.1.jar breathalyzer.scala
// run scala -cp scalatest-1.5.1/scalatest-1.5.1.jar org.scalatest.tools.Runner -p . -o -s BreathalyzerTest
// http://www.facebook.com/careers/puzzles.php?puzzle_id=15


import org.scalatest.FunSuite
//import org.scalatest.BeforeAndAfter
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Sorting.stableSort

//this works by first creating gene predictions pairs i.e. for each prediction the closest next non overlapping prediction. 
//With those pairs it then creates combinations by going in both directions using the closest next combination.


object Gattaca {

  def get_length_of_dna = {
    Source.fromFile("gattaca.txt").getLines.toList.head.toInt 
  }

  def get_number_of_gene_predictions = {
    var nb = ""
    Source.fromFile("gattaca.txt").getLines.foreach(line => {
      if (line.trim.matches("""^\d+$""")) nb = line
    })
    nb.toInt
  }

  def create_list_of_gene_predictions = {
    val rgx = """^(\d+)\s+(\d+)\s+(\d+)$""".r
    var predictions = List[List[Int]]()
    Source.fromFile("gattaca.txt").getLines.foreach(line => {
      if (line.trim.matches("""^\d+\s+\d+\s+\d+$""")){
        val rgx(start, end, score) = line
        predictions :+= List[Int](start.toInt, end.toInt, score.toInt)
      }
    })
    stableSort(predictions, (e1: List[Int], e2: List[Int]) => e1(0) < e2(0)).toList
  }

  def create_gene_predictions_pairs = {
    val predictions = this.create_list_of_gene_predictions
    var pairs = List[List[List[Int]]]()
    predictions.foreach(p => {
      val before = predictions.filter(pf => pf(1) < p(0))
      val after = predictions.filter(pf => pf(0) > p(1))
      if (before.size > 0) {
        val bpair = before.last.toList :: List(p)
        pairs :+= bpair 
      }
      if (after.size > 0) {
        val apair = p :: List(after.head.toList) 
        pairs :+= apair 
      } 
    }) 
    pairs.distinct
  }

  def create_combinations_of_pairs = {
    val pairs = this.create_gene_predictions_pairs
    var combos = List[List[List[Int]]]()
    pairs.foreach(p => {
      var combo = List[List[List[Int]]]()
      combo :+= p
      while ( pairs.filter( pf => pf(1) == combo.head(0) ).size > 0 ) {
       combo +:= pairs.filter( pf => pf(1) == combo.head(0) ).head
      }     
      while ( pairs.filter( pf => pf(0) == combo.last(1) ).size > 0 ) {
       combo :+= pairs.filter( pf => pf(0) == combo.last(1) ).head
      }
      combos :+= combo.flatten.distinct
    })
    combos.distinct
  }

  def find_max_score = {
    val combos = this.create_combinations_of_pairs
    var scores = List[Int]()
    combos.foreach(c => {
      scores :+= c.foldLeft(0) {(total, n) => total + n(2)}
    })
    scores.max
  }

}


class GattacaTest extends FunSuite{
  trait Fixture {
  }
  test("correctly finds the DNA lenght"){
    assert(Gattaca.get_length_of_dna == 100)
  }     
  test("finds the number of gene predictions"){
    assert(Gattaca.get_number_of_gene_predictions == 8)
  }
  test("correctly gets gene predictions data"){
    assert((Gattaca.create_list_of_gene_predictions(0))(0) == 0)
    assert((Gattaca.create_list_of_gene_predictions(0))(1) == 9)
    assert((Gattaca.create_list_of_gene_predictions(0))(2) == 22)
    assert((Gattaca.create_list_of_gene_predictions(5))(0) == 45)
    assert((Gattaca.create_list_of_gene_predictions(5))(1) == 74)
    assert((Gattaca.create_list_of_gene_predictions(5))(2) == 26)
  }
  test("it generates the correct number of gene pairs"){
    val pairs = Gattaca.create_gene_predictions_pairs 
    assert(pairs.size == 8)
  }
  test("it generates the correct gene pairs"){
    val pairs = Gattaca.create_gene_predictions_pairs 
    assert(pairs(0) == List(List(0, 9, 22), List(10, 28, 20)))
    assert(pairs(1) == List(List(3, 18, 24), List(20, 39, 26)))
    assert(pairs(2) == List(List(10, 28, 20), List(43, 70, 27)))
    assert(pairs(3) == List(List(20, 39, 26), List(43, 70, 27)))
    assert(pairs(4) == List(List(43, 70, 27), List(78, 97, 23)))
    assert(pairs(5) == List(List(20, 39, 26), List(45, 74, 26)))
    assert(pairs(6) == List(List(45, 74, 26), List(78, 97, 23)))
    assert(pairs(7) == List(List(20, 39, 26), List(65, 99, 45)))
  }
  test("it creates the correct amount of gene combinations"){
    assert(Gattaca.create_combinations_of_pairs.size == 4)
  }
  test("it creates the right gene combinations"){
    assert(Gattaca.create_combinations_of_pairs(0) == List(List(0, 9, 22), List(10, 28, 20), List(43, 70, 27), List(78, 97, 23)))
    assert(Gattaca.create_combinations_of_pairs(1) == List(List(3, 18, 24), List(20, 39, 26), List(43, 70, 27), List(78, 97, 23)))
    assert(Gattaca.create_combinations_of_pairs(2) == List(List(3, 18, 24), List(20, 39, 26), List(45, 74, 26), List(78, 97, 23)))
    assert(Gattaca.create_combinations_of_pairs(3) == List(List(3, 18, 24), List(20, 39, 26), List(65, 99, 45)))
  }
  test("this thing works"){
    assert(Gattaca.find_max_score == 100)
  }
}

