// compile scalac -cp scalatest-1.5.1/scalatest-1.5.1.jar liarliar.scala
// run scala -cp scalatest-1.5.1/scalatest-1.5.1.jar org.scalatest.tools.Runner -p . -o -s LiarLiarTest
// http://www.facebook.com/careers/puzzles.php?puzzle_id=20

// the basis for the whole thing is that since they all either lie or tell the truth, and 
// given that the answers might be incomplete, the program just looks for the name that appears
// the most as a way to figure out the bigger group...

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.io.Source

object Liar {

  def get_lines = {
    Source.fromFile("liarliar.txt").getLines
  }

  def find_number_of_members = {
    get_lines.toArray.head.toInt 
  }

  def create_array_of_answers(lines:Iterator[String]) = {
    var answers = Array[Array[String]]()
    for (line <- lines) {
      if (line.matches("\\w.*\\d")) {
        answers :+= Array[String]()
      }	
      else {
        val i = answers.indices.last
        if (i != -1) answers(i) :+= line
      }
    }
    answers
  }
  
  def create_deduped_flat_array(answers:Array[Array[String]]) = {
    val flat_answers = answers.flatMap {
      case arr: Array[String] => arr
      case y => y
    }
    flat_answers.distinct
  }
  
  def create_map_of_scores = {
    val answers = this.create_array_of_answers(this.get_lines)
    val flat_answers = this.create_deduped_flat_array(answers)
    val scores = scala.collection.mutable.Map[String,Int]()
    flat_answers.foreach(flat => scores(flat) = 0)
    flat_answers.foreach(flat => answers.foreach(ans => if (ans.contains(flat)) scores(flat) += 1 ))
    scores
  }
  
  def find_max_score = {
    var list = List[Int]()
    for ((key, value) <- create_map_of_scores) list :+= value
    list.max
  }

  def liar = {
    List(find_max_score, find_number_of_members - find_max_score)
  }

 
}


class LiarLiarTest extends FunSuite{
  trait Fixture {
    val lines = Liar.get_lines
  }
  test("counts the file's number of lines"){
    new Fixture {
      val count = lines.length
      assert(count == 16) 
    }
  }     
  test("counts the number of vet members"){
    new Fixture {
      val count = Liar.find_number_of_members
      assert(count == 6) 
    }
  }     
  test("the answers array contains as many arrays as there are vets"){
    new Fixture {
      val length = Liar.create_array_of_answers(lines).length
      assert(length == 6)
    }
  }
  test("each member of the answers array contains the correct number of members"){
    new Fixture {
      val answers = Liar.create_array_of_answers(lines)
      assert(answers(0).length == 1)
      assert(answers(1).length == 1)
      assert(answers(2).length == 1)
      assert(answers(3).length == 1)
      assert(answers(4).length == 2)
      assert(answers(5).length == 3)
    }
  }
  test("each member of the answers array contains the correct members"){
    new Fixture {
      val answers = Liar.create_array_of_answers(lines)
        assert(answers(0)(0) == "Tommaso")
        assert(answers(1)(0) == "Galileo")
        assert(answers(2)(0) == "Tommaso")
        assert(answers(3)(0) == "Tommaso")
        assert(answers(4)(0) == "Isaac")
        assert(answers(4)(1) == "Stephen")
        assert(answers(5)(0) == "Lili")
        assert(answers(5)(1) == "Lolo")
        assert(answers(5)(2) == "Lulu")
    }
  }
  test("it creates a deduped flat array of all answers"){
    new Fixture {
      val answers = Liar.create_array_of_answers(lines)
      val flat_answers = Liar.create_deduped_flat_array(answers)
      assert(flat_answers.length == 7)
      assert(flat_answers(0) == "Tommaso")
      assert(flat_answers(3) == "Stephen")
    }
  }
  test("counts the number of occurences for each vet"){
    new Fixture {
      val scores = Liar.create_map_of_scores
      assert(scores("Tommaso") == 3)
      assert(scores("Isaac") == 1)
      assert(scores("Galileo") == 1)
    }
  }
  test("finds the correct max score"){
    new Fixture {
      val max_score = Liar.find_max_score
      assert(max_score == 3)
    }
   } 
   test("this thing works"){
    new Fixture {
      assert(Liar.liar(0) == 3)
      assert(Liar.liar(1) == 3)
    }
  }



}

