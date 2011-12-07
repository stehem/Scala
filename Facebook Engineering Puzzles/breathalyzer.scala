// compile scalac -cp scalatest-1.5.1/scalatest-1.5.1.jar breathalyzer.scala
// run scala -cp scalatest-1.5.1/scalatest-1.5.1.jar org.scalatest.tools.Runner -p . -o -s BreathalyzerTest
// http://www.facebook.com/careers/puzzles.php?puzzle_id=17


import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.io.Source
import scala.collection.mutable.ListBuffer

object Breathalyzer {

  def make_dictionary = {
    Source.fromFile("breathalyzer.txt").getLines
  }

  def get_post = {
    val lines = Source.fromFile("breath_post.txt").getLines
    var list = List[String]()
    for (line <- lines) """\w+""".r findAllIn line foreach {list :+= _}
    list
  }

  def use_dictionary = {
    //using ListBuffer is much faster
    var results = ListBuffer[Int]()
    for (word <- this.make_dictionary) {
      val score = this.compare_words("SENTENTCNES", word.trim)
      results += score.size
      if (word == "SENTENCES") println(score)
    }  
    results
  }


  def compare_words(wordz:String,word:String) = {
    var dump = List[String]()
    var count = 0
    var previous_index = 0
    if (word.diff(wordz) == word && wordz.length <= word.length){
      word.foreach(dump :+= _.toString)
    }
    else{
      wordz.foreach(letter => {
        val index = word.indexOf(letter)
        // that makes me sad
        val condition = if (count + 1 == wordz.length) {1 == 1} 
                        else {index != 0 && previous_index != -1}
        val condition2 = word.count(l => l.toString == letter) > 1 && count > index
        if (index == -1) dump :+= letter.toString
        else {
          if (wordz.length < word.length && count == 0){ 
            word.diff(wordz).foreach(dump :+= _.toString)
            if (condition && (index <= previous_index || index > count)) dump :+= letter.toString
          }
          if (wordz.length == word.length){
            if (condition && index != count) dump :+= letter.toString
          }
	  else{
            if (condition && condition2 && (index <= previous_index || previous_index == -1)) dump :+= letter.toString
	  }
        }   
      //for debugging
        println("lettre: "+letter+"   index: "+index+"   previous_index: "+previous_index+"   count: "+count)
        previous_index = index
        count += 1
      })
    } 
    dump
  }

}


class BreathalyzerTest extends FunSuite{
  trait Fixture {
  }
  //the first size assert should be the minimum number of changes
  test("avoiture"){
    val dump = Breathalyzer.compare_words("avoiture", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "a") 
  }     
  test("voiturep"){
    val dump = Breathalyzer.compare_words("voiturep", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "p") 
  }
  test("avoiturep"){
    val dump = Breathalyzer.compare_words("avoiturep", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "a") 
    assert(dump(1).toString == "p") 
  }
  test("voixture"){
    val dump = Breathalyzer.compare_words("voitxure", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "x") 
  }
  test("voture"){
    val dump = Breathalyzer.compare_words("voture", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "i") 
  }
  test("votue"){
    val dump = Breathalyzer.compare_words("votue", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "i") 
    assert(dump(1).toString == "r") 
  }
  test("voxture"){
    val dump = Breathalyzer.compare_words("voxture", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "x") 
  }
  test("voxturez"){
    val dump = Breathalyzer.compare_words("voxturez", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "x") 
    assert(dump(1).toString == "z") 
  }
  test("voxtuze"){
    val dump = Breathalyzer.compare_words("voxtuze", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "x") 
    assert(dump(1).toString == "z") 
  }
  test("vioture"){
    val dump = Breathalyzer.compare_words("vioture", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "i") 
    assert(dump(1).toString == "o") 
  }
  test("qvoitzures"){
    val dump = Breathalyzer.compare_words("qvoitzures", "voiture")
    assert(dump.size == 3)
    assert(dump(0).toString == "q") 
    assert(dump(1).toString == "z") 
    assert(dump(2).toString == "s") 
  }
  test("voiiture"){
    val dump = Breathalyzer.compare_words("voiiture", "voiture")
    assert(dump.size == 1)
    assert(dump(0).toString == "i") 
  }
  test("voiiituure"){
    val dump = Breathalyzer.compare_words("voiiituure", "voiture")
    assert(dump.size == 3)
    assert(dump(0).toString == "i") 
    assert(dump(1).toString == "i") 
    assert(dump(2).toString == "u") 
  }
  test("voatuse"){
    val dump = Breathalyzer.compare_words("voatuse", "voiture")
    assert(dump.size == 2)
    assert(dump(0).toString == "a") 
    assert(dump(1).toString == "s") 
  }
  test("erutiov"){
    val dump = Breathalyzer.compare_words("erutiov", "voiture")
    assert(dump.size == 6)
    assert(dump(0).toString == "e") 
    assert(dump(1).toString == "r") 
    assert(dump(2).toString == "u") 
    assert(dump(3).toString == "i") 
    assert(dump(4).toString == "o") 
    assert(dump(5).toString == "v") 
  }
  test("xxxxxxxxxx"){
    val dump = Breathalyzer.compare_words("xxxxxxxxxx", "voiture")
    assert(dump.size == 10)
    assert(dump(0).toString == "x") 
    assert(dump(1).toString == "x") 
    assert(dump(2).toString == "x") 
    assert(dump(3).toString == "x") 
    assert(dump(4).toString == "x") 
    assert(dump(5).toString == "x") 
    assert(dump(6).toString == "x") 
    assert(dump(7).toString == "x") 
    assert(dump(8).toString == "x") 
    assert(dump(9).toString == "x") 
  }
  test("yyy"){
    val dump = Breathalyzer.compare_words("yyy", "voiture")
    assert(dump.size == 7)
    assert(dump(0).toString == "v") 
    assert(dump(1).toString == "o") 
    assert(dump(2).toString == "i") 
    assert(dump(3).toString == "t") 
    assert(dump(4).toString == "u") 
    assert(dump(5).toString == "r") 
    assert(dump(6).toString == "e") 
  }
  test("yyyyyyy"){
    val dump = Breathalyzer.compare_words("yyyyyyy", "voiture")
    assert(dump.size == 7)
    assert(dump(0).toString == "v") 
    assert(dump(1).toString == "o") 
    assert(dump(2).toString == "i") 
    assert(dump(3).toString == "t") 
    assert(dump(4).toString == "u") 
    assert(dump(5).toString == "r") 
    assert(dump(6).toString == "e") 
  }
  test("xxxture"){
    val dump = Breathalyzer.compare_words("xxxture", "voiture")
    assert(dump.size == 3)
    assert(dump(0).toString == "x") 
    assert(dump(1).toString == "x") 
    assert(dump(2).toString == "x") 
  }
  test("zziturau"){
    val dump = Breathalyzer.compare_words("zziturau", "voiture")
    assert(dump.size == 4)
    assert(dump(0).toString == "z") 
    assert(dump(1).toString == "z") 
    assert(dump(2).toString == "a") 
    assert(dump(3).toString == "u") 
  }
  test("sententcnes"){
    val dump = Breathalyzer.compare_words("sententcnes", "sentences")
    assert(dump.size == 2)
    assert(dump(0).toString == "t") 
    assert(dump(1).toString == "n") 
  }
  test("gets the words to process form the post file"){
    val post = Breathalyzer.get_post    
    assert(post(0).toString == "tihs") 
    assert(post(1).toString == "sententcnes") 
    assert(post(2).toString == "iss") 
    assert(post(3).toString == "nout") 
    assert(post(4).toString == "varrry") 
    assert(post(5).toString == "goud") 
  }
}

