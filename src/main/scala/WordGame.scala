package cntdn

import io.Source
import scalaz._, Scalaz._, effect._

object WordGame extends App {
	
	// Create the unsafe IO operation that will load the words
	val loadDictionaryAction = IO { 
		Source.fromFile("/Users/James/Desktop/cntdn/src/main/resources/dictionary.txt").getLines.toStream 
	}

	// Change the world by loading the dictionary!
	val words = loadDictionaryAction.unsafePerformIO.toList

	// Characters we can use - first half of the alphabet for a test!
	val testString = ('a' |-> 'm') mkString

	// Can a word s be made using the testString characters tstr?
	def canBeMade(s: List[Char], tstr: String = testString) : Boolean = s match {
		case Nil 		=> true
		case h :: t => tstr.contains(h) ? canBeMade(t, tstr diff h.point[List]) | false
	}
	
	// Make a list of words than can be made with these characters
	val canMake = words filter { word => canBeMade(word.toList) } println
}
