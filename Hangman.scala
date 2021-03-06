import scala.io.Source
import scala.util.Random

object Hangman {
  var word: String = ""
  var hiddenWord :Array[String] = Array()
  var gameOver = false
  var chances = 6

  def main(args: Array[String]): Unit = {
    print_instructions()
    chooseWord()
    for (_ <- 0 until word.length){hiddenWord = hiddenWord :+ "_"}
    drawfig()
    print_word()
    play()
    endGame()
  }

  def play(): Unit ={
    while (!gameOver){
      var input = scala.io.StdIn.readLine("GUESS A LETTER: ")
      var guesses = findinword(input)
      for (i<- guesses){hiddenWord(i) =  word.charAt(i).toString}
      print_word()
      drawfig()
      isGameOver()
    }
  }

  def chooseWord(): Unit ={
    var wordlist:Array[String] = Array()
    val rawwordlist = Source.fromFile("/Users/sureshp/Downloads/hangman_words.txt")
    for(wrd<-rawwordlist.getLines()){wordlist = wordlist :+ wrd}
    rawwordlist.close()
    word = wordlist(Random.nextInt(wordlist.length)).toUpperCase
  }

  def print_word(): Unit ={
    for (char <- hiddenWord){
      print(char+" ")
    }
    println()
    println(s"You have $chances wrong guesses left.")
    println()
  }

  def findinword(input: String): Array[Int] ={
    var indices: Array[Int] = Array()
    for (i <- 0 until word.length){
      if (word(i).toString == input){indices = indices :+ i}
    }
    if (indices.isEmpty){chances = chances - 1; println(s"Sorry, $input is not in the word!") }
    return indices
  }

  def isGameOver(): Unit ={
    var freechars = 0
    if (chances == 0){gameOver = true}
    for (char <- hiddenWord){
      if (char == "_"){freechars += 1}
    }
    if (freechars == 0){gameOver = true}
  }

  def endGame(): Unit ={
    println("GAME OVER")
    if (chances == 0){println(s"Better luck next time! The word was '$word'")}
    else{println("Congratulations, you have guessed the right word!")}
  }

  def print_instructions(): Unit ={
    println("WELCOME TO HANGMAN!")
    println("  INSTRUCTIONS:")
    println("-> The goal of the game is to guess the word by guessing alphabets one at a time.")
    println("-> Make sure you type in your guesses one at a time.")
    println("-> eg. GUESS A LETTER: E")
    println("-> DONOT guess the same letter again if you had previously gotten it wrong. You will lose a chance both times.")
    println("-> If you guess for a second time a letter you had previously gotten right, don't worry, you will not lose a chance.")
    println("-> Always keep Caps Lock ON.")
    println("-> That's pretty much it. Enjoy!")
    println()
  }

  def drawfig(): Unit ={
    if (chances == 6){
      println("|---------|")
      println("|         ")
      println("|         ")
      println("|        ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances == 5){
      println("|---------|")
      println("|         O")
      println("|         ")
      println("|        ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances == 4){
      println("|---------|")
      println("|         O")
      println("|        / ")
      println("|        ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances == 3){
      println("|---------|")
      println("|         O")
      println("|        /| ")
      println("|        ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances==2){
      println("|---------|")
      println("|         O")
      println("|        /|\\ ")
      println("|        ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances==1){
      println("|---------|")
      println("|         O")
      println("|        /|\\ ")
      println("|        / ")
      println("|           ")
      println("|")
      println("------------")
    }
    else if (chances == 0){
      println("|---------|")
      println("|         O")
      println("|        /|\\ ")
      println("|        / \\")
      println("|           ")
      println("|")
      println("------------")
    }
  }
}
