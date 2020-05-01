import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class Pin(_name: String) {
  val colors: Map[String, String] = Map("R" -> "\u001B[31m", "G" -> "\u001B[32m", "B" -> "\u001B[34m",
    "Y" -> "\u001B[33m", "C" -> "\u001B[36m", "M" -> "\u001B[35m")
  val name: String = _name
  val color = f"${colors(name)}\u2B24${"\u001B[0m"}"
}

class Code(p1: Pin, p2: Pin, p3: Pin, p4: Pin) {
  val code: Array[Pin] = Array(p1, p2, p3, p4)
  var correctcolor = 0
  var correctpos = 0
  var guessed = false

  def print_code(): Unit = {
    println("|----|----|----|----|")
    for (pin <- code) {
      if (code.indexOf(pin) == 0) {
        printf(f"| ${pin.color} |")
      }
      else if (code.indexOf(pin)== 3) {
        printf(f" ${pin.color} |")
        for (_ <- 0 until correctcolor){
          print(f"\u2022")
        }
        for (_ <- 0 until correctpos){
          print(f"${"\u001B[31m"}\u2022${"\u001B[0m"}")
        }
        println()
      }
      else{
        printf(f" ${pin.color} |")
      }
    }
  }

  def check(original: Code): Unit={
    var used : ArrayBuffer[String]= ArrayBuffer()
    for (pin <- code){
      for (orpin <- original.code){
        if (pin.name == orpin.name && used.indexOf(pin.name) == -1){
          if (code.indexOf(pin)==original.code.indexOf(orpin)){
            correctpos += 1
          }
          else {
            correctcolor += 1
          }
          used = used :+ pin.name
        }
      }
    }
    if (correctpos == 4){
      original.guessed = true
    }
  }

}

object mastermind {
  val colorlist: Array[String] = Array("R", "G", "B", "Y", "M", "C")
  val original: Code = random_code()
  var codes: ArrayBuffer[Code] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    print_instructions()
    var chances = 10
    while(chances > 0 && !original.guessed){
      play()
      for (code <- codes){
        code.print_code()
        if (codes.indexOf(code) == codes.length-1){
          println("|----|----|----|----|")
          println()
        }
      }
      chances -= 1
    }
    if (chances == 0 && !original.guessed){
      println("Oh no, you lost! The code was:")
    }
    else{
      println("Congratulations, you guessed the code!")
    }
    original.print_code()
    println("|----|----|----|----|")
  }

  def play(): Unit ={
    val guess = user_input()
    val guesscode = new Code(new Pin(guess(0)), new Pin(guess(1)), new Pin(guess(2)), new Pin(guess(3)))
    guesscode.check(original)
    codes = codes :+ guesscode
  }

  def random_code(): Code ={
    var tempclist: ArrayBuffer[String] = ArrayBuffer()
    var codeparams: ArrayBuffer[Pin]= ArrayBuffer()
    for (i <- colorlist){
      tempclist = tempclist :+ i
    }
    for (_ <- 0 until 4){
      val rand_index = Random.nextInt(tempclist.length)
      val pin = new Pin(tempclist(rand_index))
      tempclist -= tempclist(rand_index)
      codeparams = codeparams :+ pin
    }
    val code = new Code(codeparams(0), codeparams(1), codeparams(2), codeparams(3))
    return code
  }

  def user_input(): Array[String] ={
    var valid = 4
    var input = scala.io.StdIn.readLine("Enter your guess: ").split(" ").map(_.toUpperCase)
    for (i <- input){
      if (colorlist.indexOf(i) == -1){
        valid -= 1
      }
    }
    if (valid != 4){
      input = user_input()
    }
    return input
  }

  def print_instructions(): Unit ={
    println("WELCOME TO MASTERMIND!")
    println("INSTRUCTIONS:")
    println("1. Your goal is to break the code. You have 10 chances. The code has no repeated pins.")
    println("2. There are 6 colours - red(R/r), green(G/g), blue(B/b), yellow(Y/y), cyan(C/c), magenta(M/m).")
    println("3. Input your answer as 4 space separated characters. eg. r r g y")
    println("4. The feedback is given in the form of red and white bullets.")
    println("5. Red signifies that a pin is in its correct position.")
    println("6. White signifies that a pin is in the code but not in its correct position.")
    println("7. Since the pins are checked one by one, if a pin of a certain color is repeated in the input, the feedback of only the first occurrence of that pin is counted.")
    println("8. That's all for now. Good Luck!")
    println(s"${"\u001B[1m"}GAME STARTS${"\u001B[0m"}")
    println()
  }

}
