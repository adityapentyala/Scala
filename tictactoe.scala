object tictactoe {

  var whoseTurn = "O"
  var winner = " "
  var gameOver = false
  var grid: Array[Array[String]] = Array(Array(" ", " ", " "),
                                         Array(" ", " ", " "),
                                         Array(" ", " ", " ") )

  def main(args: Array[String]): Unit ={
    print_instructions()

    while(!gameOver){
      print_grid(grid)
      player_move(whoseTurn, grid)
    }

  }

  def print_instructions(): Unit ={
    println("INSTRUCTIONS:")
    println("The grid is of the form")
    println("|-----|-----|-----|")
    for (i <- 0 to 2; j <- 0 to 2){
      if (j == 0){
        print("|")
      }
      print(s"($i,$j)")
      if (j == 2){
        println("|")
        println("|-----|-----|-----|")
      }
      else{
        print("|")
      }
    }
    println("To place your symbol, you must enter the coordinate where you wish to place your symbol.")
    println("Input must be given in the form x,y with no space or brackets")
    println("For example, if you wish to place your symbol in the middle, you must enter '1,1'")
    println("Example: ")
    println("Where would you like to place your O? 1,1")
    println("In case of illegal placement, the system will prompt you to try again.")
    println("That's all. Have fun!")
    println()
    println()
    println("GAME BEGINS")
  }

  def print_grid(grid: Array[Array[String]]): Unit ={
    println("|---|---|---|")
    for (i <- 0 to 2; j <- 0 to 2){
      if (j == 0){
        print("| ")
      }
      print(grid(i)(j))
      if (j == 2){
        println(" | ")
        println("|---|---|---|")
      }
      else{
        print(" | ")
      }
    }
  }

  def player_move(turn: String, grid: Array[Array[String]]): Unit ={
    var coordinate_input = scala.io.StdIn.readLine(s"Where would you like to place your $turn? ")
    var coordinate = coordinate_input.split(",").map(_.toInt)
    if (grid(coordinate(0))(coordinate(1)) == " "){
      grid(coordinate(0))(coordinate(1)) = turn
      switch_turn()
      check_for_win()
      check_if_tie()
    }
    else{
      println("Oops! Illegal move. Try again!")
      player_move(turn, grid)
    }

  }

  def switch_turn(): Unit ={
    if (whoseTurn == "O"){whoseTurn = "X"}
    else {whoseTurn = "O"}
  }

  def check_for_win(): Unit ={
    if (grid(0)(0) == "O" && grid(0)(1) == "O" && grid(0)(2) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(1)(0) == "O" && grid(1)(1) == "O" && grid(1)(2) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(2)(0) == "O" && grid(2)(1) == "O" && grid(2)(2) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(0) == "O" && grid(1)(0) == "O" && grid(2)(0) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(1) == "O" && grid(1)(1) == "O" && grid(2)(1) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(2) == "O" && grid(1)(2) == "O" && grid(2)(2) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(0) == "O" && grid(1)(1) == "O" && grid(2)(2) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(2) == "O" && grid(1)(1) == "O" && grid(2)(0) == "O"){winner = "O"; there_is_a_winner()}
    else if (grid(0)(0) == "X" && grid(0)(1) == "X" && grid(0)(2) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(1)(0) == "X" && grid(1)(1) == "X" && grid(1)(2) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(2)(0) == "X" && grid(2)(1) == "X" && grid(2)(2) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(0)(0) == "X" && grid(1)(0) == "X" && grid(2)(0) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(0)(1) == "X" && grid(1)(1) == "X" && grid(2)(1) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(0)(2) == "X" && grid(1)(2) == "X" && grid(2)(2) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(0)(0) == "X" && grid(1)(1) == "X" && grid(2)(2) == "X"){winner = "X"; there_is_a_winner()}
    else if (grid(0)(2) == "X" && grid(1)(1) == "X" && grid(2)(0) == "X"){winner = "X"; there_is_a_winner()}
  }

  def check_if_tie(): Unit ={
    if (grid(0)(0) != " " && grid(0)(1) != " " && grid(0)(2) != " " && grid(1)(0) != " " && grid(1)(1) != " "
        && grid(1)(2) != " " && grid(2)(0) != " " && grid(2)(1) != " " && grid(2)(2) != " " && !gameOver){
      gameOver = true
      print_grid(grid)
      println("GAME OVER")
      println("Well played! It is a tie!")
    }
  }

  def there_is_a_winner(): Unit ={
    print_grid(grid)
    println("GAME OVER")
    println(s"$winner is the winner!")
    gameOver = true
  }

}
