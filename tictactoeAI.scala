object tictactoeAI {
  val X = "X"
  val O = "O"
  val EMPTY = " "

  def main(args: Array[String]): Unit = {
    print_instructions()
    val user = scala.io.StdIn.readLine("Would you like to be X or O? (X starts): ")
    var game_over = false
    var board = initial_state()
    var ai_turn = false
    if (user == X) {
      ai_turn = false
    }
    else if (user == O) {
      ai_turn = true
    }
    println("GAME BEGINS!")
    println()
    print_board(board)
    while (!game_over) {
      if (ai_turn) {
        println()
        println("AI is thinking...")
        println()
        val ai_action = minimax(board)
        board = result(board, ai_action)
        print_board(board)
        ai_turn = false
        game_over = isTerminalState(board)
      }
      else {
        println()
        val user_input = scala.io.StdIn.readLine(s"Where would you like to place your $user? ")
        println()
        val user_action_arr = user_input.split(",").map(_.toInt)
        val user_action = (user_action_arr(0), user_action_arr(1))
        board = result(board, user_action)
        print_board(board)
        ai_turn = true
        game_over = isTerminalState(board)
      }
    }
    if (winner(board) != EMPTY) {
      println(s"${winner(board)} is the winner!")
    }
    else {
      println("Well played, it is a draw!")
    }
  }

  def initial_state(): Array[Array[String]] = {
    val board = Array(Array(EMPTY, EMPTY, EMPTY),
      Array(EMPTY, EMPTY, EMPTY),
      Array(EMPTY, EMPTY, EMPTY))
    return board
  }

  def whoseTurn(state: Array[Array[String]]): String = {
    var EmptyCount = 0
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (state(i)(j) == " ") {
          EmptyCount += 1
        }
      }
    }
    if (EmptyCount % 2 == 0) {
      return O
    }
    else {
      return X
    }
  }

  def possibleactions(state: Array[Array[String]]): Array[(Int, Int)] = {
    var actions: Array[(Int, Int)] = Array()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (state(i)(j) == " ") {
          actions = actions :+ (i, j)
        }
      }
    }
    return actions
  }

  def result(state: Array[Array[String]], action: (Int, Int)): Array[Array[String]] = {
    var newstate = state.map(_.clone())
    if (newstate(action._1)(action._2) == EMPTY) {
      newstate(action._1)(action._2) = whoseTurn(state)
    }
    return newstate
  }

  def winner(state: Array[Array[String]]): String = {
    var reward = getreward(state)
    if (reward == 1) {
      return X
    }
    else if (reward == -1) {
      return O
    }
    else {
      return EMPTY
    }
  }

  def getreward(state: Array[Array[String]]): Int = {
    if (isTerminalState(state)) {
      if (state(0)(0) == "O" && state(0)(1) == "O" && state(0)(2) == "O") {}
      else if (state(1)(0) == "O" && state(1)(1) == "O" && state(1)(2) == "O") {
        return -1
      }
      else if (state(2)(0) == "O" && state(2)(1) == "O" && state(2)(2) == "O") {
        return -1
      }
      else if (state(0)(0) == "O" && state(1)(0) == "O" && state(2)(0) == "O") {
        return -1
      }
      else if (state(0)(1) == "O" && state(1)(1) == "O" && state(2)(1) == "O") {
        return -1
      }
      else if (state(0)(2) == "O" && state(1)(2) == "O" && state(2)(2) == "O") {
        return -1
      }
      else if (state(0)(0) == "O" && state(1)(1) == "O" && state(2)(2) == "O") {
        return -1
      }
      else if (state(0)(2) == "O" && state(1)(1) == "O" && state(2)(0) == "O") {
        return -1
      }
      else if (state(0)(0) == "X" && state(0)(1) == "X" && state(0)(2) == "X") {
        return 1
      }
      else if (state(1)(0) == "X" && state(1)(1) == "X" && state(1)(2) == "X") {
        return 1
      }
      else if (state(2)(0) == "X" && state(2)(1) == "X" && state(2)(2) == "X") {
        return 1
      }
      else if (state(0)(0) == "X" && state(1)(0) == "X" && state(2)(0) == "X") {
        return 1
      }
      else if (state(0)(1) == "X" && state(1)(1) == "X" && state(2)(1) == "X") {
        return 1
      }
      else if (state(0)(2) == "X" && state(1)(2) == "X" && state(2)(2) == "X") {
        return 1
      }
      else if (state(0)(0) == "X" && state(1)(1) == "X" && state(2)(2) == "X") {
        return 1
      }
      else if (state(0)(2) == "X" && state(1)(1) == "X" && state(2)(0) == "X") {
        return 1
      }
      else {
        return 0
      }
    }
    return 0
  }

  def isTerminalState(state: Array[Array[String]]): Boolean = {
    if (state(0)(0) != " " && state(0)(1) != " " && state(0)(2) != " " && state(1)(0) != " " && state(1)(1) != " "
      && state(1)(2) != " " && state(2)(0) != " " && state(2)(1) != " " && state(2)(2) != " ") {
      return true
    }
    if (state(0)(0) == "O" && state(0)(1) == "O" && state(0)(2) == "O") {
      return true
    }
    else if (state(1)(0) == "O" && state(1)(1) == "O" && state(1)(2) == "O") {
      return true
    }
    else if (state(2)(0) == "O" && state(2)(1) == "O" && state(2)(2) == "O") {
      return true
    }
    else if (state(0)(0) == "O" && state(1)(0) == "O" && state(2)(0) == "O") {
      return true
    }
    else if (state(0)(1) == "O" && state(1)(1) == "O" && state(2)(1) == "O") {
      return true
    }
    else if (state(0)(2) == "O" && state(1)(2) == "O" && state(2)(2) == "O") {
      return true
    }
    else if (state(0)(0) == "O" && state(1)(1) == "O" && state(2)(2) == "O") {
      return true
    }
    else if (state(0)(2) == "O" && state(1)(1) == "O" && state(2)(0) == "O") {
      return true
    }
    else if (state(0)(0) == "X" && state(0)(1) == "X" && state(0)(2) == "X") {
      return true
    }
    else if (state(1)(0) == "X" && state(1)(1) == "X" && state(1)(2) == "X") {
      return true
    }
    else if (state(2)(0) == "X" && state(2)(1) == "X" && state(2)(2) == "X") {
      return true
    }
    else if (state(0)(0) == "X" && state(1)(0) == "X" && state(2)(0) == "X") {
      return true
    }
    else if (state(0)(1) == "X" && state(1)(1) == "X" && state(2)(1) == "X") {
      return true
    }
    else if (state(0)(2) == "X" && state(1)(2) == "X" && state(2)(2) == "X") {
      return true
    }
    else if (state(0)(0) == "X" && state(1)(1) == "X" && state(2)(2) == "X") {
      return true
    }
    else if (state(0)(2) == "X" && state(1)(1) == "X" && state(2)(0) == "X") {
      return true
    }
    else {
      return false
    }
  }

  def minimax(state: Array[Array[String]]): (Int, Int) = {
    if (isTerminalState(state)) {
      throw new Exception("game over")
    }
    var optimalaction = (0, 0)
    if (whoseTurn(state) == X) {
      var bestval = -100
      for (act <- possibleactions(state)) {
        if (bestval < minval(result(state, act))) {
          bestval = minval(result(state, act))
          optimalaction = act
        }
      }
      return optimalaction
    }
    else {
      var bestval = 100
      for (act <- possibleactions(state)) {
        if (bestval > maxval(result(state, act))) {
          bestval = maxval(result(state, act))
          optimalaction = act
        }
      }
      return optimalaction
    }
  }

  def maxval(state: Array[Array[String]]): Int = {
    if (isTerminalState(state)) {
      return getreward(state)
    }
    var v = -100
    for (action <- possibleactions(state)) {
      v = v.max(minval(result(state, action)))
    }
    return v
  }

  def minval(state: Array[Array[String]]): Int = {
    if (isTerminalState(state)) {
      return getreward(state)
    }
    var v = 100
    for (action <- possibleactions(state)) {
      v = v.min(maxval(result(state, action)))
    }
    return v
  }

  def print_instructions(): Unit = {
    println("INSTRUCTIONS:")
    println("The grid is of the form")
    println("|-----|-----|-----|")
    for (i <- 0 to 2; j <- 0 to 2) {
      if (j == 0) {
        print("|")
      }
      print(s"($i,$j)")
      if (j == 2) {
        println("|")
        println("|-----|-----|-----|")
      }
      else {
        print("|")
      }
    }
    println("When choosing your symbol, type in either X or O")
    println("To place your symbol, you must enter the coordinate where you wish to place your symbol.")
    println("Input must be given in the form x,y with no space or brackets")
    println("For example, if you wish to place your symbol in the middle, you must enter '1,1'")
    println("Example: ")
    println("Where would you like to place your O? 1,1")
    println("Make sure you DONOT place your symbol in an invalid state.")
    println("That's all. Have fun!")
    println()
  }

  def print_board(state: Array[Array[String]]): Unit = {
    println("|---|---|---|")
    for (i <- 0 to 2; j <- 0 to 2) {
      if (j == 0) {
        print("| ")
      }
      print(state(i)(j))
      if (j == 2) {
        println(" | ")
        println("|---|---|---|")
      }
      else {
        print(" | ")
      }
    }
  }

}
