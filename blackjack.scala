import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Card(number: String, suit: String, value: Int) {
  val cardnumber: String = number
  val cardsuit: String = suit
  val cardname: String = cardnumber + "of" + cardsuit
  var cardval: Int = value
}

class Deck() {
  val cardvals: Map[String, Int] = Map("Ace" -> 1, "Two" -> 2, "Three" -> 3, "Four" -> 4, "Five" -> 5, "Six" -> 6,
    "Seven" -> 7, "Eight" -> 8, "Nine" -> 9, "Ten" -> 10, "Jack" -> 10, "Queen" -> 10, "King" -> 10)
  val numbers: Array[String] = Array("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten",
    "Jack", "Queen", "King")
  val suites: Array[String] = Array("Hearts", "Spades", "Diamonds", "Clubs")
  var deck: ArrayBuffer[Card] = ArrayBuffer()
  for (number <- numbers) {
    for (suit <- suites) {
      deck = deck :+ new Card(number, suit, cardvals(number))
    }
  }

  def drawcard(): Card = {
    val cardindex = Random.nextInt(deck.length)
    var drawncard = deck(cardindex)
    deck -= drawncard
    return drawncard
  }
}

class Hand() {
  var cardsinhand: ArrayBuffer[Card] = ArrayBuffer()
  var value: Int = 0
  for (card <- cardsinhand) {
    value += card.cardval
  }

  def add(card: Card): Unit = {
    cardsinhand = cardsinhand :+ card
    evaluate()
  }

  def evaluate(): Unit = {
    var tempval = 0
    for (card <- cardsinhand) {
      tempval += card.cardval
      value = tempval
    }
  }

  def printhand(): Unit = {
    print("Your hand is: ")
    for (card <- cardsinhand) {
      print(card.cardname + ", ")
    }
    println()
  }

  def printdealerhand(): Unit = {
    print("Dealer's hand is: ")
    for (card <- cardsinhand) {
      print(card.cardname + ", ")
    }
    println()
  }
}

class Dealer(deck: Deck) {
  var hand = new Hand()
  var win: Boolean = false

  def playhand(): Unit = {
    hand.evaluate()
    if (hand.value < 16) {
      hit()
    }
    else if (hand.value > 21) {
      win = false
    }
    else {
      stand()
    }
  }

  def hit(): Unit = {
    println("Dealer hits")
    while (hand.value <= 21 && hand.value <= 16) {
      hand.add(deal())
      hand.evaluate()
    }
    hand.printdealerhand()
  }

  def deal(): Card = {
    val card = deck.drawcard()
    return card
  }

  def stand(): Unit = {
    println("Dealer stands")
    hand = hand
  }

}

class Player(dealer: Dealer) {
  val actions: Array[String] = Array("stand", "hit", "surrender")
  var hand = new Hand()
  var win: Boolean = false
  var aceval = 1

  def playhand(): Unit = {
    var playeraction = scala.io.StdIn.readLine(s"What would you like to do (hit/stand/surrender)? ")
    while (actions.indexOf(playeraction) == -1) {
      playeraction = scala.io.StdIn.readLine("Invalid response! Please enter a valid action: ")
    }
    if (playeraction == "hit") {
      hit()
    }
    else if (playeraction == "stand") {
      stand()
    }
    else if (playeraction == "surrender") {
      surrender()
    }
    for (card <- hand.cardsinhand) {
      if (card.cardnumber == "Ace") {
        aceval = scala.io.StdIn.readLine("What do you want the value of your ace to be (1/11)? ").toInt
        card.cardval = aceval
      }
    }
  }

  def stand(): Unit = {
    hand = hand
    hand.printhand()
  }

  def hit(): Unit = {
    hand.add(dealer.deal())
    hand.printhand()
    var more = scala.io.StdIn.readLine("Would you like another card (Y/N)? ")
    while (more == "Y" && hand.value <= 21) {
      hand.add(dealer.deal())
      hand.printhand()
      more = scala.io.StdIn.readLine("Would you like another card (Y/N)? ")
    }
    hand.printhand()
  }

  def surrender(): Unit = {
    win = false
    dealer.win = true
    hand.printhand()
  }

}

object blackjack {
  var deck = new Deck()
  var dealer = new Dealer(deck)
  var user = new Player(dealer)

  def main(args: Array[String]): Unit = {
    print_instructions()
    var playagain = "Y"
    while (playagain == "Y") {
      play()
      reset()
      println()
      playagain = scala.io.StdIn.readLine("Play again (Y/N)? ")
      println()
    }
  }

  def play(): Unit = {
    for (_ <- 1 until 3) {
      user.hand.add(dealer.deal())
      dealer.hand.add(dealer.deal())
    }
    println("Dealer's hand: " + dealer.hand.cardsinhand(0).cardname + ", ???")
    user.hand.printhand()
    user.playhand()
    user.hand.evaluate()
    dealer.hand.printdealerhand()
    dealer.playhand()
    dealer.hand.evaluate()
    println(s"Dealer's hand's value is: ${dealer.hand.value}, your hand's value is: ${user.hand.value}")
    if (!dealer.win && !user.win) {
      if (user.hand.value > 21 && dealer.hand.value > 21) {
        println("Well, looks like its a push (draw)!")
      }
      else if (user.hand.value > 21) {
        user.win = false
        dealer.win = true
        check()
      }
      else if (dealer.hand.value > 21) {
        user.win = true
        dealer.win = false
        check()
      }
      else if (dealer.hand.value > user.hand.value) {
        dealer.win = true
        user.win = false
        check()
      }
      else if (dealer.hand.value < user.hand.value) {
        dealer.win = false
        user.win = true
        check()
      }
      else {
        println("Well, looks like its a push (draw)!")
      }
    }
    else {
      check()
    }
  }

  def check(): Unit = {
    if (dealer.hand.value != user.hand.value) {
      if (dealer.win) {
        println("Oh no, you lose!")
      }
      else if (user.win) {
        println("Congratulations, you win!")
      }
    }

  }

  def reset(): Unit = {
    deck = new Deck()
    dealer = new Dealer(deck)
    user = new Player(dealer)
  }

  def print_instructions(): Unit = {
    println("WELCOME TO BLACKJACK!")
    println("Please make sure that you are familiar with basic blackjack gameplay before playing.")
    println("Visit https://www.blackjackapprenticeship.com/how-to-play-blackjack/ to do so.")
    println("   Rules of the House:-")
    println("1. NO BETS. This is a friendly single player game of blackjack.")
    println("2. If you and the dealer both go over 21, it is a tie.")
    println("3. If you and the dealer get the same hand value, it is a tie.")
    println("4. Whoever gets the higher value wins, assuming that the value remains lesser than or equal to 21.")
    println("5. When the system prompts you for a yes/no (Y/N) input, make sure you type either 'Y' or 'N'. \n " +
      "  You will NOT get re-prompted. In all other prompts, you will get re-prompted.")
    println("6. You have only 3 ways of playing your hand - you can either hit, stand or surrender. You CANNOT \n" +
      "   double down or split.")
    println("7. That's just about it. Enjoy!")
    println()
  }

}
