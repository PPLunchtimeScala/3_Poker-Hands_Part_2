import scala.collection.{SortedMap, SortedSet}

//Domain objects
//
// Suits - all singleton objects
sealed trait Suit { def description: String; def order: Int;}
case object Diamonds extends Suit { val description = "Diamonds"; val order = 1}
case object Hearts extends Suit { val description = "Hearts"; val order = 2 }
case object Clubs extends Suit { val description = "Clubs"; val order = 3 }
case object Spades extends Suit { val description = "Spades"; val order = 4 }
// Values - all singleton objects
sealed trait Value { def value: Int; def description: String }
case object Ace extends Value { val value = 1; val description = "Ace" }
case object Two extends Value { val value = 2; val description = "Two" }
case object Three extends Value { val value = 3; val description = "Three" }
case object Four extends Value { val value = 4; val description = "Four" }
case object Five extends Value { val value = 5; val description = "Five" }
case object Six extends Value { val value = 6; val description = "Six" }
case object Seven extends Value { val value = 7; val description = "Seven" }
case object Eight extends Value { val value = 8; val description = "Eight" }
case object Nine extends Value { val value = 9; val description = "Nine" }
case object Ten extends Value { val value = 10; val description = "Ten" }
case object Jack extends Value { val value = 11; val description = "Jack" }
case object Queen extends Value { val value = 12; val description = "Queen" }
case object King extends Value { val value = 13; val description = "King" }
// Card class to represent a card
case class Card(value: Value, suit: Suit) {
  val description: String = value.description + " of " + suit.description
}
object Card {
    implicit val ordering = new Ordering[Card] {
      override def compare(x: Card, y: Card): Int = {
        val diff = java.lang.Long.signum(x.value.value - y.value.value)
        diff match {
          case 0 => if(x == y) 0 else java.lang.Long.signum(x.suit.order - y.suit.order)
          case _ => diff
        }
      }
    }
}
//
//Classes representing poker outcomes
sealed trait Outcome { def description: String }
case class HighCard(card: Card) extends Outcome {
  def description = "High card: " + card.description
}
case class Pair(cards: List[Card]) extends Outcome {
  def description = "One Pair: " + cards(0).value.description.pluralise
}
case class TwoPair(cards: List[List[Card]]) extends Outcome {
  def description = "Two Pair: " + cards(0)(0).value.description.pluralise + " over " + cards(1)(0).value.description.pluralise
}
  //case class ThreeOfAKind(cards: List(Card)) extends Outcome
  //case class FourOfAKind(cards: List(Card)) extends Outcome
  //case class FullHouse(cards: List(Card)) extends Outcome
  //case class Flush(cards: List(Card)) extends Outcome
  //case class Straight(cards: List(Card)) extends Outcome
  //case class StraightFlush(cards: List(Card)) extends Outcome
  //Hand class to represent a hand
//
  case class Hand(cards: SortedSet[Card]) {
    lazy val suits = cards.map(_.suit)
    lazy val values: SortedMap[Int, List[Card]] = {
      cards.foldLeft(SortedMap.empty[Int, List[Card]])( (map: SortedMap[Int, List[Card]], card: Card) => {
        val oldList = map.get(card.value.value)
        oldList match {
            case None => map + (card.value.value -> List(card))
            case Some(list) => map + (card.value.value -> (card :: list))
      }})
    }
//    lazy val setSizes: SortedMap[Int, Int] = {
//      values.map( (key: Int, cards: List[Card]) => (cards.length -> key) )
//    }
  val outcome: Outcome = {
    values.size match {
      //Could be straight, flush, straightflsuh or high card
      case 5 => HighCard(values.last._2.head)
      //could be Pair
      case 4 => {
        val pair = values.collectFirst( { case cardList if cardList._2.length == 2 => cardList._2 })
        Pair(pair.get)
      }
      //Could be Two pair
      case 3 => {
        val pairs = values.collect( { case cardList if cardList._2.length == 2 => cardList._2 })
        TwoPair(pairs.toList)
      }
      //Could be 4 of a kind or full house
      case 2 => ???
    }

  }
  }
//    val outcome: Option[Outcome] = {
//      cards match {
//        //Valid outcomes
//        case list => Some(HighCard(list))
//      }
//    }
//    def evaluateHand: Outcome = {
//      outcome.get
//    }
  object PokerApp {
    def classifyHand(cardsAsStrings: List[String]) = {
      //Convert to cards
      val cards = cardsAsStrings.map(_.toCard)
      //Convert to set of cards, doing basic validation.
      val cardSet = cards.foldLeft(Left(SortedSet.empty[Card]): Either[SortedSet[Card], String]) ( (set: Either[SortedSet[Card], String], card: Either[Card, String]) =>
        (set, card) match {
          case (Right(message), _) => Right[SortedSet[Card], String](message)
          case (_, Right(_)) => Right("Invalid hand: Contains invalid card")
          case (_, Left(aCard)) if set.left.get.contains(aCard) => Right(s"Invalid hand: ${aCard.description} appears two times")
          case (_, Left(aCard)) => Left(set.left.get + aCard)
        })
      //Get hand from set of cards
      val hand: Either[Hand, String] = cardSet match {
        case Right(error) => Right(error)
        case Left(set) if set.size < 5 => Right("Invalid hand: Too few cards")
        case Left(set) if set.size > 5 => Right("Invalid hand: Too many cards")
        case Left(set) => Left(Hand(set))
      }
      //Return message
      hand match {
        case Right(error) => error
        case Left(handy) => handy.outcome.description
      }
    }
  }
  println("******Hand asserts*****")
  //Basic
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "4F")) == "Invalid hand: Contains invalid card")
  assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
  //Intermediate
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
  //assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
  //assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")
  //assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Queens and Fours")
  // Larger card listed first
  //assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives")
  // Larger card listed first
  //Advanced
  //assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
  //assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
  //assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")

  ////// String to Value/Suit/Card utils
  implicit class StringToCard(asString: String) {
    def toCard(): Either[Card, String] = {
      val (value, suit) = {
        if (asString.length == 2) (asString.charAt(0).toString.toValue, asString.charAt(1).toString.toSuit)
        else if (asString.length == 3) (asString.substring(0, 2).toValue, asString.charAt(2).toString.toSuit)
        else (Right(""), Right(""))
      }
      (value, suit) match {
        case (v:Left[Value, String], s:Left[Suit, String]) => Left(Card(v.left.get, s.left.get))
        case (_,_) => Right(s"Invalid Card: ${asString}")
      }
    }
    def toSuit(): Either[Suit, String] = {
      asString match {
        case "D" => Left(Diamonds)
        case "H" => Left(Hearts)
        case "C" => Left(Clubs)
        case "S" => Left(Spades)
        case _ => Right(s"Invalid Suit: ${asString}")
      }
    }
    def toValue(): Either[Value, String] = {
      val mapping: Map[String, Value] = Map("A" -> Ace,
        "2" -> Two, "3" -> Three, "4" -> Four, "5" -> Five, "6" -> Six,
        "7" -> Seven, "8" -> Eight, "9" -> Nine, "10" -> Ten,
        "J" -> Jack, "Q" -> Queen, "K" -> King)
      mapping.getOrElse(asString, None) match {
        case None => Right(s"Invalid Value: ${asString}")
        case value:Value => Left(value)
      }
    }
    def pluralise(): String = {
      asString match {
        case "Six" => "Sixes"
        case "six" => "sixes"
        case str:String => str + "s"
    }
    }
  }
println("Card asserts")
assert("A".toValue()==Left(Ace))
assert("345".toValue()==Right("Invalid Value: 345"))
assert("D".toSuit()==Left(Diamonds))
assert("345".toSuit()==Right("Invalid Suit: 345"))
assert("1".toCard()==Right("Invalid Card: 1"))
assert("10CC".toCard()==Right("Invalid Card: 10CC"))
assert("10F".toCard()==Right("Invalid Card: 10F"))
assert("10D".toCard()==Left(Card(Ten, Diamonds)))
assert("AD".toCard()==Left(Card(Ace, Diamonds)))