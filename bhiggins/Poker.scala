package paddypower

case class Card(suit: Suit.Value, face: Face.Value) extends Ordered[Card] {
  def compare(card: Card) = {
    if (this.face == card.face) {
      if (this.suit == card.suit) 0
      else if (this.face > card.face) 1
      else -1
    } else if (this.face < card.face) -1
    else 1
  }
}

case class Result(result: String, rank: Int)//, cards: List[Card])
object Suit extends Enumeration { val Hearts, Diamonds, Clubs, Spades = Value }
object Face extends Enumeration { val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value }
object Cards {
  import Suit._
  val suitMap = Map('H'-> Hearts, 'D'-> Diamonds, 'C'-> Clubs, 'S' -> Spades)

  import Face._
  val rankMap = Map("A"-> Ace, "2"-> Two, "3"-> Three, "4"-> Four, "5"-> Five, "6"-> Six, "7"-> Seven, "8"-> Eight,
    "9"-> Nine, "10"-> Ten, "J"-> Jack, "Q"-> Queen, "K"-> King)

  private def parseRank(x: String): Option[Face.Value] = rankMap.get(x);

  private def parseSuit(x: Char): Option[Suit.Value] = suitMap.get(x);

  private def parseCard(c: String): Option[Card] =
    for (s <- parseSuit(c.head);
         r <- parseRank(c.tail))
      yield (Card.apply(s, r))

  def parseCards(x: List[String]): List[Card] =  x.flatMap(x => parseCard(x)).sorted
}

object Hands {
  private def classifyFlush(cards: List[Card]): Either[List[Card], Result] =
    if (cards.filter(c => c.suit == cards.head.suit).length == cards.length) Right(Result("Flush", 500 + cards.last.face.id)) else Left(cards)

  private def getRunLength(cards: List[Card]): Int = getCardPairings(cards)((c1, c2) => (c1.face.id + 1) % 13 == c2.face.id % 13).length

  private def isStraight(cards: List[Card]): Either[List[Card], Result] =
    if (getRunLength(cards) == cards.length - 1) Right(Result(s"${cards.last.face} High Straight", 400 + cards.last.face.id))
    else swapAceIfExists(cards) match {
      case Some(swappedCards) =>
        if (getRunLength(swappedCards) == swappedCards.length - 1)
          Right(Result(s"${swappedCards.last.face} High Straight", 400 + swappedCards.last.face.id))
        else Left(cards)
      case None => Left(cards)
    }

  private def swapAceIfExists(cards: List[Card]): Option[List[Card]] =
    if (cards.last.face == Face.Ace) Some(cards.last :: cards.dropRight(1)) else None

  private def groupByKind(cards: List[Card]): List[(Card, Card)] = getCardPairings(cards)((c1, c2) => (c1.face.id == c2.face.id))

  private def getCardPairings(cards: List[Card])(func: (Card, Card) => Boolean) : List[(Card, Card)] =
    for ((c1, c2) <- (cards.zip(cards.tail))
       if func(c1, c2)) yield (c1, c2)

  private def classifyBySuit(cards: List[Card]): Either[List[Card], Result] = {
    val tuples = groupByKind(cards)
    if (tuples.length == 1) Right(Result(s"A pair of ${tuples.head._1.face}", 100 + tuples.head._1.face.id))
    else if (tuples.length == 2)
      if (firstAndLastTuplesMatch(tuples)) Right(Result("Three of a kind", 300 + tuples.head._1.face.id))
      else Right(Result("Two pair", 200 + tuples.head._1.face.id))
    else if (tuples.length == 3)
      if (firstAndLastTuplesMatch(tuples)) Right(Result("Four of a kind", 700 + tuples.last._2.face.id))
      else Right(Result("Full House", 600 + tuples.last._1.face.id * 2 + tuples.last._2.face.id))
    else Left(cards)
  }

  private def classifyByGroupings(cards: List[Card]): Either[List[Card], Result] =
    classifyStraight(cards) match {
      case Right(straightResult) =>
        classifyFlush(cards) match {
          case Right(flushResult) => Right(Result(straightResult.result + " " + flushResult.result, 800 + cards.last.face.id))
          case Left(cards) => Right(straightResult)
        }
      case Left(swappedCards) => classifyFlush(swappedCards)
    }

  private def classifyStraight(cards: List[Card]): Either[List[Card], Result] =
    isStraight(cards) match {
      case Right(result) => Right(result)
      case Left(swappedAce) => isStraight(swappedAce) match {
        case Right(result) => Right(result)
        case Left(cards) => Left(swappedAce)
      }
    }

  private def firstAndLastTuplesMatch(tuples: List[(Card, Card)]): Boolean = tuples.head._1.face == tuples.last._2.face

  def classify(cards: List[Card]): Result =
    classifyBySuit(cards) match {
      case Right(result) => result
      case Left(c) => classifyByGroupings(c).right.getOrElse(Result(s"High Card: ${cards.last.face}", cards.last.face.id))
  }
}

object Poker {
  def main(args: Array[String]): Unit = {
    println(Hands.classify(Cards.parseCards(List("D2", "C2", "S2", "H2", "SA"))))
    println(Hands.classify(Cards.parseCards(List("C10", "S10", "H10", "SQ", "DA"))))
    println(Hands.classify(Cards.parseCards(List("C10", "S10", "H9", "SQ", "DA"))))
    println(Hands.classify(Cards.parseCards(List("C2", "S3", "H4", "S5", "DA"))))
    println(Hands.classify(Cards.parseCards(List("DA", "C10", "SJ", "HQ", "SK"))))
    println(Hands.classify(Cards.parseCards(List("C10", "S10", "HQ", "SQ", "DA"))))
    println(Hands.classify(Cards.parseCards(List("C10", "S10", "HQ", "SQ", "D10"))))
    println(Hands.classify(Cards.parseCards(List("C10", "C6", "CQ", "C4", "CK"))))
    println(Hands.classify(Cards.parseCards(List("C10", "CJ", "CQ", "CA", "CK"))))
    println(Hands.classify(Cards.parseCards(List("CQ", "DJ", "D6", "SA", "CK"))))
  }
}