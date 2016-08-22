package com.johnregan


sealed trait Suit

case object Hearts extends Suit

case object Diamonds extends Suit

case object Clubs extends Suit

case object Spades extends Suit

trait Card {
  def rank: Int

  val suit: Suit
}

case class Number(number: Int, suit: Suit) extends Card {
  def rank = number

  override def toString: String = number + suit.toString
}

case class Picture(letter: Char, suit: Suit) extends Card {
  def rank = letter match {
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case _ => throw new Error(s"Unexpected character mapped as picture. $letter")
  }

  override def toString: String = letter + suit.toString
}

trait Hands {
  def cards: List[Card]

  def getCardString(f: List[Card] => Card, cards: List[Card]): (String, String) = {
    f(cards) match {
      case Number(number, suit) => (number.toString, suit.toString.charAt(0).toString)
      case Picture(letter, suit) => (letter.toString, suit.toString.charAt(0).toString)
    }
  }

  def compareWithPrevious[T](headCondition: (T, T) => (Boolean), prevElement: T, remainingElements: List[T]): Boolean = {
    remainingElements match {
      case Nil => true
      case head :: tail if headCondition(prevElement, head) => compareWithPrevious(headCondition, head, tail)
      case head :: tail => false
    }
  }

  protected def checkForMultipleCardInstances(sizeOfGroup: Int, expectedOccurrences: Int, cards: List[Card]): Boolean = {

    val filtered = cards.groupBy(_.rank).filter(_._2.size == sizeOfGroup)
    filtered.size == expectedOccurrences
  }
}

case class Hand(cards: List[Card]) extends Hands with Flush with Straight with Trips with TwoPair with Pair {
  def isRoyalFlush(): Boolean = {
    if (isFlush() && isStraight()) {

      cards.sortBy(_.rank) match {
        case List(_, _, _, Picture('K', _), Picture('A', _)) => true
        case _ => false
      }
    } else {
      false
    }
  }

  def isStraightFlush(): Boolean = isFlush() && isStraight()

  def isFullHouse(): Boolean = isTrips() && isPair()

  override def toString: String = {
    if (isStraightFlush()) getFlushResultMsg() + " - " + getStraightResultMsg()
    else if (isFlush()) getFlushResultMsg()
    else if (isStraight()) getStraightResultMsg()
    else if (isFullHouse()) getTripsResultMsg() + " over " + getPairResultMsg()
    else if (isPair()) getPairResultMsg()
    else if (isTwoPair()) getTwoPairResultMsg()
    else if (isTrips()) getTripsResultMsg()
    else "Not implemented"
  }
}

trait Flush extends Hands {
  def isFlush(): Boolean = checkForFlush(cards)

  private def checkForFlush(cards: List[Card]): Boolean = areSuitsSame(cards.map(_.suit))

  private def areSuitsSame(list: List[Suit]): Boolean = {
    compareWithPrevious((a: Suit, b: Suit) => a == b, list.head, list)
  }

  def getFlushResultMsg(): String = cards.head.suit.toString
}

trait Straight extends Hands {
  def isStraight(): Boolean = checkForStraight(cards)

  private def checkForStraight(cards: List[Card]): Boolean = {
    isSequential(cards.map(_.rank).sorted)
  }

  private def isSequential(list: List[Int]): Boolean = {
    compareWithPrevious((a: Int, b: Int) => b == a + 1, list.head - 1, list)
  }

  def getStraightResultMsg(): String = {
    val sortedCards = cards.sortBy(_.rank)
    getCardString(_.head, sortedCards)._1 + " to " + getCardString(_.last, sortedCards)._1
  }
}

trait Trips extends Hands {
  def isTrips(): Boolean = checkForMultipleCardInstances(3, 1, cards)

  def getTripsResultMsg(): String = {
    val pairItems: List[Card] = cards.groupBy(_.rank).filter(_._2.size == 3).values.head
    getCardString(_.head, pairItems)._1 + "'s"
  }
}

trait TwoPair extends Hands {
  def isTwoPair(): Boolean = checkForMultipleCardInstances(2, 2, cards)

  def getTwoPairResultMsg(): String = {
    val pairItems: Iterable[List[Card]] = cards.groupBy(_.rank).filter(_._2.size == 2).values
    List(getCardString(_.head, pairItems.head)._1, getCardString(_.head, pairItems.last)._1).mkString("", "'s and ", "'s")
  }
}

trait Pair extends Hands {
  def isPair(): Boolean = checkForMultipleCardInstances(2, 1, cards)

  def getPairResultMsg(): String = {
    val pairItems: List[Card] = cards.groupBy(_.rank).filter(_._2.size == 2).values.head
    getCardString(_.head, pairItems)._1 + "'s"
  }
}

object PokerApp {

  def classifyHand(hand: List[String]): String = hand match {
    case hand if hand.length < 5 => "Invalid hand: Too few cards"
    case hand if hand.length > 5 => "Invalid hand: Too many cards"
    case hand => {
      getDuplicates(hand) match {
        case Some(duplicates) =>
          "Invalid hand: " + duplicates.head + " appears " + duplicates.size + " times"
        case None => {
          val listOfCards: List[Card] = hand.map(card => getCardFromRawInput(card.substring(0, card.length - 1), card.last))
          getHand(Hand(listOfCards))
        }
      }
    }
  }

  private def getDuplicates(hand: List[String]): Option[List[String]] = {
    hand.groupBy(identity).filter(_._2.size > 1).values.headOption
  }

  def getHand(hand: Hand): String = {
    hand match {
      case hand if (hand.isRoyalFlush()) => "Royal Flush"
      case hand if (hand.isStraightFlush()) => "Straight Flush:" + hand
      case hand if (hand.isFullHouse()) => "Full House:" + hand
      case hand if (hand.isFlush()) => "Flush:" + hand
      case hand if (hand.isStraight()) => "Straight:" + hand
      case hand if (hand.isTrips()) => "Three of a Kind:" + hand
      case hand if (hand.isTwoPair()) => "Two Pair:" + hand
      case hand if (hand.isPair()) => "Pair:" + hand
      case hand => "HighCard"
    }
  }

  def getCardFromRawInput(picOrNumber: String, suit: Char): Card = {
    picOrNumber match {
      case "10" => Number(10, getSuitFromLetter(suit))
      case _ => picOrNumber(0) match {
        case char if char.isDigit => Number(char.asDigit, getSuitFromLetter(suit))
        case letter => Picture(letter, getSuitFromLetter(suit))
      }
    }
  }

  def getSuitFromLetter(suit: Char): Suit = suit match {
    case 'H' => Hearts
    case 'D' => Diamonds
    case 'C' => Clubs
    case 'S' => Spades
    case _ => throw new Error(s"Invalid card. Suit specified not one of the four $suit")
  }
}

object executor extends App {

}