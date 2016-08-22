package com.johnregan

import org.scalatest.{FlatSpec, Matchers}


class PokerAppSpec extends FlatSpec with Matchers {

  "A poker app" should "match a royal flush correctly" in {
    val hand=List("10H","JH","QH","KH","AH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Royal Flush"
  }

  it should "match a straight flush correctly" in {
    val hand=List("9H","10H","JH","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Straight Flush:Hearts - 9 to K"
  }

  it should "match a full house correctly" in {
    val hand=List("9D","9S","JH","JD","JS")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Full House:J's over 9's"
  }

  it should "match a flush correctly" in {
    val hand=List("2H","10H","JH","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Flush:Hearts"
  }

  it should "match a straight correctly" in {
    val hand=List("9D","10S","JH","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Straight:9 to K"
  }

  it should "match trips correctly" in {
    val hand=List("9H","9D","9S","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Three of a Kind:9's"
  }

  it should "match two pair correctly" in {
    val hand=List("9H","9D","JH","QH","QS")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Two Pair:9's and Q's"
  }

  it should "match a pair correctly" in {
    val hand=List("9H","9D","JH","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Pair:9's"
  }

  it should "high card correctly" in {
    val hand=List("2D","10H","JH","QH","KH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "HighCard"
  }

  it should "respond invalid for too few cards" in {
    val hand=List("2D","10H","JH","QH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Invalid hand: Too few cards"
  }

  it should "respond invalid for too many cards" in {
    val hand=List("2D","10H","JH","QH","AC","3H")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Invalid hand: Too many cards"
  }

  it should "respond invalid for duplicate cards" in {
    val hand=List("JD","QC","4H","4H","4H")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "Invalid hand: 4H appears 3 times"
  }

  it should "return the high card" in {
    val hand=List("2D","10H","JH","QH")
    val result=PokerApp.classifyHand(hand)
    result shouldBe "High Card:QC"
  }
}