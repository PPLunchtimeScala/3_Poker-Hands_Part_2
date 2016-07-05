#PP Lunchtime Scala 
##Puzzle Number 3 - Poker-Hands Part 2
###Meetup date: 12/07/2016

##Submissions
* Submit your solution in a single scala worksheet
* Push the worksheet to the repo in a new folder with your name
* Use the asserts listed below to test your code
* No throwing exceptions
* No loops
* Favour Option/Try/Either to capture error cases
* Stick to basic Scala - no external libraries

Note:
* Come to the meetup whether you have done all, part or none of the problem.
* Break any of the rules you want. The only goal is that you learn something.

##Description
Last week's problem was longer than planned and many people didn't get a chance to finish it. 
This week, have a look at the code in last week's repo: https://github.com/PPLunchtimeScala/2_Poker-Hands
Now steal as much as you want and finish the problem! Seriously, read all the code and grab the best bits to start you off.

##Tests - same as last week!
If your code is correct, the following asserts should pass. 
Copy and paste them into the bottom of your worksheet and use them as the acceptance criteria for your code.

###Basic functionality
```scala
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
```
###Intermediate functionality
```scala
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Queens and Fours") //Larger card listed first
assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives") //Larger card listed first
```
###Advanced functionality
```scala
assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")
```
