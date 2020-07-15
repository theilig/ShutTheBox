import scala.util.Random

class Dice {
  def roll: Int = Random.between(1, 7)
  def possibilities: Set[Int] = Set(1, 2, 3, 4, 5, 6)
}
