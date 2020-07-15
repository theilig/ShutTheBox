case class Box(d: List[Dice], flaps: Map[Int, Boolean]) {
  def flapFlipsWithDoubles(roll: List[Int], i: Int): List[List[Int]] = {
    roll match {
      case x :: y :: Nil if x == y => flapFlips(x + y, 1) ::: flapFlips((x + y) * 2, 1)
      case x => flapFlips(x.sum, 1)
    }
  }

  def winningPercentage: Double = {
    val percentage = Box.knownAnswer(flaps).getOrElse(flaps match {
      case x if x.values.forall(x => x) => 1
      case _ =>
        val possibilities = dicePossibilities(d)
        val percentages = possibilities.map(roll => {
          val possibleFlips = flapFlipsWithDoubles(roll, 1)
          val choices: List[Double] = possibleFlips.map(flip => {
            if (flip.forall(f => flaps.contains(f) && !flaps(f))) {
              val newFlaps = flaps ++ flip.map(n => n -> true).toMap
              copy(flaps = newFlaps).winningPercentage
            } else {
              0
            }
          })
          choices.max
        })
        percentages.sum / percentages.length
    })
    Box.saveAnswer(flaps, percentage)
    percentage
  }

  def dicePossibilities(myDice: List[Dice]): List[List[Int]] = {
    myDice match {
      case Nil => Nil
      case x :: Nil => (1 to 6).toList.map(l => List(l))
      case x :: y =>
        val otherDicePossibilities = dicePossibilities(y)
        (for {
          ourDie <- 1 to 6
          otherDie <- otherDicePossibilities
        } yield ourDie :: otherDie).toList
    }
  }
  def flapFlips(roll: Int, minimum: Int): List[List[Int]] = {
    roll match {
      case x if x < minimum => Nil
      case x =>
        List(x) :: (minimum to x).flatMap(n => {
          flapFlips(x - n, n + 1).map(l => n :: l)
        }).toList
    }
  }
}

object Box {
  var knownAnswers: Map[Map[Int, Boolean], Double] = Map()

  def knownAnswer(flaps: Map[Int, Boolean]): Option[Double] = {
    knownAnswers.get(flaps)
  }
  def saveAnswer(flaps: Map[Int, Boolean], answer: Double): Unit = {
    knownAnswers += (flaps -> answer)
  }
}
