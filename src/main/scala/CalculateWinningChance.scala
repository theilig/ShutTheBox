case object CalculateWinningChance extends App {
  val state = for {
    flap1 <- List(true, false)
    flap2 <- List(true, false)
    flap3 <- List(true, false)
    flap4 <- List(true, false)
    flap5 <- List(true, false)
    flap6 <- List(true, false)
    flap7 <- List(true, false)
    flap8 <- List(true, false)
    flap9 <- List(true, false)
    flap10 <- List(true, false)
    flap11 <- List(true, false)
    flap12 <- List(true, false)
  } yield {Map(1 -> flap1, 2 -> flap2, 3 -> flap3, 4 -> flap4, 5 -> flap5, 6 -> flap6, 7 -> flap7, 8 -> flap8,
    9 -> flap9, 10 -> flap10, 11 -> flap11, 12 -> flap12)}

  state.foreach(m => {
    val box = Box(List(new Dice, new Dice), m)
    val wp = box.winningPercentage
    if (wp > .1) {
      println(m.keys.toList.sorted.map(k => {
        if (m(k)) {
          "-"
        } else {
          k.toString
        }
      }).mkString(" ") + ":" + wp)
    }})
}
