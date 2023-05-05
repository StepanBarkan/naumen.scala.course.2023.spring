object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = (seq.size - 1 to 0).map(x =>  seq(x))


  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = fibonacci(idx).last

  def fibonacci(idx: Int): Seq[Int] = idx match {
    case 0 => 0 :: Nil
    case 1 => 0 :: 1 :: Nil
    case _ => (2 to idx)
      .foldLeft(fibonacci(1))(
        (s, i) => s :+ (s(i - 1) + s(i - 2))
      )
  }

  private lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.map(c => MORSE.getOrElse(c.toString.toUpperCase, c.toString)).mkString(" ")


  def wordReverse(text: String): String = {
    text.split(" ").map(word => {
      val part = word.partition(_.isLetter)
      val reversed = part._1.reverse.toLowerCase
      (if (word.headOption.exists(_.isUpper)) reversed.capitalize
      else reversed) + part._2

    }).mkString(" ")
  }

}
