package example

object Exercise1 {

  /**
   * Given a sorted list of Integers that has been "rotated", of size at least one, locate the start of the sequence
   *
   * @param sequenced the input list of Integers
   * @return a tuple containing the value of the sequence start and it's index
   */
  def locateFirst( sequenced: List[Int] ): (Int, Int) = {
    val target: Int = sequenced.head    // Will fail if input is empty list

    def locate( segment: List[(Int, Int)], cycle: Int = 1 ): (Int, Int) = {
      // for fun we can instrument to see how many iterations this takes
      def result( value: Int, index: Int ): (Int, Int) = {
        println( s"processed input of size ${sequenced.length} in $cycle cycles" )
        (value, index)
      }

      val (left, right) = segment.splitAt( segment.length / 2 )

      val keep = if ( left.last._1 < target ) left else right

      keep match {
        case (value, index) :: Nil if (value < target ) => result(value, index + 1)
        case _ :: Nil                                   => result(target, 0)
        case (value, index) :: _ if (value < target )   => result(value, index + 1)     // not necessary but a small optimization for cases when `keep` starts with the sequence beginning
        case _                                          => locate(keep, cycle + 1)
      }
    }

    sequenced.tail match {
      case Nil    => (target, 0)  // degenerate case: list of one Integer
      case rest   => val indexed = rest.zipWithIndex
                     locate(indexed)
    }
  }

}
