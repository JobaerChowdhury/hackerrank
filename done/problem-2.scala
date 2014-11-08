object Solution {
	def solve(cycles: Int): Int = {
		var length = 1
		for(i <- 0 to cycles) {
			if( i == 0) {
				length = length
			} else if (i % 2 == 0) {
				length = length + 1
			} else {
				length = 2 * length
			}
		}
		length
	}

	def main(args: Array[String]) {
        import io.Source.stdin
        var numTests = stdin.getLines().take(1).toList.head.toInt
        println(numTests)
        println(stdin.getLines().take(numTests).map(_.toInt).map(solve(_)))
        //println(temp.drop(1).map(_.toInt).map(solve(_)))
   	}
}
