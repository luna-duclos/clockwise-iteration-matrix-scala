object Direction extends Enumeration {
  type Direction = Value
  val RIGHT, DOWN, LEFT, UP = Value
}

def clockwiseMatrix(input : Array[Array[Int]]) : String = {
  // A clockwise rotation can be described as a set of rotations going Right -> Down -> Left -> Up.
  // Each iteration goes in its direction until it meets a row/column that was already processed.
  // With an amount of iterations equal to width * length.
  // We define an enum above to encode this, and then we setup a loop to do just that for each value until done.
  import Direction._

  val res = new StringBuilder()
  if ( input.length == 0) return res.toString()
  if ( input(0).length == 0) return res.toString()

  val height = input.length
  val width = input(0).length

  var x, y = 0
  // When going right the first time, we need to go till the end => till width
  // When going down the first time, we also need to go till the end => till height
  // When going left the first time, we also need to go till the end => till 0, ie. set at -1 so we do so
  // When going up the first time, we need to skip the first row as we already processed it when going right from 0, 0 => to row 1, ie. set at 0 so we do so
  var (rightDone, downDone, leftDone, upDone) = (width, height, -1, 0)
  var direction = RIGHT

  // The number of iterations we need to do total is width * height, -1 because we're zero indexed
  for ( i <- 0 to (width*height)-1) {
    if (i != 0)
      res ++= ", "


    res ++= input(y)(x).toString()

    val _ = direction match
      case RIGHT => {
        x = x+1
        if (x+1 == rightDone) {
          rightDone = x
          direction = DOWN
        }
      }
      case DOWN => {
        y = y + 1
        if (y+1 == downDone) {
          downDone = y
          direction = LEFT
        }
      }
      case LEFT => {
        x = x - 1
        if (x-1 == leftDone) {
          leftDone = x
          direction = UP
        }
      }
      case UP => {
        y = y - 1
        if (y-1 == upDone) {
          upDone = y
          direction = RIGHT
        }
      }
  }

  res.toString()
}