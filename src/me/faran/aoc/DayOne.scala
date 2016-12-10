package me.faran.aoc

object DayOne {
  val input = List("L1", "L3", "L5", "L3", "R1", "L4", "L5", "R1", "R3", "L5", "R1", 
      "L3", "L2", "L3", "R2", "R2", "L3", "L3", "R1", "L2", "R1", "L3", "L2", "R4", 
      "R2", "L5", "R4", "L5", "R4", "L2", "R3", "L2", "R4", "R1", "L5", "L4", "R1", 
      "L2", "R3", "R1", "R2", "L4", "R1", "L2", "R3", "L2", "L3", "R5", "L192", "R4", 
      "L5", "R4", "L1", "R4", "L4", "R2", "L5", "R45", "L2", "L5", "R4", "R5", "L3", "R5", 
      "R77", "R2", "R5", "L5", "R1", "R4", "L4", "L4", "R2", "L4", "L1", "R191", "R1", "L1", 
      "L2", "L2", "L4", "L3", "R1", "L3", "R1", "R5", "R3", "L1", "L4", "L2", "L3", "L1", 
      "L1", "R5", "L4", "R1", "L3", "R1", "L2", "R1", "R4", "R5", "L4", "L2", "R4", "R5", 
      "L1", "L2", "R3", "L4", "R2", "R2", "R3", "L2", "L3", "L5", "R3", "R1", "L4", "L3", "R4", 
      "R2", "R2", "R2", "R1", "L4", "R4", "R1", "R2", "R1", "L2", "L2", "R4", "L1", "L2", "R3", "L3", 
      "L5", "L4", "R4", "L3", "L1", "L5", "L3", "L5", "R5", "L5", "L4", "L2", "R1", "L2", "L4", "L2", 
      "L4", "L1", "R4", "R4", "R5", "R1", "L4", "R2", "L4", "L2", "L4", "R2", "L4", "L1", "L2", "R1", 
      "R4", "R3", "R2", "R2", "R5", "L1", "L2")
  def main(args: Array[String]) {
    val final_coordinate = move((0,0), input, "N")
    println(final_coordinate)
  } 
  
  def move(coordinate: Tuple2[Int, Int], input: List[String], direction: String): Tuple2[Int, Int] = {
    if (input.isEmpty)
      coordinate
    else {
      val next = input.head
      val d =  next.drop(1).toInt
      val (x, y) = coordinate
      if (next.take(1) == "R") {
        if (direction == "N")
          move(Tuple2(x + d, y), input.tail, "E")
        else if (direction == "E")
          move(Tuple2(x , y - d), input.tail, "S")
        else if (direction == "S")
          move(Tuple2(x - d , y ), input.tail, "W")
        else
          move(Tuple2(x , y + d ), input.tail, "N")
      } else {
        if (direction == "N")
          move(Tuple2(x - d, y), input.tail, "W")
        else if (direction == "E")
          move(Tuple2(x , y + d), input.tail, "N")
        else if (direction == "S")
          move(Tuple2(x + d , y ), input.tail, "E")
        else
          move(Tuple2(x , y - d ), input.tail, "S")
      }
    }
  }
}


