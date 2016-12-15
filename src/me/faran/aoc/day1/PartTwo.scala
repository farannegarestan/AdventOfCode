package me.faran.aoc.day1

import scala.collection.immutable.Range

object PartTwo {
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
    val final_location = input.foldLeft((0,0,"N", List[(Int,Int)]())) { (coordinate, next) => 
      val d =  next.drop(1).toInt
      val (x, y, direction, history) = coordinate
      if (next.take(1) == "R") {
        if (direction == "N")
          (x + d, y, "E", history ::: (x+1 to x+d).map(i => (i,y)).toList)
        else if (direction == "E")
          (x , y - d, "S", history ::: (y-d to y-1).reverse.map(i => (x,i)).toList)
        else if (direction == "S")
          (x - d , y, "W", history ::: (x-d to x-1).reverse.map(i => (i,y)).toList)
        else
          (x , y + d, "N", history ::: (y+1 to y+d).map(i => (x,i)).toList)
      } else {
        if (direction == "N")
          (x - d, y, "W", history ::: (x-d to x-1).reverse.map(i => (i,y)).toList)
        else if (direction == "E")
          (x , y + d, "N", history ::: (y+1 to y+d).map(i => (x,i)).toList)
        else if (direction == "S")
          (x + d , y, "E", history ::: (x+1 to x+d).map(i => (i,y)).toList)
        else
          (x , y - d, "S", history ::: (y-d to y-1).reverse.map(i => (x,i)).toList)
      }
    }
    val history = final_location._4
    println(findFirstDuplicate(history, Set()))
  }
  
  def findFirstDuplicate(hist: List[Tuple2[Int,Int]], visited: Set[Tuple2[Int, Int]]) : Option[Tuple2[Int, Int]] = {
    if (hist.isEmpty) {
      None
    } else if(visited.contains(hist.head)) {
      Some(hist.head)
    } else {
      findFirstDuplicate(hist.tail, visited + hist.head)
    }
  }
  
}


