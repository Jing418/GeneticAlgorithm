package tests

import org.scalatest._
import statistics.Statistics


class LectureTask2 extends FunSuite {
  test("LT2") {

    val epsilon: Double = 0.001
    //test1 of standardDeviation
    val My_list = List(4.0, 2.0, 3.0)
    val F= (x: Double) =>x
    val output : Double = Statistics.standardDeviation[Double](My_list,F)
    assert(Math.abs(output - 0.8165) < epsilon)

    //test2 of standardDeviation
    val My_list2 = List("four", "dog", "elephant", "tiger")//(4,3,8,5)
    val F2= (x: String) => x.length.toDouble
    val output2 : Double = Statistics.standardDeviation[String](My_list2,F2)
    assert(Math.abs(output2 - 1.87083) < epsilon)
    assert(Math.abs(output2 - 4.0) < epsilon == false)

    //test3 of standardDeviation
    val My_list3 = List( "wwww", "dddd", "oooo", "bbbb")// no_deviation
    val F3= (x: String) => x.length.toDouble
    val output3 : Double = Statistics.standardDeviation[String](My_list3,F3)
    assert(Math.abs(output3 - 0.0) < epsilon)

  }

}
