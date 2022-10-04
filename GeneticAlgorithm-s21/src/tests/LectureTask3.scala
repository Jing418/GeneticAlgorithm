package tests

import org.scalatest._
import statistics.Statistics

class LectureTask3 extends FunSuite {

  test("LT3") {
    val epsilon: Double = 0.0001
    val F= (x: Double) =>x
    val My_list1 = List(4.0, 2.0, 3.0)

    val output : Double = Statistics.bayesianAverage[Double](My_list1,F,7,1)
    val output4 : Double = Statistics.bayesianAverage[Double](My_list1,F,2,5)
    val output5 : Double = Statistics.bayesianAverage[Double](My_list1,F,0,5)
    assert(Math.abs(output - 1.6) < epsilon)
    assert(Math.abs(output4 - 3.8) < epsilon)
    assert(Math.abs(output5 - 3.0) < epsilon)

    val My_list2 = List(4.0, 5.0, 3.0)
    val output2 : Double = Statistics.bayesianAverage[Double](My_list2,F,1,4)
    assert(Math.abs(output2 - 4.0) < epsilon)
    assert(Math.abs(output2 - 5.0) < epsilon == false)

    val My_list3 = List("good", "fine", "ok")//4，4，2, 1, 1
    val F2= (x: String) => x.length.toDouble
    val output3 : Double = Statistics.bayesianAverage[String](My_list3,F2,2,1)
    assert(Math.abs(output3 - 2.4) < epsilon)


  }

}
