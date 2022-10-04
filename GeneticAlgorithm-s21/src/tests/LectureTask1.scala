package tests

import org.scalatest._
import statistics.Statistics

class LectureTask1 extends FunSuite {


  test("LT1") {
    val epsilon: Double = 0.0001
    //test1 of average
    val My_list = List(4.0, 2.0, 3.0)
    val F= (x: Double) =>x
    val output : Double = Statistics.average[Double](My_list,F)
    assert(Math.abs(output - 3.0) < epsilon)

    //test2 of average
    val My_list2 = List("four", "dog", "elephant", "tiger")
    val F2= (x: String) => x.length.toDouble
    val output2 : Double = Statistics.average[String](My_list2,F2)
    assert(Math.abs(output2 - 5.0) < epsilon)
    assert(Math.abs(output2 - 4.0) < epsilon == false)

    //test3 4 of topK
    val My_list3 = List(6.0, 7.0, 3.0, 4.0, 5.0, 8.0, 1.0, 2.0)
    val My_list4 = List(11.0, 12.0, 14.0, 111.0, 4.0, 99.0)
    val My_list0= List(111.0, 99.0)
    val F3= (listDouble: Double) => listDouble
    val testCases3: Map[List[Double], List[Double]] =
      Map(My_list3 -> List(8.0, 7.0, 6.0, 5.0),
        My_list4 -> List(111.0, 99.0, 14.0, 12.0),
        My_list0 -> List(111.0, 99.0)
      )
    for((input, output) <- testCases3){
      assert(Statistics.topK(input, F3, 4) == output, input)
    }


  }
}
