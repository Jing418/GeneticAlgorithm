package tests
import org.scalatest.FunSuite
import music.SongRating
import music.Song
import statistics.Statistics


class LectureTask5 extends FunSuite{
  test("LT5"){

    val rating1 = new SongRating(5,2)
    val rating2 = new SongRating(5,2)
    val rating3 = new SongRating(2,1)
    val rating4 = new SongRating(4,3)

    val song1 = new Song("Up","Nav","af15ARAP3H4",List(rating1,rating2))
    val song2 = new Song("Everything at Once","Lenka","eE9tV1WGTgE",List(rating3,rating4))
    val songList1 : List[Song] = List(song1)
    val songList2 : List[Song] = List(song1,song2)



    val map1: Map[String,Int] = Map()
    val map2: Map[String,Int] = Map("af15ARAP3H4" -> 1)//1
    val map3: Map[String,Int] = Map("af15ARAP3H4" -> 5,"af15ARAP3H4" -> 3,"eE9tV1WGTgE" -> 1)//3
    val map4: Map[String,Int] = Map("af15ARAP3H4" -> 5,"af15ARAP3H4" -> 3,"eE9tV1WGTgE" -> 4, "af15ARAP3H4" -> 4)//4
    val map5: Map[String,Int] = Map("af15ARAP3H4" -> 2,"af15ARAP3H4" -> 2)//2
    val map6: Map[String,Int] = Map("af15ARAP3H4" -> 5,"af15ARAP3H4" -> 5)


    val actualOutput1 : Song => Double = Song.costFunction(map1)
    val actualOutput2 : Song => Double = Song.costFunction(map2)
    val actualOutput3 : Song => Double = Song.costFunction(map3)
    val actualOutput4 : Song => Double = Song.costFunction(map4)
    val actualOutput5 : Song => Double = Song.costFunction(map5)
    val actualOutput6 : Song => Double = Song.costFunction(map6)


    val BA1: Double = Statistics.bayesianAverage[Song](songList1,actualOutput1,0,0)
    val BA2: Double = Statistics.bayesianAverage[Song](songList2,actualOutput2,0,0)
    val BA3: Double = Statistics.bayesianAverage[Song](songList2,actualOutput3,0,0)
    val BA4: Double = Statistics.bayesianAverage[Song](songList2,actualOutput4,4,3)
    val BA5: Double = Statistics.bayesianAverage[Song](songList1,actualOutput5,0,0)
    val BA6: Double = Statistics.bayesianAverage[Song](songList2,actualOutput6,0,0)

    val epsilon: Double = 0.001

    assert(Math.abs(BA1 - 0.08333333333333333) < epsilon)
    assert(Math.abs(BA2 - 500.05555555555554) < epsilon)
    assert(Math.abs(BA3 - 500.0416666666667) < epsilon)
    assert(Math.abs(BA4 - 2.024305555555556) < epsilon)
    assert(Math.abs(BA5 - 1000.0) < epsilon)
    assert(Math.abs(BA6 - 0.08055555555555555) < epsilon)

  }
}
