package tests
import music.{Playlist, Song, SongRating}
import org.scalatest._
import statistics.Statistics

class LectureTask6 extends FunSuite {

  test("LT6") {

    val rating1 = new SongRating(1,5)
    val rating2 = new SongRating(2,2)
    val rating3 = new SongRating(5,2)


    val song1 = new Song("Up","Nav","af15ARAP3H4",List(rating1))
    val song2 = new Song("Everything at Once","Lenka","eE9tV1WGTgE",List(rating2,rating3))
    val song3 = new Song("you dont know about me","ella vos","tLKb_fJZS9I",List(rating1,rating2,rating3))


    val map1: Map[String,Int] = Map("af15ARAP3H4" -> 1,"af15ARAP3H4" -> 2,"eE9tV1WGTgE" -> 1)
    val map2: Map[String,Int] = Map("af15ARAP3H4" -> 2,"af15ARAP3H4" -> 2,"eE9tV1WGTgE" -> 2,"af15ARAP3H4" -> 2)



    val playList1 : List[Playlist] = List(new Playlist(List(song1,song2)))
    val playList2 : List[Playlist] = List(new Playlist(List(song2)))
    val playList3 : List[Playlist] = List(new Playlist(List(song3,song3)))


    val actualOutput1: Playlist => Double = Playlist.costFunction(map1)
    val actualOutput2: Playlist => Double = Playlist.costFunction(map2)


    val BA1: Double = Statistics.bayesianAverage[Playlist](playList1,actualOutput1,0,0)
    val BA2: Double = Statistics.bayesianAverage[Playlist](playList2,actualOutput1,0,0)
    val BA3: Double = Statistics.bayesianAverage[Playlist](playList3,actualOutput2,0,0)

    val epsilon: Double = 0.001
    println(BA3)

    assert(Math.abs(BA1 - 1333.3333333333333) < epsilon)
    assert(Math.abs(BA2 - 10000.0) < epsilon)
    assert(Math.abs(BA3 - 238.09523809523813) < epsilon)




  }

}
