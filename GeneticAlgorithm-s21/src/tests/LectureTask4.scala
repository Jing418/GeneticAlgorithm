package tests

import org.scalatest.FunSuite
import music.SongRating
import music.Song

class LectureTask4 extends FunSuite{

  test("LT4") {

    // addRating
    val rating1  = new SongRating(5, 2)
    val rating2 = new SongRating(4, 3)
    val song = new Song ("summer", "keshi", "abc", List(rating1, rating2))
    val new_rating = new SongRating(3, 1)

    val expectOutput = new Song ("summer", "keshi", "abc", List(rating1, rating2,new_rating))
    val actualOutput = song.addRating(new_rating)
    assert(expectOutput.ratings(2).energy == actualOutput.ratings(2).energy)
    assert(expectOutput.ratings(2).rating == actualOutput.ratings(2).rating)

    // readSongsFromFile
    val actualOutput2 = Song.readSongsFromFile("data/a.csv")
    val rating3  = new SongRating(5, 5)
    val rating4  = new SongRating(5, 2)
    val rating5  = new SongRating(2, 1)
    val rating6  = new SongRating(5, 2)
    val rating7  = new SongRating(5, 1)
    val song1 = new Song ("s","Nav","af15ARAP3H4",List(rating3))
    val song2 = new Song ("Everything at Once","Lenka","eE9tV1WGTgE",List(rating4))
    val song3 = new Song("you dont know about me","ella vos","tLKb_fJZS9I",List(rating5,rating7))
    val song4 = new Song("Too Much To Ask","Niall Horan","ljXSjIph5ZM",List(rating6))
    val expectOutput2 : List[Song] = List(song1,song2,song3,song4)
    //println(actualOutput2(4).youtubeId) should be fault
    println(actualOutput2(3).youtubeId)
    assert(actualOutput2(3).youtubeId == expectOutput2(0).youtubeId)
    assert(actualOutput2(0).youtubeId == expectOutput2(2).youtubeId)
    assert(actualOutput2(0).ratings(1).rating == expectOutput2(2).ratings(1).rating)
    assert(actualOutput2(0).ratings(1).energy == expectOutput2(2).ratings(1).energy)





  }


}
