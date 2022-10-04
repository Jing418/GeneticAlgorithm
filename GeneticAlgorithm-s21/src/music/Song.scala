package music

import statistics.Statistics

import scala.io.Source

class Song(val title: String, val artist: String, val youtubeId: String, val ratings: List[SongRating]) {

  def averageRating(): Double = {
    // This is an example of calling your average function to get the average rating of a song
    Statistics.average(ratings, (rating: SongRating) => rating.rating)
  }

  def averageEnergyRating(): Double = {
    // This is an example of calling your average function to get the average energy rating of a song
    Statistics.average(ratings, (rating: SongRating) => rating.energy)
  }

  // comment this in when you complete LT3 to compute the bayesian average of song ratings
  def bayesianRating(extraRatings: Int, valueOfExtraRatings: Int): Double = {
    Statistics.bayesianAverage(this.ratings, (rating: SongRating) => rating.rating, extraRatings, valueOfExtraRatings)
  }

  def addRating( newRating: SongRating): Song ={
    //println(newRating.energy)
    val totalRatingList:List[SongRating] = ratings :+ newRating
    //println(totalRatingList(2).rating)
    val output = new Song (title,artist,youtubeId,totalRatingList)
    output
  }
}

object Song {

  def lineSong(line:Any):Song = {
    val line1 = line.toString.split(',')
    val ratingToList:List[SongRating] = List(new SongRating(line1(3).toInt,line1(4).toInt))
    val songToList = new Song(line1(2),line1(1),line1(0),ratingToList)
    songToList
  }

  def updateList(beforeAdd:List[Any]):List[Song] = {
    if (beforeAdd.isEmpty) {
      List()
    }
    else{
      val length : Int = beforeAdd.length
      val newLine = lineSong(beforeAdd(length-1))
      List(newLine) ++ updateList(beforeAdd.dropRight(1))
    }
  }


  def compareSongs(a1: Song, a2: Song): Boolean = {
    a1.youtubeId.toLowerCase() < a2.youtubeId.toLowerCase()
  }

  def nextSame( input : List[Song]):  List[SongRating] = {
    if (input.isEmpty){
      List()
    }else{
      val length: Int = input.length
      List(input(length-1).ratings.head) ++ nextSame(input.dropRight(1))
    }
  }

  def throwAdded(addList: List[SongRating], addSong: Song): Song ={
    if (addList.length == 1){
      addSong.addRating(addList.last)
    }else{
      throwAdded(addList.dropRight(1), addSong.addRating(addList.last))
    }
  }


  def addSongs( input : List[Song]): List[Song] = {
    if (input.isEmpty){
      List()
    }else{
      if (input.length >= 2){

        if(input(input.length-1).youtubeId == input(input.length-2).youtubeId){

          val ratingID: String = input.last.youtubeId
          val finalIdList: List[Song] = input.filter(_.youtubeId == ratingID)
          val sameRatingList:  List[SongRating] = nextSame(finalIdList)
          val sameRatingList2: List[SongRating] = sameRatingList.drop(1)
          val songWithNewRating: Song = throwAdded(sameRatingList2,input(input.length-1))
          val songDrop: List[Song] = input.dropRight(sameRatingList2.length + 1)
          val finalList : List[Song] = List(songWithNewRating) ++ addSongs(songDrop)
          finalList

        }else{
          List(input(input.length-1)) ++ addSongs(input.dropRight(1))
        }

      }else{
        List(input(input.length-1)) ++ addSongs(input.dropRight(1))
      }
    }
  }

  def readSongsFromFile(filename: String): List[Song] = {
    val file = Source.fromFile(filename)
    val lines: List[String] = file.getLines().toList
    file.close()
    val totalList: List[Song] = updateList(lines)
    // TODO: Parse the lines and return a List of Songs containing all the information from the file
    val listInSort : List[Song] = totalList.sortWith(compareSongs)
    val output: List[Song] = addSongs(listInSort)
    output
  }








  def makeIncubator(songs: List[Song]): List[Double] => Song = {
    genes: List[Double] => {
      // assumes there is only 1 gene and converts that Double to an Int and retrieves the song at
      // that position in the list
      val geneSong: Int = (genes.head.abs * songs.length).toInt % songs.length
      songs(geneSong)
    }
  }


  def costFunction(input: Map[String,Int]): Song => Double = {
    ( rateSong : Song) => {

      val userRating : Int  = input.getOrElse(rateSong.youtubeId,0) // get -> raiting value
      val f: SongRating => Double = (x: SongRating) => x.rating

      if ( userRating == 1 || userRating == 2) {
        //println("111111")
        1000.0
      }

      else if(userRating == 0){
        //println("222222")
        1/(Statistics.bayesianAverage(rateSong.ratings,f,2,3)*3.0)

      }else{
        //println("3333333")
        1/(Statistics.bayesianAverage(rateSong.ratings,f,2,3) * input(rateSong.youtubeId))
      }

    }

  }




}