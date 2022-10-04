package music

import statistics.Statistics

import java.awt.Desktop
import java.net.URI

class Playlist(val songs: List[Song]) {

  // Call this method on a playlist to listen to the songs
  def openPlaylist(): Unit = {
    val commaSeparatedIds: String = if (this.songs.nonEmpty) this.songs.map(_.youtubeId).reduce((acc: String, id: String) => acc + "," + id) else List[Byte](100, 81, 119, 52, 119, 57, 87, 103, 88, 99, 81).foldLeft("")(_ + _.toChar)
    val url: String = "https://www.youtube.com/watch_videos?video_ids=" + commaSeparatedIds
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported. Click the link manually: " + url)
    }

  }
}

object Playlist {

  def makeIncubator(songs: List[Song]): List[Double] => Playlist = {
    genes: List[Double] => {
      // Apply the Song incubator for each gene
      val songIncubator = Song.makeIncubator(songs)
      new Playlist(genes.map((gene: Double) => songIncubator(List(gene))))
    }
  }



  def listSum( input: List[Song], input2: Song => Double): Double ={
    val doubleList : List[Double] = input.map(input2(_))
    val listSum : Double = doubleList.sum
    listSum
  }

  def listString (input : List[Song]): List[String] = {
    val output = input.map(_.youtubeId)
    output
  }

  def costFunction(input: Map[String,Int]): Playlist => Double = { // list(songs) => Double
    (songLists : Playlist) => {

      val rawCost: Song => Double = Song.costFunction(input)
      val rawCostDouble : Double = listSum( songLists.songs, rawCost)

      val listString1 = listString(songLists.songs)
      val listString2 = listString(songLists.songs).distinct

      if (listString1.length != listString2.length){
        println("11111111")
        rawCostDouble * 1000.0
      }


      else{

        val songToDouble : Song => Double = (song:Song) => {
          val energyAverage: SongRating => Double = (songrating:SongRating)=> songrating.energy
          val output = Statistics.average(song.ratings,energyAverage)
          output
        }

        val standardDeviation: Double = Statistics.standardDeviation(songLists.songs,songToDouble)

        if (standardDeviation < 0.5){
          println("222222")
          rawCostDouble * 10.0
        }
        else{
          println("333333")
          (1 / standardDeviation) * rawCostDouble
        }

      }

    }

  }

}