package statistics

object  Statistics {

  def average[T](data: List[T], f: T => Double): Double = {
    val doubleList : List[Double] = data.map(f)
    val listSum : Double = doubleList.sum
    val listLength : Int = doubleList.length
    val output: Double = listSum / listLength
    output
  }

  def topK[T](data: List[T], f: T => Double, k: Int): List[T] = {
    val numbersSorted = data.sortWith((a: T, b: T) => f(a) > f(b))
    val output = numbersSorted.slice(0,k)
    output
  }

  def standardDeviation[T](data: List[T], f: T => Double): Double = {
    val doubleList : List[Double] = data.map(f)
    val doubleAverage: Double = average(data,f)
    val eachDiff: List[Double] = doubleList.map(_ - doubleAverage)
    val numbersSquared: List[Double] = eachDiff.map(Math.pow(_, 2.0))
    val listSum : Double = numbersSquared.sum
    val listLength : Int = doubleList.length
    val division: Double = listSum / listLength
    val output : Double = Math.sqrt(division)
    output
  }

  def bayesianAverage[T](data: List[T], f: T => Double, fakeNumber:Int, fakeValue:Int): Double = {
    val doubleList : List[Double] = data.map(f)
    val listSum : Double = doubleList.sum
    val totalSum : Double = listSum + fakeNumber * fakeValue
    val totalLength : Int = doubleList.length + fakeNumber
    val output = totalSum / totalLength
    output
  }




}
//lab note
//LT1
//My_list = List(5.0, 2.0, 3.0)
//F= ((x: Double) =>x)
//Statistics.average[Double](My_list,F)


//My_list = List("cat", "dog", "elephant", "tiger) F= ((x: String) => x.length.toDouble )
//Statistics.average[String](My_list,F)


//My_list = List(6.0, 7.0, 3.0, 4.0, 5.0, 8.0, 1.0, 2.0)
//F: (T => Double ) = ((listDouble: Double) => listDouble)
//Returned = topK(My_list, F, 4)

//My_list = List(“two”, “three”, “four”, “five”, “six”, ”seven”, “eight”, “cat”)
//F = ((last2: String) => last2.length.toDouble)
//returnrd = topK(My_list, F, 2)

//LT2
//My_list = List(3.0, 5.0, 1.0, 2.0)

//F(T->Double) = (histDouble: Double)=> listDouble)
// Retured = standardDeviation(My_list, F) assert(returned == 1

//LT3
//Testing:
//rating1 = new songRating(5, 2)
//rating2 = new songRating(4, 3)
//song = New Song ('hello", adele, laksjd;lfka;lfdka;ldsk, List(rating1, rating2)
//new_rating =new songRating(3, 1)
//newSong = song.addRating(new_rating)
//Title = "hello' Artist = "adele
//youtubelD = laksjd,;llka;lfrdka;ldsk
//I Ratings = List(rating1, rating2, new_rating)

//LT4
//create ratings for the Song Song = new Song( asdtkslkdilskdif, "insert ratings heret)

//myMap = Map("asdfkslkdilskdif"-> 4, qowieurowieurowieur"->5)
//Cost = costFunction(myMap) Song=>Double costof asdfkslkdjskdjf = Cost(Song) //returns a Double

//Def costFunction(map: Map[Sstring, Int): Song=>Double {}

//LT5 and LT6
//We just went through it very quickly and we didn't have a specific example but we did a very detailed description of what they mean.
