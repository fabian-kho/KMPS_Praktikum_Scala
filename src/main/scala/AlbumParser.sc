import scala.io.Source
import java.io.FileNotFoundException
import java.util.concurrent.locks.Condition
import scala.annotation.tailrec

case class Track(
                  title: String,
                  length: String,
                  rating: Int,
                  features: List[String],
                  writers: List[String]
                )

case class Album(
                  title: String,
                  date: String,
                  artist: String,
                  tracks: List[Track]
                )

/**
 * Read file and return char list or null if failed
 *
 * @param source
 * @return
 */
def getTokenList(source: String): Either[List[Char], Null] = source match {
  case "" => null
  case _ =>
    try {
      val buffer = Source.fromFile(source)
      val tokenList = buffer.toList
      buffer.close
      Left(tokenList)
    } catch {
      case ioe: FileNotFoundException =>
        print(ioe)
        null
    }

}

def path: String = "/Users/fabiankirchhoff/Documents/Uni/Semester_5/KMPS/ Teil 1 - Prof. Fassbender Funktionale Programmierung/Praktikum und Tutorium/Praktikum 2/alben.xml"

val tokenList = getTokenList(path) match {
  case Left(result) => result
  case Right(value) => null
}

/**
 * Find the first given character in list
 *
 * @param haystack
 * @param needle
 * @param currentIndex
 * @return
 */
@tailrec
def getIndexOfChar(haystack: List[Char], needle: Char, currentIndex: Int): Int = haystack(currentIndex) == needle match {
  case false => getIndexOfChar(haystack, needle, currentIndex + 1)
  case true => currentIndex
}

/**
 * Get string between start and end index and delete special chars
 *
 * @param haystack
 * @param current
 * @param end
 * @param stack
 * @return
 */
def getStringToIndex(haystack: List[Char], current: Int, end: Int, stack: String): String = current == end match {
  case false => haystack(current) match {
    case '\n' | '\r' | '\t' => null
    case '>' => getStringToIndex(haystack, current + 1, end, stack) //filter > out
    case _ => stack + haystack(current) + getStringToIndex(haystack, current + 1, end, stack)
  }
  case true => stack
}

/**
 * iterate over tokenList (char) and pars into tokenList (string)
 *
 * @param tokenList
 * @return
 */
def createTokenList(tokenList: List[Char]): List[String] = tokenList match {
  case Nil => Nil
  case _ :: Nil => Nil
  case head :: tail => head match {
    case '<' =>
      val index = getIndexOfChar(tail, '>', 1) // find index of needle
      val token = getStringToIndex(tail, 0, index + 1, "") // parse string
      token match {
        case null => createTokenList(tail) // catch special char returns null, continue with tail
        case _ => token :: createTokenList(tail)
      }
    case '>' =>
      val index = getIndexOfChar(tail, '<', 1)
      val token = getStringToIndex(tail, 0, index, "")
      token match {
        case null => createTokenList(tail) // catch special char returns null, continue with tail
        case _ => token :: createTokenList(tail)
      }
    case _ => createTokenList(tail)
  }
}


/**
 * parse track
 *
 * @param list
 * @param currentTrack
 * @return
 */
@tailrec
def parseTrack(list: List[String], currentTrack: Track): (Track, List[String]) = list match {
  case Nil => null
  case x :: Nil => (currentTrack, x :: Nil)
  case head :: x :: tail => head match {
    // Track is completed
    case "/track" => (currentTrack, head :: x :: tail)

    // Parse title of track
    case "title" =>
      val trackCopy = currentTrack.copy(title = x)
      parseTrack(tail, trackCopy)

    // Parse length of track
    case "length" =>
      val trackCopy = currentTrack.copy(length = x)
      parseTrack(tail, trackCopy)

    // Parse rating of track
    case "rating" =>
      val trackCopy = currentTrack.copy(rating = x.toInt)
      parseTrack(tail, trackCopy)

    // Parse feature of track, and add to track features list
    case "feature" =>
      val trackCopy = currentTrack.copy(features = currentTrack.features :+ x)
      parseTrack(tail, trackCopy)

    // Parse feature of track, and add to track writing list
    case "writing" =>
      val trackCopy = currentTrack.copy(writers = currentTrack.writers :+ x)
      parseTrack(tail, trackCopy)

    // Continue iterate
    case _ => parseTrack(x :: tail, currentTrack)
  }

}

/**
 * parse album
 *
 * @param list
 * @param currentAlbum
 * @param inTrack
 * @return
 */
@tailrec
def parseAlbum(list: List[String], currentAlbum: Album, inTrack: Boolean = false): (Album, List[String]) = list match {
  case Nil => null
  case x :: Nil => (currentAlbum, Nil)
  case x :: Nil => (currentAlbum, x :: Nil)
  case head :: x :: tail => inTrack match {
    // Album attributes (inTrack flag)
    case false => head match {
      // Album is completed
      case "/album" => (currentAlbum, x :: tail)
      // Parse track and change the inTrack flag
      case "track" =>
        val (track, list) = parseTrack(x :: tail, Track("", "", 0, List(), List()))
        val albumCopy = currentAlbum.copy(tracks = currentAlbum.tracks :+ track)
        parseAlbum(x :: tail, albumCopy, !inTrack)

      // Parse title of album
      case "title" =>
        val albumCopy = currentAlbum.copy(title = x)
        parseAlbum(tail, albumCopy, inTrack)

      // Parse artist of album
      case "artist" =>
        val albumCopy = currentAlbum.copy(artist = x)
        parseAlbum(tail, albumCopy, inTrack)

      // Parse date of album
      case "date" =>
        val albumCopy = currentAlbum.copy(date = x)
        parseAlbum(tail, albumCopy, inTrack)

      // Continue iterate
      case _ => parseAlbum(x :: tail, currentAlbum, inTrack)
    }
    case true => head match {
      // End of track attributes was reached, change inTrack flag
      case "/track" =>
        parseAlbum(x :: tail, currentAlbum, !inTrack)

      // Continue iterate
      case _ => parseAlbum(x :: tail, currentAlbum, inTrack)
    }
  }
}

/**
 * parse tokenlist into albums
 *
 * @param tokenList
 * @return
 */
def parseFile(tokenList: List[String]): List[Album] = tokenList match {
  case Nil => Nil
  case head :: tail => head match {

    // Parse new album
    case "album" =>
      val (album, list) = parseAlbum(tail, Album("", "", "", List()))
      album match {
        // Catch if null was returned
        case null => parseFile(list)

        // Add the album to the album list
        case _ => album +: parseFile(list)
      }

    // Continue iterate
    case _ => parseFile(tail)
  }
}



/**
 * checks if String is a white space
 *
 * @param s
 * @return
 */
def isBlank(s: String): Boolean = s.trim.isEmpty

/**
 * Helper function to print tracks
 *
 * @param track
 * @return
 */
def trackToString(track: Track): String = {
  "\n\tTrack: \n" +
    "\t\t\tTitle: " + track.title + '\n' +
    "\t\t\tLength: " + track.length + '\n' +
    "\t\t\tRating: " + track.rating.toString + '\n' +
    "\t\t\tWriters: " + track.writers.toString() + '\n' +
    "\t\t\tFeatures: " + track.features.toString()
}

/**
 * Helper function to print albums
 * @param album
 * @return
 */
def albumToString(album: Album): String = {
  "\n\nAlbum: \n" +
    "\tTitle: " + album.title + '\n' +
    "\tDate: " + album.date + '\n' +
    "\tArtist: " + album.artist
}

// print parsed file
parseFile(createTokenList(tokenList)).map((a: Album) => {
  print(albumToString(a) + a.tracks.map((t: Track) => trackToString(t)))
})

//print token list with albums and tracks to string with for loop
for (album <- parseFile(createTokenList(tokenList))) {
  print(albumToString(album))
  for (track <- album.tracks) {
    print(trackToString(track))
  }
}


//Aufgabe 1

// a)
def map[A](input_list: List[A], func: A => A): List[A] = input_list match {
  case Nil => Nil
  case head :: tail => func(head) :: map(tail, func)
}

// b)
def albumToUpperCase(albums: List[Album]): List[Album] = {
  map(albums, (album: Album) => album.copy(title = album.title.toUpperCase))
}

def trackToUpperCase(tracks: List[Track]): List[Track] = {
  map(tracks, (track: Track) => track.copy(title = track.title.toUpperCase))
}

albumToUpperCase(parseFile(createTokenList(tokenList))).map((a: Album) => {
  println(albumToString(a) + trackToUpperCase(a.tracks).map((t: Track) => trackToString(t)))
})

// c)
def poly_map[A, B](input_list: List[A], func: A => B): List[B] = input_list match {
  case Nil => Nil
  case head :: tail => func(head) :: poly_map(tail, func)
}


//d)
def getTrackLengths(albums: List[Album]): List[List[String]] = {
  poly_map(albums, (album: Album) => poly_map(album.tracks, (track: Track) => track.length))
}

getTrackLengths(parseFile(createTokenList(tokenList))).map((a: List[String]) => {
  print(a + " / ")
})


// Aufgabe 2
val mkAlbum = parseFile(createTokenList(tokenList))(1)

// a)
def filter[A](input_list: List[A], condition: A => Boolean): List[A] = input_list match {
  case Nil => Nil
  case head :: tail => if (condition(head)) head :: filter(tail, condition) else filter(tail, condition)
}

// b)
filter(mkAlbum.tracks, (track: Track) => track.rating >=4).map((t: Track) => print(trackToString(t)))

// c)
poly_map(filter(mkAlbum.tracks, (track: Track) => track.writers.contains("Rod Temperton")), (t: Track) => print(trackToString(t)))



// Aufgabe 3
// a)
def partition[A](input_list: List[A], condition: A => Boolean): List[List[A]] = {
  def innerPartition(input_list: List[A], condition: A => Boolean, result: List[A]): List[List[A]] = input_list match {
    case Nil => result :: Nil
    case head :: tail => if (condition(head)) result :: innerPartition(tail, condition, List()) else innerPartition(tail, condition, result :+ head)
  }
  innerPartition(input_list, condition, List())
}
print(partition(List('a', 'b', 'c', 'D', 'e', 'f', 'G', 'H', 'i','J'), (c: Char) => c.isUpper))


// b)
print(partition(mkAlbum.tracks, (track: Track) => track.title.contains("Thriller")))

// c)
// filter all white spaces and '<' and '>' from tokenList
//val filteredTokenList = filter(tokenList, (c: Char) => !isBlank(c.toString))
//val newtokenlist = partition(filteredTokenList, (c: Char) => c == '>' || c == '<')
//val result = poly_map(newtokenlist, (l: List[Char]) => l.mkString)
//val res = filter(result, (s: String) => !isBlank(s))

print(filter(
  poly_map(
    partition(
      filter(tokenList, (c: Char) => !isBlank(c.toString)),
      (c: Char) => c == '>' || c == '<'),
    (l: List[Char]) => l.mkString),
  (s: String) => !isBlank(s)))

