val dictionary = List("eat", "tea", "ate")

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

//val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
//  dictionary.map()
//}
def wordOccurrences(w: Word): Occurrences = {
  val loweredWord = w.map(_.toLower)
  val occurrencesMap = loweredWord.groupBy((c: Char) => c)
  occurrencesMap.toList.map(occ => (occ._1, occ._2.length)).sortWith(_._2 < _._2)
}

def somefunc(x: List[(Occurrences, Word)]): List[Word] = {
  x.map(_._2)
}

dictionary.map((w: Word) => (wordOccurrences(w), w))