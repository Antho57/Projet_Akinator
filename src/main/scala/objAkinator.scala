import java.io.FileNotFoundException

object objAkinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal


  val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Animal("Chien"))

  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Animal(v) => if (it.next() == "o") true else false
    case Question(_, oui, non) => if (it.next() == "o") jeuSimple(oui, it) else jeuSimple(non, it)
  }

  def jeuLog(a: ABanimal, it: Iterator[String]): List[String]= a match{
    case Animal(_) => List(it.next())
    case Question(_, oui, non) => val c = it.next()
      if (c == "o") c::jeuLog(oui, it)
      else c::jeuLog(non, it)
  }

  def jeuApprentissage(a: ABanimal, it: Iterator[String]): ABanimal = a match {
    case Animal(v) => if (it.next() == "o") a else {
      val tmp = new Animal(it.next())
      val qtmp = it.next()
      if (it.next() == "o") new Question(qtmp, tmp, a) else new Question(qtmp, a, tmp)
    }
    case Question(q, oui, non) => if (it.next() == "o") new Question(q, jeuApprentissage(oui, it), non) else new Question(q, oui, jeuApprentissage(non, it))
  }

  import scala.io.Source
  def fichierToABanimal(nomf: String): ABanimal = {
    try {
      val fichier = Source.fromFile(nomf)
      val f = fichier.getLines().toList.iterator
      fichier.close()
      def aux(f: Iterator[String]): ABanimal = {
        val nextVal = f.next()
        if (nextVal.startsWith("q :")) new Question(nextVal.slice(3, nextVal.length), aux(f), aux(f))
        else new Animal(nextVal)
      }
      aux(f)
    }
    catch {
      case e: FileNotFoundException => throw new FileNotFoundException("Fichier introuvable")
    }
  }

  def main(args: Array[String]) {
    println(jeuSimple(a, "o\no\no\no\n".linesIterator))
    println(jeuLog(a, "o\nn\no\nn\n".linesIterator))
    println(jeuApprentissage(a, "n\nn\nChat\nEst-ce qu'il ronronne ?\no\n".linesIterator))
    println(fichierToABanimal("src/main/scala/Arbre.txt"))
  }
}
