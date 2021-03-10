import scala.io.Source
import java.io._
import java.util.Scanner

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



  def jeuApprentissageFinal(a: ABanimal): ABanimal = a match {
    case Animal(v) => System.out.println("Pensez-vous à : " +v +" ?")
      val sc = new Scanner(System.in)
      if (sc.nextLine() == "o"){
        System.out.println("J'ai gagné !!!\n");
        a
      }else {
        System.out.println("J'ai perdu - quelle est la bonne réponse ?")
        val nom = sc.nextLine()
        val tmp = new Animal(nom)
        System.out.println("Quelle question permet de différencier " +nom +" de " +v +" ?")
        val qtmp = sc.nextLine()
        System.out.println("Quelle est la réponse à cette question pour " +nom)
        if (sc.nextLine() == "o") new Question(qtmp, tmp, a)
        else new Question(qtmp, a, tmp)
      }
    case Question(q, oui, non) => System.out.println(q)
      val sc = new Scanner(System.in)
      if (sc.nextLine() == "o") new Question(q, jeuApprentissageFinal(oui), non)
      else new Question(q, oui, jeuApprentissageFinal(non))
  }



  def fichierToABanimal(nomf: String): ABanimal = {
    try {
      val fichier = Source.fromFile("src/main/scala/" +nomf)
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

  def ABanimalToFichier(nomf:String, a:ABanimal): Unit ={
    val writer = new FileWriter(new File("src/main/scala/" +nomf))
    def aux(w: FileWriter, a: ABanimal): Unit =a match{
      case Animal(n) => w.write(n+"\n")
      case Question(q, oui, non) => w.write("q :" +q +"\n")
        aux(w, oui)
        aux(w, non)
    }
    aux(writer, a)
    writer.close()
  }


  def jeuSimpleJNSP(a:ABanimal, it:Iterator[String], l:List[ABanimal]=Nil):Boolean= a match {
    case Animal(v) => if (it.next() == "o") true else if (l.isEmpty) false else jeuSimpleJNSP(l.head, it, l.tail)
    case Question(_, oui, non) => val tmp = it.next()
      if (tmp == "o") jeuSimpleJNSP(oui, it, l)
      else if (tmp == "n") jeuSimpleJNSP(non, it, l)
      else jeuSimpleJNSP(oui, it, non::l)
  }


  def main(args: Array[String]) {
    System.out.println("<------ Bienvenue dans AKINATOR ------>" +"\n\n");
    System.out.println("Avant de commencer vous devez penser à un animal !" +"\n\n"
      +"Vous devez répondre aux questions par 'o' pour oui et 'n' pour non !" +"\n\n"
      +"Êtes-vous prêts ?" +"\n");
    val sc = new Scanner(System.in)
    if (sc.nextLine() == "o") {
      var rep = jeuApprentissageFinal(fichierToABanimal("Arbre.txt"));
      System.out.println("Voulez-vous rejouer ?")
      while (sc.nextLine() == "o") {
        rep = jeuApprentissageFinal(rep);
        System.out.println("Voulez-vous rejouer ?")
      }
      System.out.println("Merci d'avoir joué ;)")
      ABanimalToFichier("Arbre.txt", rep)
    }






    /*println(jeuSimple(a, "o\no\no\no\n".linesIterator))
    println(jeuLog(a, "o\nn\no\nn\n".linesIterator))
    println(jeuApprentissage(a, "n\nn\nChat\nEst-ce qu'il ronronne ?\no\n".linesIterator))
    println(fichierToABanimal("Arbre.txt"))
    println(jeuSimpleJNSP(a, "o\nx\nx\nn\nn\no\no\n".linesIterator))*/
  }
}