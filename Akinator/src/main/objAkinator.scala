package main

import java.nio.file.Files.lines
import scala.io.Source

object objAkinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal


  val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))

  def jeuSimple(a : ABanimal, it: Iterator[String]): Boolean = a match{
    case Animal(_) => if (it.next()=="o") true else false
    case Question(_, oui, non) =>if (it.next()=="o") jeuSimple(oui, it) else jeuSimple(non, it)
  }

  def main(args: Array[String]) {
    println(jeuSimple(a, "o\no\no\no\n".linesWithSeparators))
  }

}
