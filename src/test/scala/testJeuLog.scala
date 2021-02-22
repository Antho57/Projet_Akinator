import objAkinator._
import org.scalatest._

class testJeuLog extends FunSuite{

  test("jeuLog qui affiche que des o") {
    val l = jeuLog(Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))
      , "o\no\no\no\n".linesIterator)

    val rep = "o,o,o,o".split(",");
    assert(rep === l)
  }


  test("jeuLog qui affiche des o et des n") {
    val l = jeuLog(Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))
      , "o\nn\no\nn\n".linesIterator)

    val rep = "o,n,o,n".split(",");
    assert(rep === l)
  }
}