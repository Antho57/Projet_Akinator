import objAkinator._
import org.scalatest._

class testJeuSimple extends FunSuite{

  test("jeuSimple qui trouve la bonne réponse") {
    assert(jeuSimple(Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))
      , "o\no\no\no\n".linesIterator))
  }

  test("jeuSimple qui ne trouve pas la bonne réponse") {
    assert(!jeuSimple(Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))
      , "o\no\no\nn\n".linesIterator))
  }


}
