import objAkinator._
import org.scalatest._

class testJeuSimpleJNSP extends FunSuite{

  test("jeuSimpleJNSP avec une reponse JNSP") {
    assert(jeuSimpleJNSP(Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Question("Est-ce qu'il ronronne ?", Animal("Chat"), Animal("Chien")))
      , "o\nx\nx\nn\nn\no\no\n".linesIterator))
  }


}
