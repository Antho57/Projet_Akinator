import objAkinator._
import org.scalatest._

class testJeuApprentissage extends FunSuite{

  test("jeuApprentissage qui apprends la différence entre chat et chien") {
    val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Animal("Chien"))

    val result = jeuApprentissage(a, "n\nn\nChat\nEst-ce qu'il ronronne?\no\n".linesIterator)

    val rep = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne?",Animal("Chat"),Animal("Chien")));
    assert(rep === result)
  }


  test("jeuApprentissage qui apprends rien") {
    val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Animal("Chien"))

    val result = jeuApprentissage(a, "n\no\n".linesIterator)

    assert(a === result)
  }
}