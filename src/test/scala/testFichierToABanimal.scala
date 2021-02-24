import objAkinator._
import org.scalatest._

import java.io.FileNotFoundException

class testFichierToABanimal extends FunSuite{

  test("fichierToABanimal qui lis, traite et affiche le fichier") {
    val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Animal("Chien"))
    val a2 = jeuApprentissage(a, "n\nn\nChat\nEst-ce qu'il ronronne ?\no\n".linesIterator)
    val result = fichierToABanimal("Arbre.txt")

    assert(a2 === result)
  }

  test("fichierToABanimal qui trouve pas le fichier") {
    intercept[FileNotFoundException] {
      val result = fichierToABanimal("blabla.txt")
    }
  }

}