import objAkinator._
import org.scalatest._

import java.io.FileNotFoundException

class testABanimalToFichier extends FunSuite{

  test("ABanimalToFichier crée un fichier et FichierToABaniaml vérifie le fichier créé") {
    val a = Question("Est-ce qu'il a des ailes ?", Question("Est-ce qu'il a des plumes ?", Question("Est-ce qu'il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Question("Est-ce qu'il a des poils ?", Animal("Chauve-souris"), Animal("Ptérodactyle"))), Animal("Chien"))
    ABanimalToFichier("Test.txt", a)
    val a2 = fichierToABanimal("Test.txt")

    assert(a2 === a)
  }
}