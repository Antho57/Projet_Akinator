package animal

object objAkinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal


  val a = Question("Est-ce qu'il a des ailes ?",
    Question("Est-ce qu'il a des plumes ?",
      Question("Est-ce qu'il a un goitre ?",
        Animal("Pélican"), Animal("Pigeon")),
      Question("Est-ce qu'il a des poils ?",
        Animal("Chauve-souris"), Animal("Ptérodactyle"))),
    Question("Est-ce qu'il ronronne ?",
      Animal("Chat"), Animal("Chien")))

}
