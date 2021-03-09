
def triangle(n: Int): Unit = {
  def au(a:Int, acc:String): Unit ={
    if(a >= 0) {
      au(a-1,(acc + "*"))
      println(acc)
    }
  }
  au(n,"")
}

triangle(7);