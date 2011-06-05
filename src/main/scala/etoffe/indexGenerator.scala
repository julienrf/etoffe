package etoffe

trait IndexGenerator {
  def next(): String
}

class NumberGenerator extends IndexGenerator {
  
  private var i = 0: Long
  
  override def next() = {
    i = i + 1
    i.toString
  }
}
