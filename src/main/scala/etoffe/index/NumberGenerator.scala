package etoffe.index

class NumberGenerator extends Generator {
  
  private var i: Long = 0;

  def next(): String = {
    i = i + 1
    i.toString
  }

}