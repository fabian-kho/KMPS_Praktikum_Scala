import scala.annotation.tailrec
// Aufgabe 4

def prod(f: Double => Double)(a: Double, b: Double): Double = {
  if (a > b) 1.0
  else f(a) * prod(f)(a + 1, b)
}

prod(x => x)(1, 5)

def div(f: Double => Double)(a: Double, b: Double): Double = {
  if (a > b) 1.0
  else f(a) / prod(f)(a + 1, b)
}

div(x => x)(1, 5)

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a>b) 0
  else f(a)+sum(f)(a+1,b)
}
sum(x => x)( 1, 5)

def calcFoldl(f: (Double,Double) => Double) (a: Double, b: Double): Double = {
  @tailrec
  def innerCalc(a: Double, acc: Double): Double = {
    if (a > b) acc
    else innerCalc(a + 1, f(a, acc))
  }

  if (f(-1, -1) > 0) innerCalc(a, 1)
  else innerCalc(a, 0)
}

calcFoldl((x, y) => y * x)(1, 5)
calcFoldl((x, y) => y / x)(1, 5)

calcFoldl((x, y) => y + x)(1, 5)
calcFoldl((x, y) => y - x)(1, 5)

// b)
// beides

// c)
// bei gleichen Werten wird der Wert returned
// 0 oder 1 würde vlt. Sinn machen habe ich aber jetzt nicht implementiert

// d)
def calcFoldImperativ(f: (Double,Double) => Double)(a: Double, b: Double): Double = {
  if (f(-1,-1) > 0) (a.toInt to b.toInt).map(x => f(x, 1.0)).fold(1.0)((x, y) => x * y)
  else (a.toInt to b.toInt).map(x => f(x, 0.0)).fold(0.0)((x, y) => x + y)
}

calcFoldImperativ((x, y) => y * x)(1, 5)

def calcFoldImperativ(f: (Double,Double) => Double)(a: Double, b: Double): Double = {
  if (f(-1,-1) > 0) (a.toInt to b.toInt).map(x => x.toDouble).fold(1.0)((x, y) => x * y)
  else (a.toInt to b.toInt).map(x => x.toDouble).fold(0.0)((x, y) => x + y)
}