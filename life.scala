import scala.util.Random

object life extends App{

    def initBoardState[A](width: Int, height: Int): IndexedSeq[IndexedSeq[Int]] = {
        def stateHelper(col: Int, row: Int, tempOut: IndexedSeq[Int], output: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {
            val value = Random.nextInt(2)

            row match {
                case a if (row == height && col == width) => output
                case b if (col == width) => stateHelper(1, row+1, IndexedSeq.empty[Int], output :+ tempOut)
                case _ => stateHelper(col, row, tempOut :+ value, output)
            }
        }
        stateHelper(1, 1, IndexedSeq.empty[Int], IndexedSeq.empty[IndexedSeq[Int]])
    }

    val a = initBoardState(5,5)

    print(a.mkString("\n"))
    
}