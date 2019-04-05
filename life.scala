import scala.util.Random
import scala.annotation.tailrec

object life extends App{

    def initBoardState(width: Int, height: Int): IndexedSeq[IndexedSeq[Int]] = {
        @tailrec
        def stateHelper(col: Int, row: Int, tempOut: IndexedSeq[Int], output: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {
            val value = Random.nextInt(2)

            value match {
                case a if (row == height && col == width) => output // board done
                case b if (col == width) => stateHelper(0, row+1, IndexedSeq.empty[Int], output :+ tempOut) // single row done
                case _ => stateHelper(col+1, row, tempOut :+ value, output) // single fill
            }
        }
        stateHelper(0, 0, IndexedSeq.empty[Int], IndexedSeq.empty[IndexedSeq[Int]])
    }

    def printBoard[A](seq: Seq[Seq[A]]): Unit = seq match {
        case Seq() => return ()
        case Seq(single) => {
            print("\n|" + single.mkString("")+ "|" + "\n\n")
            return ()
        }
        case (head +: tail) => {
            print("\n|" + head.mkString("")+ "|")
            printBoard(tail)
        }
    }

    def nextBoardState(board: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {
        def stateHelper(board: IndexedSeq[IndexedSeq[Int]], row: Int, col: Int): IndexedSeq[IndexedSeq[Int]] = {
            val rightEdge = board.head.size-1
            val bottomEdge = board.size-1 

            val value = (row, col) match {
                case (0,0) => { // left upper corner
                    board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1) 
                }
                case (bottomEdge, 0) => { // left bottom corner
                    board(row)(col+1) + board(row+1)(col+1) + board(row+1)(col)     
                }
                case (bottomEdge, rightEdge) => { // right bottom corner
                    board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1) 
                }
                case (0, rightEdgeCord) => { // right upper corner
                    board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col) 
                }
                case (0, _) => { // top edge
                    board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1)
                }
                case (_, 0) => { // left edge
                    board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1) + board(row+1)(col+1) + board(row+1)(col)
                }
                case (bottomEdge, _) => { // bottom edge
                    board(row)(col+1) + board(row+1)(col+1) + board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1)
                }
                case (_, rightEdge) => { // right edge
                    board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col)
                }
                case (_, _) => {
                    board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1) + board(row+1)(col+1)
                }
            }

            board(row)(col) match {
                case 0 => 
                case 1 => 
            }
        }
    }




    //printBoard(initBoardState(30,10).map(n => n.map(k => if (k == 1) '#' else ' ')))
    //print("\u001b[2J")
    
}