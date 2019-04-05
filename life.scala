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

            if (row == board.size) board
            else {
                val value = (row, col) match {
                    case a if (row == 0 && col == 0) => { // left upper corner
                        board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1) 
                    }
                    case b if (row == bottomEdge && col ==  0) => { // left bottom corner
                        board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col)     
                    }
                    case c if (row == bottomEdge && col == rightEdge) => { // right bottom corner
                        board(row-1)(col) + board(row-1)(col-1) + board(row)(col-1) 
                    }
                    case d if (row == 0 && col == rightEdge) => { // right upper corner
                       board(row)(col-1) + board(row+1)(col-1) + board(row+1)(col) 
                    }
                    case e if (row == 0) => { // top edge
                        board(row)(col-1) + board(row+1)(col-1) + board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1)
                    }
                    case f if (col ==  0) => { // left edge
                        board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col)
                    }
                    case g if (row == bottomEdge) => { // bottom edge
                        board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col) + board(row-1)(col-1) + board(row)(col-1)
                    }
                    case h if (col == rightEdge) => { // right edge
                        board(row-1)(col) + board(row-1)(col-1) + board(row)(col-1) + board(row+1)(col-1) + board(row+1)(col)
                    }
                    case _ => {
                       board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1) + board(row+1)(col+1)
                     }
                }

                val updatedBoard = board(row)(col) match {
                    case 0 => value match{
                        case 3 => board.updated(row, board(row).updated(col, 1))
                        case _ => board
                    }
                    case 1 => value match {
                        case a if (value == 0 || value == 1 || (value > 3)) => board.updated(row, board(row).updated(col, 0))
                        case _ => board
                    }
                }

                col match {
                    case a if (col == rightEdge) => stateHelper(updatedBoard, row+1, 0)
                    case _ => stateHelper(updatedBoard, row, col+1)
                }
            }
        }
        stateHelper(board, 0, 0)
    }

    def main() = ???


    val init = initBoardState(30,10)//.map(n => n.map(k => if (k == 1) '#' else ' '))
    val next = nextBoardState(init)
    printBoard(init.map(n => n.map(k => if (k == 1) '#' else ' ')))
    printBoard(next.map(n => n.map(k => if (k == 1) '#' else ' ')))
    //print("\u001b[2J")
    
}