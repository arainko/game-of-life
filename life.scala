import scala.util.Random
import scala.annotation.tailrec

object life extends App{

    def initBoardState(width: Int, height: Int): IndexedSeq[IndexedSeq[Int]] = {
        @tailrec
        def stateHelper(col: Int, row: Int, tempOut: IndexedSeq[Int], output: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = (row, col) match {
            case a if (row == height && col == width) => output // board done
            case b if (col == width) => stateHelper(0, row+1, IndexedSeq.empty[Int], output :+ tempOut) // single row done
            case _ => {
                val value = Random.nextInt(2)
                stateHelper(col+1, row, tempOut :+ value, output) // single fill
            }
        }
    stateHelper(0, 0, IndexedSeq.empty[Int], IndexedSeq.empty[IndexedSeq[Int]])
    }

    @tailrec
    def printBoard[A](seq: Seq[Seq[A]]): Unit = seq match {
        case Seq() => return ()
        case Seq(single) => {
            print("\n" + single.mkString("") + "\n\n")
            return ()
        }
        case (head +: tail) => {
            print("\n" + head.mkString(""))
            printBoard(tail)
        }
    }

    def nextBoardState(board: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {
        @tailrec
        def stateHelper(board: IndexedSeq[IndexedSeq[Int]], nextStateBoard: IndexedSeq[IndexedSeq[Int]], row: Int, col: Int): IndexedSeq[IndexedSeq[Int]] = row match {
            case a if (row == board.size) => nextStateBoard
            case _ => {
                val rightEdge = board.head.size-1
                val bottomEdge = board.size-1 
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
                    case _ => { // everything else
                        board(row+1)(col) + board(row+1)(col-1) + board(row)(col-1) + board(row-1)(col-1) + board(row-1)(col) + board(row-1)(col+1) + board(row)(col+1) + board(row+1)(col+1)
                    }
                }    

                val updatedBoard = board(row)(col) match {
                    case 0 => value match {
                        case 3 => nextStateBoard.updated(row, nextStateBoard(row).updated(col, 1))
                        case _ => nextStateBoard
                    }
                    case 1 => value match {
                        case a if ((value < 2) || (value > 3)) => nextStateBoard.updated(row, nextStateBoard(row).updated(col, 0))
                        case _ => nextStateBoard.updated(row, nextStateBoard(row).updated(col, 1))
                    }
                }

                col match {
                    case a if (col == rightEdge) => stateHelper(board, updatedBoard, row+1, 0)
                    case _ => stateHelper(board, updatedBoard, row, col+1)
                }
            }
        }
        val nextBoard= board.map(n => n.map(k => 0))  // fresh board
        stateHelper(board, nextBoard, 0, 0)
    }

    def runLife(board: IndexedSeq[IndexedSeq[Int]]): Unit = {
        print("\u001b[2J")
        printBoard(board.map(n => n.map(k => if (k == 1) '#' else ' ')))
        Thread.sleep(150)
        runLife(nextBoardState(board))
    }

    val init = initBoardState(60,20)
    val board = nextBoardState(init)

    runLife(board)

    
}