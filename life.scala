import scala.util.Random
import scala.annotation.tailrec
import scala.io.Source

object life extends App{

    def boardFromFile(filePath: String): Vector[Vector[Int]] = {
        val boardFile = Source.fromFile(filePath).getLines
        val convertedBoard = boardFile.foldLeft (Vector.empty[Vector[Int]]) ( (acc: Vector[Vector[Int]], curr: String) => acc :+ curr.toVector.map(n => n match {
            case '.' => 0
            case 'O' => 1
        } ))
        val preCleanupBoard = convertedBoard.filter(n => !(n.isEmpty))
        val maxWidth = preCleanupBoard.map(_.size).reduce(_ max _)
        preCleanupBoard.foldLeft (Vector.empty[Vector[Int]]) ( (acc: Vector[Vector[Int]], curr: Vector[Int]) => acc :+ curr.padTo(maxWidth, 0) )
    }

    def initBoardState(width: Int, height: Int): Vector[Vector[Int]] = {
        def stateHelper(col: Int, row: Int, tempOut: Vector[Int], output: Vector[Vector[Int]]): Vector[Vector[Int]] = (row, col) match {
            case (row, col) if (row == height && col == width) => output // board done
            case (row, col) if (col == width) => stateHelper(0, row+1, Vector.empty[Int], output :+ tempOut) // single row done
            case _ => {
                val value = Random.nextInt(2)
                stateHelper(col+1, row, tempOut :+ value, output) // single fill
            }
        }
    stateHelper(0, 0, Vector.empty[Int], Vector.empty[Vector[Int]])
    }

    def nextBoardState(board: Vector[Vector[Int]]): Vector[Vector[Int]] = {
        def stateHelper(board: Vector[Vector[Int]], nextStateBoard: Vector[Vector[Int]], row: Int, col: Int): Vector[Vector[Int]] = row match {
            case a if (row == board.size) => nextStateBoard
            case _ => {
                val rightEdge = board.head.size-1
                val bottomEdge = board.size-1 
                val value = (row, col) match {
                    case (row, col) if (row == 0 && col == 0) => { // left upper corner
                        board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1) 
                    }
                    case (row, col) if (row == bottomEdge && col ==  0) => { // left bottom corner
                        board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col)     
                    }
                    case (row, col) if (row == bottomEdge && col == rightEdge) => { // right bottom corner
                        board(row-1)(col) + board(row-1)(col-1) + board(row)(col-1) 
                    }
                    case (row, col) if (row == 0 && col == rightEdge) => { // right upper corner
                        board(row)(col-1) + board(row+1)(col-1) + board(row+1)(col) 
                    }
                    case (row, col) if (row == 0) => { // top edge
                        board(row)(col-1) + board(row+1)(col-1) + board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1)
                    }
                    case (row, col) if (col ==  0) => { // left edge
                        board(row+1)(col) + board(row+1)(col+1) + board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col)
                    }
                    case (row, col) if (row == bottomEdge) => { // bottom edge
                        board(row)(col+1) + board(row-1)(col+1) + board(row-1)(col) + board(row-1)(col-1) + board(row)(col-1)
                    }
                    case (row, col) if (col == rightEdge) => { // right edge
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
    val freshBoard= board.map(n => n.map(k => 0))
    stateHelper(board, freshBoard, 0, 0)
    }

    def printBoard[A](seq: Seq[Seq[A]]): Unit = seq.foreach(n => print("\n" + n.mkString("")))

    def runLife(board: Vector[Vector[Int]]): Unit = {
        print("\u001b[2J") // clear screen char

        val formattedBoard = board.map(n => n.map(k => if (k == 1) '#' else ' '))
        printBoard(formattedBoard)

        Thread.sleep(150)

        runLife(nextBoardState(board))
    }

    val init = boardFromFile("cos.txt")
    //val init = initBoardState(10,10)
    val board = nextBoardState(init)
    //println(init)
    runLife(board)

    
}