package main

import scala.io._

class Main extends App {

  val result = for {
    fileName <- extractArg(args,0)
    filePath <- extractArg(args, 1)
    lines <- readLines(fileName,filePath)
  } yield lines

  result.fold(
    e => s"Error: $e",
    v => s"Result: $v"
  )


  def readLines(fileName:String, filePath:String): Either[Throwable,List[String]] = {
    try{
      val fileLines = for {
        lines <- Source.fromFile(filePath+fileName).getLines.toList
      }yield lines
      Right(fileLines)
    }
    catch{
      case exc:Exception => Left(exc)
    }

  }

  def extractArg(args: Array[String], index: Integer): Either[Throwable, String] = {
    try{
      Right(args(index))
    }catch{
      case iob: IndexOutOfBoundsException => Left(iob)
      case e: Exception => Left(e)
    }
  }

  def argToInt(arg: String): Either[Throwable, Int] = {
    try{
      Right(arg.toInt)
    }catch{
      case ex: Exception => Left(ex)
    }
  }

}
