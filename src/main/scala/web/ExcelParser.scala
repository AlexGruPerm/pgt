package web

import org.apache.poi.ss.usermodel.{DataFormatter, Row, Sheet, WorkbookFactory}
import tmodel._
import zio.json.DecoderOps
import zio.{Task, ZIO}

import scala.jdk.CollectionConverters._
import java.io.File
import tmodel.EncDecTestModelImplicits._

import java.nio.file.Path

case class InvalidExcelHeader(msg: String) extends Exception
case class InvalidSheetName(msg: String) extends Exception
case class EmptyMeta(msg: String) extends Exception

case class ExcelTestsHeader(
                             id: String,
                             name: String,
                             call_type: String,
                             ret_type: String,
                             use_commit: String,
                             call: String,
                             success_condition: String,
                           ) {
  def debugHeader: Unit =
    println(
      s"""
         |id = $id
         |name = $name
         |call_type = $call_type
         |ret_type = $ret_type
         |use_commit = $use_commit
         |call = $call
         |success_condition = $success_condition
         |""".stripMargin)

  def isInvalid: Boolean = {
    debugHeader
    !(id == "id" &&
      name == "name" &&
      call_type == "call_type" &&
      ret_type == "ret_type" &&
      use_commit == "use_commit" &&
      call == "call" &&
      success_condition == "success_condition")
  }
}

object ExcelTestsHeader {
  def apply(rowHeader: Row): ExcelTestsHeader =
    ExcelTestsHeader(
      id = rowHeader.getCell(0).getStringCellValue,
      name = rowHeader.getCell(1).getStringCellValue,
      call_type = rowHeader.getCell(2).getStringCellValue,
      ret_type = rowHeader.getCell(3).getStringCellValue,
      use_commit = rowHeader.getCell(4).getStringCellValue,
      call = rowHeader.getCell(5).getStringCellValue,
      success_condition =rowHeader.getCell(6).getStringCellValue
    )
}

object ExcelParser {

  private def checkHeader(rowHeader: Row): ZIO[Any, InvalidExcelHeader, Unit] =
    ZIO.when(ExcelTestsHeader(rowHeader).isInvalid)(
      ZIO.fail(InvalidExcelHeader("Не корректные названия в заголовке"))
    ).unit

  private def checkSheetName(sheet: Sheet): ZIO[Any, InvalidSheetName, Unit] =
    ZIO.when(sheet.getSheetName != "tests")(
      ZIO.fail(InvalidSheetName("Лист с тестами должен называться test"))
    ).unit

  private def parseMeta(cellMeta: String): ZIO[Any,EmptyMeta,TestsMeta] = for {
    _ <- ZIO.when(cellMeta.isEmpty)(
      ZIO.fail(EmptyMeta("Ячейка с подключением к БД пустая или не корректная"))
    )
    tm <- cellMeta.fromJson[TestsMeta] match {
      case Right(meta) => ZIO.succeed(meta)
      case Left(e) => ZIO.fail(EmptyMeta(s"Ошибка парсинга меты БД: $e"))
    }
  } yield tm

  private def parseSheetRows(sheet: Sheet): Task[Option[List[Test]]] =
    ZIO.attempt {
      val formatter = new DataFormatter()
      val rows = sheet.iterator().asScala.toList
      if (rows.isEmpty) None
      else {
        val dataRows = rows.tail.tail // пропускаем мету и заголовок
        // Фильтруем пустые строки
        val nonEmptyRows = dataRows.filter { row =>
          val cells = (0 until 7).map { c =>
            val cell = row.getCell(c)
            val value = if (cell == null) "" else formatter.formatCellValue(cell)
            value.trim
          }
          cells.exists(_.nonEmpty) // строка не пустая, если хотя бы одна ячейка не пустая
        }
        Some(nonEmptyRows.map { row =>
          val cells: Map[Int, String] = (0 until 7).map { c =>
            val cell = row.getCell(c)
            val value = if (cell == null) "" else formatter.formatCellValue(cell)
            (c, value)
          }.toMap
          Test(
            id = {
              val idStr = cells.getOrElse(0, "").trim
              if (idStr.isEmpty) 0 else idStr.toInt
            },
            name = cells.getOrElse(1, ""),
            call_type = cells.getOrElse(2, "").trim match {
              case "select" => Select
              case "function" => Function
              case "select_function" => Select_function
              case "func_inout_cursor" => Func_inout_cursor
              case "dml_sql" => Dml_sql
              case _ => Select
            },
            ret_type = cells.getOrElse(3, "").trim match {
              case "cursor" => Cursor
              case "dataset" => Dataset
              case "integer_value" => Integer_value
              case "affected_rows" => Affected_rows
              case "" => Unknown
              case _ => Unknown
            },
            use_commit = cells.get(4).map(_.trim) collect {
              case "true" => true
              case "false" => false
            },
            call = cells.getOrElse(5, ""),
            success_condition = {
              val scStr = cells.getOrElse(6, "").trim
              if (scStr.nonEmpty) {
                scStr.fromJson[List[SucCondElement]] match {
                  case Right(list) => Some(list)
                  case Left(error) =>
                    println(s"JSON parse error for test id ${cells.getOrElse(0, "")}: $error")
                    None
                }
              } else None
            }
          )
        })
      }
    }


  def parse(excelPath: Path): ZIO[Any, Throwable, TestModel] =
    ZIO.scoped {
      for {
        wb     <- ZIO.fromAutoCloseable(
          ZIO.attempt(WorkbookFactory.create(excelPath.toFile))
        )
        sheet  <- ZIO.attempt(wb.getSheetAt(0))
        _      <- checkSheetName(sheet)
        testMeta <- ZIO.attempt(
          sheet.getRow(0).getCell(1).getStringCellValue
        ).flatMap(parseMeta)
        _      <- checkHeader(sheet.getRow(1))
        list   <- parseSheetRows(sheet)
      } yield TestModel(testMeta, list)
    }

  /*
  def parse(excelPath: String): ZIO[Any, Throwable, TestModel] = for {
    file <- ZIO.attempt(new File(excelPath))
    wb <- ZIO.attempt(WorkbookFactory.create(file))
    sheet <- ZIO.attempt(wb.getSheetAt(0))
    _ <- checkSheetName(sheet)
    testMeta <- parseMeta(sheet.getRow(0).getCell(1).getStringCellValue)
    _ <- checkHeader(sheet.getRow(1))
    listTestOpt <- parseSheetRows(sheet)
  } yield TestModel(testMeta,listTestOpt)
*/


}
