package tmodel

import common.types.SessionId
import common.{TestExecutionException, TestExecutionResult}
import data.TestRepoTypes.TestID
import zio.http.template.{Html, bgColorAttr, borderAttr, br, colSpanAttr, css, div, idAttr, pre, table, td, tr, widthAttr, wrapAttr}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

/**
 * to produce JSON from our data we define a JsonEncoder
 * to parse JSON in our Types we use a JsonDecoder
 */
sealed trait CallType
case object Select extends CallType{
  override def toString: String = "select"
}
case object Function extends CallType{
  override def toString: String = "function"
}
case object Select_function extends CallType{
  override def toString: String = "select_function"
}
case object Func_inout_cursor extends CallType{
  override def toString: String = "func_inout_cursor"
}
case object Dml_sql extends CallType{
  override def toString: String = "dml_sql"
}

sealed trait RetType
case object Cursor extends RetType{
  override def toString: String = "cursor"
}
case object Dataset extends RetType{
  override def toString: String = "dataset"
}
case object Integer_value extends RetType{
  override def toString: String = "integer_value"
}
case object Unknown extends RetType{
  override def toString: String = "Unknown"
}
//todo: remove
case object Affected_rows extends RetType{
  override def toString: String = "affected_rows"
}

sealed trait SucCond
case object Rows_gt extends SucCond{
  override def toString: String = "rows >"
}
case object Rows_lt extends SucCond{
  override def toString: String = "rows <"
}
case object Rows_eq extends SucCond{
  override def toString: String = "rows ="
}
case object Rows_ne extends SucCond{
  override def toString: String = "rows <> "
}
case object Exec_time_ms  extends SucCond{
  override def toString: String = "Exec. time (ms.) < "
}
case object Fetch_time_ms extends SucCond{
  override def toString: String = "Fetch time (ms.) < "
}
case object Full_time_ms  extends SucCond{
  override def toString: String = "Full time (ms.) < "
}
case object Fields_exists  extends SucCond{
  override def toString: String = "fields exists "
}
case object Exec_exception  extends SucCond{
  override def toString: String = "is exception exist "
}

case object Intval_gt  extends SucCond{
  override def toString: String = "Intval >"
}
case object Intval_lt  extends SucCond{
  override def toString: String = "Intval <"
}
case object Intval_eq  extends SucCond{
  override def toString: String = "Intval ="
}
case object Intval_ne  extends SucCond{
  override def toString: String = "Intval <>"
}


sealed trait TestState
case object testStateUndefined extends TestState
case object testStateSuccess extends TestState
case object testStateFailure extends  TestState
case object testStateExecuting extends TestState

case class SucCondElement(condition: SucCond,
                          checkValue: Option[Int],
                          fields: Option[List[String]],
                          execResultValue: Option[Long],
                          conditionResult: Option[Boolean]
                         ){

  def check(testRes: TestExecutionResult):SucCondElement = {
    val (checkConditionRes,testResVal): (Boolean,Option[Long]) =
      condition match {
        case Rows_gt       => (testRes.err.isEmpty && checkValue.getOrElse(0) < testRes.rowCount,Some(testRes.rowCount))
        case Rows_lt       => (testRes.err.isEmpty && checkValue.getOrElse(0) > testRes.rowCount,Some(testRes.rowCount))
        case Rows_eq       => (testRes.err.isEmpty && checkValue.getOrElse(0) == testRes.rowCount,Some(testRes.rowCount))
        case Rows_ne       => (testRes.err.isEmpty && checkValue.getOrElse(0) != testRes.rowCount,Some(testRes.rowCount))
        case Intval_gt       => (testRes.err.isEmpty && checkValue.getOrElse(0) < testRes.intVal,Some(testRes.intVal))
        case Intval_lt       => (testRes.err.isEmpty && checkValue.getOrElse(0) > testRes.intVal,Some(testRes.intVal))
        case Intval_eq       => (testRes.err.isEmpty && checkValue.getOrElse(0) == testRes.intVal,Some(testRes.intVal))
        case Intval_ne       => (testRes.err.isEmpty && checkValue.getOrElse(0) != testRes.intVal,Some(testRes.intVal))
        case Exec_time_ms  => (testRes.err.isEmpty && testRes.execMs <= checkValue.getOrElse(0),Some(testRes.execMs))
        case Fetch_time_ms => (testRes.err.isEmpty && testRes.fetchMs <= checkValue.getOrElse(0),Some(testRes.fetchMs))
        case Full_time_ms  => (testRes.err.isEmpty && testRes.totalMs <= checkValue.getOrElse(0),Some(testRes.totalMs))
        case Fields_exists => (testRes.err.isEmpty && fields.getOrElse(List[String]()).forall(testRes.cols.map(cls => cls._1).contains),
          Some(1))
        case Exec_exception => (testRes.err.fold(false)(_ => true),Some(1))
      }
    this.copy(execResultValue = testResVal, conditionResult = Some(checkConditionRes))
  }

  /**
   * remove information about execResultValue and conditionResult
   */
  def uncheck():SucCondElement =
    this.copy(execResultValue = None, conditionResult = None)

}

case class TestsMeta(connect_ip: String,
                     db_name: String,
                     db_user: String,
                     db_password: String) {
  val driver = "org.postgresql.Driver"
  val url: String =  s"jdbc:postgresql://$connect_ip/$db_name?user=$db_user&password=$db_password"
  val urlMsg: String =  s"jdbc:postgresql://$connect_ip/$db_name?user=*****&password=*****"
}

case class Test( id: TestID,
                 name: String,
                 call_type: CallType,
                 ret_type: RetType = Unknown,
                 use_commit: Option[Boolean],
                 call: String,
                 success_condition: Option[List[SucCondElement]],
                 isEnabled: Boolean = false,
                 testState: TestState = testStateUndefined,
                 isExecuted: Boolean = false,
                 testRes: TestExecutionResult = TestExecutionResult(),
                 countOfExecuted: Int = 0
               ){

  private def checkScType(sc: SucCondElement) =
    sc.condition  match {
      case Exec_exception => true
      case _ => false
    }

  private def isExistSuccCondExecException(sc: List[SucCondElement]): Boolean = {
    sc.exists(sc => checkScType(sc))
  }

  private def checkSucCondEcexException(scList: List[SucCondElement]): Option[String] =
    if (isExistSuccCondExecException(scList) && scList.size>1)
      Some("Condition = exec_exception must be single condition in the success_condition list.")
    else None

  private def checkCallTypeRetType(ct: CallType, rt: RetType): Option[String] = {
    ct match {
      case _:Select_function.type =>
        rt match {
          case _:Cursor.type  => None
          case _:Integer_value.type  => None
          case _ => Some(s"TestID=[$id] For call type (select_function) is only applicable ret_type: cursor, integer_value ")
        }
      case _:Select.type =>
        rt match {
          case _: Dataset.type => None
          case _ => Some(s"TestID=[$id] For call type (select) is only applicable ret_type: dataset ")
        }
      case _:Func_inout_cursor.type =>
        rt match {
          case _: Cursor.type => None
          case _ => Some(s"TestID=[$id] For call type (func_inout_cursor) is only applicable ret_type: cursor ")
        }
      /*        case _:Dml_sql.type =>
                rt match {
                  case _: Affected_rows.type => None
                  case _ => Some(s"TestID=[$id] For call type (dml_sql) is only applicable ret_type: affected_rows ")
                }*/
      case _ => None
    }
  }

  val listSC : List[SucCondElement] =  success_condition.getOrElse(List[SucCondElement]())
  val listOfCheckFuncs: List[List[SucCondElement] => Option[String]] = List(checkSucCondEcexException)
  val scErrorText: Option[String] = listOfCheckFuncs.view.flatMap(_(listSC)).headOption
  scErrorText match {
    case Some(err) => throw new Exception(err)
    case None => ()
  }

  val listOfCheckFuncsCtRt: List[(CallType,RetType) => Option[String]] = List(checkCallTypeRetType)
  val ctrtErrorText: Option[String] = listOfCheckFuncsCtRt.view.flatMap(_(call_type,ret_type)).headOption
  ctrtErrorText match {
    case Some(err) => throw new Exception(err)
    case None => ()
  }

  private def getState(checked_success_condition: Option[List[SucCondElement]]): TestState =
    checked_success_condition match {
      case Some(listSucCond) =>
        if (listSucCond.exists(_.conditionResult.getOrElse(false) == false))
          testStateFailure
        else testStateSuccess
      case None => testStateUndefined
    }

  /**
   * Used for analyze executions results in testRes
   * with test's conditions (success_condition).
   */
  def checkConditions: Test = {
    val checked_success_conditions: Option[List[SucCondElement]] =
      success_condition.map(lst => lst.map(sc => sc.check(testRes)))

    val newTestState: TestState = getState(checked_success_conditions)
    //println(s"checkConditions newTestState=${newTestState}")
    this.copy(success_condition = checked_success_conditions,testState = newTestState)// testRes = this.testRes)
  }

  def getTestAsHtml: Html =
    div(css := "div_res",
      table(
        borderAttr := "1px solid black",
        css := (if (testState == testStateFailure)
          "test_state_failure"
        else if (testState == testStateSuccess)
          "test_state_success"
        else
          "test_state_undef" ),
        idAttr := s"table_test_$id",
        testRes.err match {
          case Some(err) =>
            tr(bgColorAttr := "#FF4500;",
              td(
                colSpanAttr:= "2",
                pre(wrapAttr:= "pre-wrap", widthAttr := "200px", s"TYPE = ${err.exceptionType}"),
                pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg),
                (if ((err.exceptionMsg contains "MESSAGE TEXT:") &&
                  (err.exceptionMsg contains "CONTEXT")
                ) {
                  pre(wrapAttr := "pre-wrap", widthAttr := "200px", err.exceptionMsg.substring(
                    err.exceptionMsg.indexOf("MESSAGE TEXT:") + 14,
                    err.exceptionMsg.indexOf("CONTEXT")
                  ))
                } else
                  " "
                  )
              ))
          case None => br()
        },
        tr(
          td(
            colSpanAttr:= "2",
            pre(wrapAttr:= "pre-wrap", widthAttr := "200px", call)
          )),
        tr(
          td(
            colSpanAttr:= "2",
            div(
              s"[${id}] ${name}"
            ),br()
          )
        ),
        tr(
          td(
            colSpanAttr:= "2",
            div(
              s"Test was executed $countOfExecuted times."
            ),br()
          )
        ),
        tr(td("Call type :"),td(call_type.toString)),
        tr(td("Return type :"),td(ret_type.toString)),
        if (use_commit.getOrElse(false))
          tr(bgColorAttr := "#F9E79F",
            td(
              colSpanAttr:= "2",
              div(
                "Commit"
              )
            )
          ) else ()
        ,
        tr(td(colSpanAttr:= "2",div("Call :"))),
        tr(td(colSpanAttr:= "2", pre(wrapAttr:= "pre-wrap", widthAttr := "200px", call) )),
        tr(td(colSpanAttr:= "2","Success conditions:")),
        //table with success conditions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tr(td(colSpanAttr:= "2",
          //condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean]
          table(
            borderAttr := "1px",
            idAttr := s"table_test_conditions_$id",
            tr(
              td("Exec value"),
              td("Condition"),
              td("Check value"),
              td("Result")
            ),
            success_condition.getOrElse(List[SucCondElement]()).map {sc =>
              tr(bgColorAttr := (if (sc.conditionResult.getOrElse(false))
                "#228B22;"
              else
                "#FF4500;"),
                td(
                  sc.condition match {
                    case Fields_exists => Html.raw(testRes.cols.map(_._1).mkString("</br>"))
                    case Exec_exception =>
                      testRes.err.fold("no exception but expected")(_ => "exception exist, it's OK")
                    case _ => sc.execResultValue.getOrElse(0).toString
                  }
                ),
                td(sc.condition.toString),
                td(
                  sc.condition match {
                    case Fields_exists => Html.raw(sc.fields.getOrElse(List[String]()).mkString("</br>"))
                    case Exec_exception => "true"
                    case _ => sc.checkValue.toString
                  }
                ),
                td(sc.conditionResult.getOrElse(false).toString)
              )
            }
          )
        ))
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      )
    )

}

case class TestModel(meta: TestsMeta, tests: Option[List[Test]]) {
  val listID : List[TestID] = tests.getOrElse(List[Test]()).map(t => t.id)
  if (listID.distinct.size != listID.size)
    throw new Exception("Not unique id in tests. Must be unique.")
}

case class TestsToRun(sid: SessionId, ids: Option[List[TestID]])

object EncDecTestModelImplicits{

  implicit val encoderTestExecutionException: JsonEncoder[TestExecutionException] = DeriveJsonEncoder.gen[TestExecutionException]
  implicit val decoderTestExecutionException: JsonDecoder[TestExecutionException] = DeriveJsonDecoder.gen[TestExecutionException]

  implicit val encoderCallType: JsonEncoder[CallType] = DeriveJsonEncoder.gen[CallType]
  implicit val decoder: JsonDecoder[CallType] = JsonDecoder[String].map {
    case "select" => Select
    case "function" => Function
    case "select_function" => Select_function
    case "func_inout_cursor" => Func_inout_cursor
    case "dml_sql" => Dml_sql
    case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
  }

  implicit val encoderRetType: JsonEncoder[RetType] = DeriveJsonEncoder.gen[RetType]
  implicit val decoderRetType: JsonDecoder[RetType] = JsonDecoder[String].map {
    case "cursor" => Cursor
    case "dataset" => Dataset
    case "integer_value" => Integer_value
    //case "affected_rows" => Affected_rows
    case anyValue => throw new Exception(s"Invalid value in field ret_type = $anyValue")
  }

  implicit val encoderSucCond: JsonEncoder[SucCond] = DeriveJsonEncoder.gen[SucCond]
  implicit val decoderSucCond: JsonDecoder[SucCond] = JsonDecoder[String].map {
    case "rows_gt" => Rows_gt
    case "rows_lt" => Rows_lt
    case "rows_eq" => Rows_eq
    case "rows_ne" => Rows_ne
    case "exec_time_ms" => Exec_time_ms
    case "fetch_time_ms" => Fetch_time_ms
    case "full_time_ms" => Full_time_ms
    case "fields_exists" => Fields_exists
    case "exec_exception" => Exec_exception
    case "intval_gt" => Intval_gt
    case "intval_lt" => Intval_lt
    case "intval_eq" => Intval_eq
    case "intval_ne" => Intval_ne
    case anyValue => throw new Exception(s"Invalid value in field inside success_condition = $anyValue")
  }

  implicit val encoderTestState: JsonEncoder[TestState] = DeriveJsonEncoder.gen[TestState]
  implicit val decoderTestState: JsonDecoder[TestState] = JsonDecoder[String].map {
    case _ => testStateUndefined
  }

  implicit val encoderTestExecutionResult: JsonEncoder[TestExecutionResult] = DeriveJsonEncoder.gen[TestExecutionResult]
  implicit val decoderTestExecutionResult: JsonDecoder[TestExecutionResult] = DeriveJsonDecoder.gen[TestExecutionResult]

  /*
  implicit val encoderTestState: JsonEncoder[TestState] = DeriveJsonEncoder.gen[TestState]
  implicit val decoderTestState: JsonDecoder[TestState] = JsonDecoder[String].map {
    case "testStateUndefined" => testStateUndefined
    case "testStateSuccess" => testStateSuccess
    case "testStateFailure" => testStateFailure
    case "testStateExecuting" => testStateExecuting
    case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
  }
  */

  implicit val encoderSucCondElement: JsonEncoder[SucCondElement] = DeriveJsonEncoder.gen[SucCondElement]
  implicit val decoderSucCondElement: JsonDecoder[SucCondElement] = DeriveJsonDecoder.gen[SucCondElement]

  implicit val encoderTestsMeta: JsonEncoder[TestsMeta] = DeriveJsonEncoder.gen[TestsMeta]
  implicit val decoderTestsMeta: JsonDecoder[TestsMeta] = DeriveJsonDecoder.gen[TestsMeta]

  implicit val encoderTest: JsonEncoder[Test] = DeriveJsonEncoder.gen[Test]
  implicit val decoderTest: JsonDecoder[Test] = DeriveJsonDecoder.gen[Test]

  implicit val encoderTestModel: JsonEncoder[TestModel] = DeriveJsonEncoder.gen[TestModel]
  implicit val decoderTestModel: JsonDecoder[TestModel] = DeriveJsonDecoder.gen[TestModel]

  implicit val encoderTestsToRun: JsonEncoder[TestsToRun] = DeriveJsonEncoder.gen[TestsToRun]
  implicit val decoderTestsToRun: JsonDecoder[TestsToRun] = DeriveJsonDecoder.gen[TestsToRun]


}
