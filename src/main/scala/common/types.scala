package common

import tmodel.{SucCondElement, Test, TestModel, TestsMeta, testStateUndefined}
import _root_.data.TestRepoTypes.TestID
import common.types.Columns

  object types {
    type SessionId = String
    type ColumnName = String
    type ColumnType = String
    type Column = (ColumnName, ColumnType)
    type Columns = IndexedSeq[Column]
    type ListRows = List[IndexedSeq[String]]
  }

  case class CallTimings(tBegin: Long, tExec: Long, tFetch: Long)

  case class TestExecutionResult(totalMs: Long,
                                 fetchMs: Long,
                                 execMs: Long,
                                 cols : Columns,
                                 rowCount: Int,
                                 err: Option[TestExecutionException] = None){
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~")
    err match {
      case Some(e) => println(s"error = ${e.exceptionType} - ${e.exceptionMsg}")
      case None => println("no error")
    }
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~")
}

  object TestExecutionResult {
    def apply(): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0)

    def apply(excType:String, errMsg: String): TestExecutionResult = {
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0,
        err = Some(TestExecutionException(excType,errMsg)))
    }

    def apply(timings: CallTimings, cols: Columns, rowCount: Int): TestExecutionResult =
      TestExecutionResult(timings.tFetch - timings.tBegin, timings.tFetch - timings.tExec,
        timings.tExec - timings.tBegin, cols, rowCount)

  }

  case class TestModelRepo(meta: TestsMeta, optListTestInRepo: Option[List[Test]]){

    private def uncheckConditions(success_condition: Option[List[SucCondElement]]): Option[List[SucCondElement]] =
      success_condition.fold(Some(List[SucCondElement]())){
        listSucCond => Some(listSucCond.map(sc => sc.uncheck()))
      }

    //todo: In all 3 methods don't do update in case of test in testInRepo not affected
    def enableOneTest(testId: TestID): TestModelRepo = {
      val updatedTests: Option[List[Test]] = optListTestInRepo.map{
        tr => tr.map {t=>
          if (t.id == testId) {
            t.copy(isEnabled = true)
          } else
            t
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

    //todo: Need refactoring to eliminate if
    def updateOneTest(testWithResults: Test): TestModelRepo ={
      val updatedTests: Option[List[Test]] = optListTestInRepo.map{
        listTestsInRepo => listTestsInRepo.collect { case testInRepo =>
          if (testInRepo.id == testWithResults.id)
            testWithResults
          else
            testInRepo
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

    def disableOneTest(testId: TestID): TestModelRepo = {
      val updatedTests: Option[List[Test]] = optListTestInRepo.map{
        tr => tr.map {t=>
          if (t.id == testId)
            t.copy(isEnabled = false,
              success_condition = uncheckConditions(t.success_condition),
              isExecuted = false,
              testState = testStateUndefined,
              testRes = TestExecutionResult()
            )
          else
            t
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

  }

  object TestModelRepo {

    def apply(tm: TestModel) : TestModelRepo = {
      val testsInRepo: Option[List[Test]] = tm.tests.map{tst => tst.map{test =>
        test.copy(testState = testStateUndefined,
          testRes = TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0)
        )
      }}
      TestModelRepo(tm.meta, testsInRepo)
    }

  }

  case class TestExecutionException(exceptionType: String, exceptionMsg: String)




