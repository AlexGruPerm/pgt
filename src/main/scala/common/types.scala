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
                                 err: Option[TestExecutionException] = None,
                                 intVal: Int = 0
                                )

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

    def apply(timings: CallTimings, cols: Columns, rowCount: Int, iVal: Int): TestExecutionResult =
      TestExecutionResult(timings.tFetch - timings.tBegin, timings.tFetch - timings.tExec,
        timings.tExec - timings.tBegin, cols, rowCount, intVal = iVal)

  }

  case class TestModelRepo(meta: TestsMeta, optListTestInRepo: Option[List[Test]]){

    private def uncheckConditions(success_condition: Option[List[SucCondElement]]): Option[List[SucCondElement]] =
      success_condition.fold(Some(List[SucCondElement]())){
        listSucCond => Some(listSucCond.map(sc => sc.uncheck()))
      }

    //todo: In all 3 methods don't do update in case of test in testInRepo not affected
    def enableOneTest(testId: TestID): TestModelRepo = {
      modifyTestById(testId, t => t.copy(isEnabled = true))
    }

    // Private helper method to modify a test by ID
    private def modifyTestById(testId: TestID, transform: Test => Test): TestModelRepo = {
      var testWasChanged = false
      val updatedTestsOpt: Option[List[Test]] = optListTestInRepo.map { listTestsInRepo =>
        listTestsInRepo.map { testInRepo =>
          if (testInRepo.id == testId) {
            val transformedTest = transform(testInRepo)
            if (transformedTest != testInRepo) { // Check if the test actually changed
              testWasChanged = true
            }
            transformedTest
          } else {
            testInRepo
          }
        }
      }
      // Only create a new TestModelRepo if a test was actually changed or if optListTestInRepo was None initially
      if (testWasChanged || (optListTestInRepo.isDefined && updatedTestsOpt.isDefined && optListTestInRepo != updatedTestsOpt) ) {
        this.copy(optListTestInRepo = updatedTestsOpt)
      } else {
        this
      }
    }

    //todo: Need refactoring to eliminate if - Done
    def updateOneTest(testWithResults: Test): TestModelRepo = {
      // Using modifyTestById, the transformation is to simply replace the test.
      // The check for whether the test actually changed is handled within modifyTestById.
      modifyTestById(testWithResults.id, _ => testWithResults)
    }

    def disableOneTest(testId: TestID): TestModelRepo = {
      modifyTestById(testId, t =>
        t.copy(isEnabled = false,
          success_condition = uncheckConditions(t.success_condition),
          isExecuted = false,
          testState = testStateUndefined,
          testRes = TestExecutionResult()
        )
      )
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




