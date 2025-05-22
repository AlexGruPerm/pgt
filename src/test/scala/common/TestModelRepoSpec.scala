package common

import org.scalatest.Ignore
import tmodel._
import zio.test._
import zio.test.Assertion._

object TestModelRepoSpec extends ZIOSpecDefault {

  // Sample data for tests
  val sampleTest1 = Test(id = 1, name = "Test 1", query = "SELECT 1", isEnabled = false, success_condition = Some(List(SucCondElement("col1", "val1", Cond.EQ))), testState = testStateInProgress, testRes = TestExecutionResult(10L, 5L, 5L, IndexedSeq(), 1))
  val sampleTest2 = Test(id = 2, name = "Test 2", query = "SELECT 2", isEnabled = true, success_condition = None, testState = testStateOk, testRes = TestExecutionResult())
  val sampleTest3 = Test(id = 3, name = "Test 3", query = "SELECT 3", isEnabled = false, success_condition = Some(List(SucCondElement("colA", "valX", Cond.GT,isChecked = true))), testState = testStateUndefined, testRes = TestExecutionResult())

  val sampleTestsMeta = TestsMeta("pg", "jdbc://localhost/test", "user", "pass", "Test Suite 1")
  val sampleTestModel = TestModel(meta = sampleTestsMeta, tests = Some(List(sampleTest1, sampleTest2)))
  val emptyTestModel = TestModel(meta = sampleTestsMeta, tests = Some(List()))
  val noneTestModel = TestModel(meta = sampleTestsMeta, tests = None)

  def spec = suite("TestModelRepoSpec")(
    suite("TestModelRepo.apply (factory)")(
      test("should initialize testState to testStateUndefined and clear testRes for all tests") {
        val repo = TestModelRepo(sampleTestModel)
        assertTrue(
          repo.optListTestInRepo.isDefined,
          repo.optListTestInRepo.get.forall(_.testState == testStateUndefined),
          repo.optListTestInRepo.get.forall(_.testRes == TestExecutionResult(0L, 0L, 0L, IndexedSeq(), 0)),
          repo.optListTestInRepo.get.find(_.id == 1).get.isEnabled == false, // original isEnabled should be preserved
          repo.optListTestInRepo.get.find(_.id == 2).get.isEnabled == true
        )
      },
      test("should handle empty list of tests") {
        val repo = TestModelRepo(emptyTestModel)
        assertTrue(repo.optListTestInRepo.isDefined, repo.optListTestInRepo.get.isEmpty)
      },
      test("should handle None list of tests") {
        val repo = TestModelRepo(noneTestModel)
        assertTrue(repo.optListTestInRepo.isEmpty)
      }
    ),
    suite("enableOneTest method")(
      test("should enable a specific test and not affect others") {
        val initialRepo = TestModelRepo(TestModel(meta = sampleTestsMeta, tests = Some(List(sampleTest1.copy(isEnabled = false), sampleTest2.copy(isEnabled = false)))))
        val updatedRepo = initialRepo.enableOneTest(1)
        val test1Enabled = updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 1).map(_.isEnabled))
        val test2Enabled = updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 2).map(_.isEnabled))
        assertTrue(test1Enabled == Some(true), test2Enabled == Some(false))
      },
      test("should not change repo if test to enable is not found") {
        val initialRepo = TestModelRepo(sampleTestModel)
        val updatedRepo = initialRepo.enableOneTest(99) // Non-existent ID
        assertTrue(updatedRepo == initialRepo) // Object identity check for no change
      },
      test("should not change repo if test is already enabled") {
        val initialRepo = TestModelRepo(TestModel(meta = sampleTestsMeta, tests = Some(List(sampleTest1.copy(isEnabled = true)))))
        val updatedRepo = initialRepo.enableOneTest(1)
        // Check if the specific test object instance is the same, or if the whole repo is the same
        // Depending on copy-on-write semantics of the helper, object identity might not hold for the test but should for the repo
        assertTrue(updatedRepo == initialRepo)
        assertTrue(updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 1)).map(_.isEnabled) == Some(true))
      }
    ),
    suite("disableOneTest method")(
      test("should disable a specific test, clear results, and uncheck conditions") {
        val testToDisable = sampleTest2.copy(isEnabled = true, success_condition = Some(List(SucCondElement("c","v",Cond.EQ, isChecked = true))), isExecuted = true, testState = testStateOk)
        val initialRepo = TestModelRepo(TestModel(meta = sampleTestsMeta, tests = Some(List(sampleTest1, testToDisable))))
        val updatedRepo = initialRepo.disableOneTest(2)

        val disabledTestOpt = updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 2))
        assertTrue(
          disabledTestOpt.isDefined,
          disabledTestOpt.get.isEnabled == false,
          disabledTestOpt.get.isExecuted == false,
          disabledTestOpt.get.testState == testStateUndefined,
          disabledTestOpt.get.testRes == TestExecutionResult(),
          disabledTestOpt.get.success_condition.get.forall(!_.isChecked), // All conditions unchecked
          updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 1)) == initialRepo.optListTestInRepo.flatMap(_.find(_.id == 1)) // Test 1 unaffected
        )
      },
      test("should not change repo if test to disable is not found") {
        val initialRepo = TestModelRepo(sampleTestModel)
        val updatedRepo = initialRepo.disableOneTest(99)
        assertTrue(updatedRepo == initialRepo)
      },
      test("should not change repo if test is already disabled (and results cleared, etc.)") {
         val alreadyDisabledTest = sampleTest1.copy(
            isEnabled = false,
            success_condition = Some(List(SucCondElement("col1", "val1", Cond.EQ, isChecked = false))), // Already unchecked
            isExecuted = false,
            testState = testStateUndefined,
            testRes = TestExecutionResult()
        )
        val initialRepo = TestModelRepo(TestModel(meta = sampleTestsMeta, tests = Some(List(alreadyDisabledTest))))
        val updatedRepo = initialRepo.disableOneTest(1)
        assertTrue(updatedRepo == initialRepo) // Expecting no change
        assertTrue(updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 1)) == Some(alreadyDisabledTest))
      }
    ),
    suite("updateOneTest method")(
      test("should update a specific test with new results and not affect others") {
        val initialRepo = TestModelRepo(sampleTestModel) // test1 has isEnabled=false, testState=testStateUndefined
        val resultsForTest1 = sampleTest1.copy(testState = testStateOk, testRes = TestExecutionResult(100L, 60L, 40L, IndexedSeq(("res", "TEXT")), 1), isEnabled = true)

        val updatedRepo = initialRepo.updateOneTest(resultsForTest1)
        val updatedTest1 = updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 1))
        val unupdatedTest2 = updatedRepo.optListTestInRepo.flatMap(_.find(_.id == 2))

        assertTrue(
          updatedTest1 == Some(resultsForTest1),
          unupdatedTest2 == initialRepo.optListTestInRepo.flatMap(_.find(_.id == 2)) // Test 2 unaffected
        )
      },
      test("should not change repo if test to update is not found") {
        val initialRepo = TestModelRepo(sampleTestModel)
        val nonExistentTestUpdate = Test(id = 99, name = "Non Existent", query = "", testState = testStateOk)
        val updatedRepo = initialRepo.updateOneTest(nonExistentTestUpdate)
        assertTrue(updatedRepo == initialRepo)
      },
      test("should not change repo if test data is identical to existing test data") {
        // First, ensure the factory sets up the test as it would be in the repo
        val repoWithInitialTest1 = TestModelRepo(TestModel(meta = sampleTestsMeta, tests = Some(List(sampleTest1))))
        val existingTest1InRepo = repoWithInitialTest1.optListTestInRepo.get.head // This is after .apply processing

        // Now "update" with this very same test data
        val updatedRepo = repoWithInitialTest1.updateOneTest(existingTest1InRepo)
        assertTrue(updatedRepo == repoWithInitialTest1) // Expecting no change, repo object identity should be the same
        assertTrue(updatedRepo.optListTestInRepo.flatMap(_.headOption) == Some(existingTest1InRepo))
      }
    )
  )
}
