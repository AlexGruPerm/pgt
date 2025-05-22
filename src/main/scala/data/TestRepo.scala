package data

import common.TestModelRepo
import common.types.SessionId
import data.TestRepoTypes.TestID
import tmodel.{Test, TestModel}

// Removed scala.collection.mutable import
import zio.{UIO, _}

  object TestRepoTypes{
    type TestID = Int
  }

  trait TestsRepo {

    /**
     * Create new Tests set in repo and return SessionId
    */
    def create(testModel: TestModel) :Task[SessionId]

    def lookup(sid: SessionId): UIO[Option[TestModelRepo]]

    def elementsCnt :UIO[Int]

    /**
     * Return list of tests from concrete TestModelRepo by sid.
     * For output in div.test_list
    */
    def testsList(sid: SessionId): UIO[Option[List[Test]]]

    /**
     * Aggregated information for debug purpose
    */
    def checkTestRepoData(sid: SessionId): UIO[Option[checkTestRepoInfo]]

    /**
     * Update one test in TestModelRepo, test must be with execution results - look TestExecutionResult
    */
    def updateTestWithResults(sid: SessionId, testWithResults: Test): UIO[Unit]

    /**
     * Enable one test in the tests set identified by sid.
    */
    def enableTest(sid: SessionId, id: TestID):  UIO[Unit]

    /**
     * Disable one test in the tests set identified by sid.
     */
    def disableTest(sid: SessionId, id: TestID): UIO[Unit]

    /**
     * Disable all tests in the tests set identified by sid.
     */
    def disableAllTestAndClearExecRes(sid: SessionId): UIO[Unit]

    def testsListEnabled(sid: SessionId): UIO[Option[List[Test]]]

}

  // Changed mutable.Map to immutable Map
  case class ImplTestsRepo(ref: Ref[Map[SessionId, TestModelRepo]]) extends TestsRepo {

    def create(testModel: TestModel): Task[SessionId] = for {
      sid <- Random.nextUUID.map(_.toString)
      // Updated to use immutable map operation
      _ <- ref.update(map => map + (sid -> TestModelRepo(testModel)))
    } yield sid

    def lookup(sid: SessionId): UIO[Option[TestModelRepo]] =
      ref.get.map(_.get(sid)) // Should work fine with immutable Map

    def elementsCnt: UIO[Int] = ref.get.map(_.size) // Should work fine with immutable Map

    def testsList(sid: SessionId): UIO[Option[List[Test]]] = for {
      test <- lookup(sid)
      tests = test.flatMap(tst => tst.optListTestInRepo)
    } yield tests

    def testsListEnabled(sid: SessionId): UIO[Option[List[Test]]] = for {
      test <- lookup(sid)
      tests = test.flatMap(olt => olt.optListTestInRepo.map(t => t.filter(_.isEnabled == true)))
    } yield tests

    def checkTestRepoData(sid: SessionId): UIO[Option[checkTestRepoInfo]] = for {
      tests <- lookup(sid)
      res = tests.map(v => v.optListTestInRepo.fold(TestsStatus.undefined)(TestsStatus.calculated).getCheckTestRepoInfo)
      res <- ZIO.succeed(res)
    } yield res

    def updateTestWithResults(sid: SessionId, testWithResults: Test): UIO[Unit] =
      ref.modify { currentMap =>
        currentMap.get(sid) match {
          case Some(repo) =>
            val updatedRepo = repo.updateOneTest(testWithResults)
            (Unit, currentMap.updated(sid, updatedRepo))
          case None =>
            (Unit, currentMap) // No change
        }
      }.unit

    def enableTest(sid: SessionId, testId: TestID): UIO[Unit] =
      ref.modify { currentMap =>
        currentMap.get(sid) match {
          case Some(repo) =>
            val updatedRepo = repo.enableOneTest(testId)
            (Unit, currentMap.updated(sid, updatedRepo))
          case None =>
            (Unit, currentMap)
        }
      }.unit

    def disableTest(sid: SessionId, testId: TestID): UIO[Unit] =
      ref.modify { currentMap =>
        currentMap.get(sid) match {
          case Some(repo) =>
            val updatedRepo = repo.disableOneTest(testId)
            (Unit, currentMap.updated(sid, updatedRepo))
          case None =>
            (Unit, currentMap)
        }
      }.unit

    def disableAllTestAndClearExecRes(sid: SessionId): UIO[Unit] =
      ref.modify { currentMap =>
        currentMap.get(sid) match {
          case Some(repo) =>
            val testsToDisable = repo.optListTestInRepo.getOrElse(List.empty)
            val fullyDisabledRepo = testsToDisable.foldLeft(repo)((currentRepo, test) => currentRepo.disableOneTest(test.id))
            (Unit, currentMap.updated(sid, fullyDisabledRepo))
          case None =>
            (Unit, currentMap)
        }
      }.unit
  }

  object ImplTestsRepo {
    def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
      ZLayer.fromZIO(
        // Changed to immutable Map
        Ref.make(Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
      )
  }