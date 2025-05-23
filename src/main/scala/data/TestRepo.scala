package data

import common.TestModelRepo
import common.types.SessionId
import data.TestRepoTypes.TestID
import tmodel.{Test, TestModel}

import scala.collection.mutable
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

    def remove(sid: SessionId): UIO[Unit]

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

  case class ImplTestsRepo(ref: Ref[mutable.Map[SessionId, TestModelRepo]]) extends TestsRepo {

    def create(testModel: TestModel): Task[SessionId] = for {
      sid <- Random.nextUUID.map(_.toString)
      _ <- ref.update(test => test.concat(List(sid -> TestModelRepo(testModel))))
    } yield sid

    def lookup(sid: SessionId): UIO[Option[TestModelRepo]] =
      ref.get.map(_.get(sid))

    def remove(sid: SessionId): UIO[Unit] = for {
      repo <- ref.get
    } yield repo.remove(sid)

    def elementsCnt: UIO[Int] = ref.get.map(_.size)

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

    def updateTestWithResults(sid: SessionId, testWithResults: Test): UIO[Unit] = for {
      test <- lookup(sid)
      _ <- test.fold(ZIO.unit) { testsSet =>
        ref.update {
          tests =>
            tests.concat(
              List(sid -> TestModelRepo(testsSet.meta, testsSet.optListTestInRepo).updateOneTest(testWithResults))
            )
        }
      }
    } yield ()

    def enableTest(sid: SessionId, testId: TestID): UIO[Unit] = for {
      test <- lookup(sid)
      _ <- test.fold(ZIO.unit) { testsSet =>
        ref.update {
          tests =>
            tests.concat(
              List(sid -> TestModelRepo(testsSet.meta, testsSet.optListTestInRepo).enableOneTest(testId))
            )
        }}
    } yield ()

    def disableTest(sid: SessionId, testId: TestID): UIO[Unit] = for {
      test <- lookup(sid)
      _ <- test.fold(ZIO.unit) { testsSet =>
        ref.update {
          tests =>
            tests.concat(
              List(sid -> TestModelRepo(testsSet.meta, testsSet.optListTestInRepo).disableOneTest(testId))
            )
        }}
    } yield ()

    def disableAllTestAndClearExecRes(sid: SessionId): UIO[Unit] = for {
      testModelRepo <- lookup(sid)
      _ <- testModelRepo.fold(ZIO.unit) { test =>
        ZIO.foreachDiscard(test.optListTestInRepo.getOrElse(List[Test]())) { t =>
          disableTest(sid, t.id)
        }}
    } yield ()
  }

  object ImplTestsRepo {
    def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
      ZLayer.fromZIO(
        Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
      )
  }