package data

import tmodel.{Test, testStateFailure, testStateSuccess}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

  case class checkTestRepoInfo(tests: TestsStatus)

  case class TestsStatus(total: Int,
                         enabled: Int,
                         disabled: Int,
                         executed: Int,
                         success: Int,
                         failure: Int,
                         successList: List[Int],
                         failureList: List[Int]
                        ){
    def getCheckTestRepoInfo: checkTestRepoInfo = checkTestRepoInfo(this)
  }

  object TestsStatus{
     def undefined: TestsStatus =
      apply(total = 0, enabled = 0, disabled = 0, executed = 0, success = 0, failure = 0,
        successList = List[Int](), failureList = List[Int]())

    def calculated(listTest: List[Test]) =
      TestsStatus(
        listTest.size,
        listTest.count(t => t.isEnabled),
        listTest.count(t => !t.isEnabled),
        listTest.count(t => t.isExecuted),
        listTest.count(t => t.testState == testStateSuccess),
        listTest.count(t => t.testState == testStateFailure),
        listTest.filter(t => t.testState
        match {
          case _: testStateSuccess.type => true
          case _ => false})
          .map(_.id),
        listTest.filter(_.testState == testStateFailure).map(_.id)
      )

  }

  object EncDeccheckTestRepoDataImplicits {

    implicit val encoderTestsStatus: JsonEncoder[TestsStatus] = DeriveJsonEncoder.gen[TestsStatus]
    implicit val decoderTestsStatus: JsonDecoder[TestsStatus] = DeriveJsonDecoder.gen[TestsStatus]

    implicit val encoderSession: JsonEncoder[checkTestRepoInfo] = DeriveJsonEncoder.gen[checkTestRepoInfo]
    implicit val decoderSession: JsonDecoder[checkTestRepoInfo] = DeriveJsonDecoder.gen[checkTestRepoInfo]
  }