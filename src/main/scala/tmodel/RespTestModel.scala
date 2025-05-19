package tmodel

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Session(id: String)
case class RespTest(id: Int, name: String)

case class RespTestModel(session: Session, tests: Option[List[RespTest]])

object EncDecRespTestModelImplicits {
  implicit val encoderSession: JsonEncoder[Session] = DeriveJsonEncoder.gen[Session]
  implicit val decoderSession: JsonDecoder[Session] = DeriveJsonDecoder.gen[Session]

  implicit val encoderRespTest: JsonEncoder[RespTest] = DeriveJsonEncoder.gen[RespTest]
  implicit val decoderRespTest: JsonDecoder[RespTest] = DeriveJsonDecoder.gen[RespTest]

  implicit val encoderRespTestModel: JsonEncoder[RespTestModel] = DeriveJsonEncoder.gen[RespTestModel]
  implicit val decoderRespTestModel: JsonDecoder[RespTestModel] = DeriveJsonDecoder.gen[RespTestModel]

}
