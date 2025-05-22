package error

import zio.json._

sealed trait AppError {
  val message: String
}

case class ConfigError(message: String) extends AppError
case class DatabaseError(message: String, underlying: Option[Throwable] = None) extends AppError
case class ValidationError(message: String) extends AppError
case class NotFoundError(message: String) extends AppError
case class WebServiceError(message: String, underlying: Option[Throwable] = None) extends AppError

case class ErrorResponse(errorType: String, message: String)

object ErrorResponse {
  implicit val encoder: JsonEncoder[ErrorResponse] = DeriveJsonEncoder.gen[ErrorResponse]
}
