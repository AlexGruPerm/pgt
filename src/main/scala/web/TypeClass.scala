package web

import data.ImplTestsRepo
import web.WebUiApp.ZioResponseMsgBadRequest
import zio.ZIO
import zio.http.Response

/**
 ZIO[ImplTestsRepo, IOException, Response]
 ZIO[Any,           IOException, Response]
 ZIO[ImplTestsRepo, IOException, Response]
 ------------------------------------------------
 ZIO[ImplTestsRepo, Exception,   Response]
*/

//  private def catchCover[C](eff: ZIO[C, Exception, Response]): ZIO[C, Nothing, Response]
//  A - ZIO[C, Exception, Response]

trait ResponseCatcher[A]{
  def catchCover(eff: A): ZIO[ImplTestsRepo, Nothing, Response]
}

object ResponseCatcherInstances {

  implicit val catchCoverImplTestsRepo: ResponseCatcher[ZIO[ImplTestsRepo, Exception, Response]] =
    (eff: ZIO[ImplTestsRepo, Exception, Response]) => {
      eff.catchAll { e: Exception =>
        ZIO.logError(e.getMessage) *> ZioResponseMsgBadRequest(e.getMessage)
      }
    }

}

