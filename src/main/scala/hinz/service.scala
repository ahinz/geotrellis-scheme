package org.hinz.server

import unfiltered.request._
import unfiltered.response._

import org.hinz.parsing._

import geotrellis._
import geotrellis.operation.render.png._
import geotrellis.operation._
import geotrellis.process._
import geotrellis.data._

object Main {

  val files = Map("elev" -> "/var/trellis/stroud/elevation30m-20110607.arg32")

  def wrapCB(p: Map[String,Seq[Any]])(content: String) =
    p("callback") match {
      case Seq() => content
      case Seq(a) => "%s(%s)" format(a toString, content)
    }

  val echo = unfiltered.netty.cycle.Planify {
    case Path(Seg("runner" :: Nil)) & Params(params) => {
      
      val p = SimpleParser(params.get("p").get.head)
      println(p)

      // ResponseString(wrapCB(params)(SexpHandler.handle(p.get) match {
      // case Left(e) => """{ error: "%s" }""" format e
      // case Right((LiteralNode(a),_)) => """{ result: "%s" }""" format a.toString
      // case Right((a,_)) => """{ error: "Non-literal return value (%s)" } """ format a.toString
      // }))

      ResponseBytes(SexpHandler.handle(p.get) match {
        case Right((LiteralNode(a:Array[Byte]),_)) => a
        case Right((a,_)) => error("Non-literal return value (%s)" format a.toString)
        case z => error("unnkown match error (%s)" format z)
      })
    }
  }

  def main(args: Array[String]) {
    unfiltered.netty.Http(1111).plan(echo).run()
  }

}
