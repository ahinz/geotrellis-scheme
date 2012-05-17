package org.hinz.parsing

import scala.util.parsing.combinator._

case class Context(symbols: Map[String,Node], lambdaCtr: Int) {
  def addSymbols(s: Map[String,Node]) = Context(symbols ++ s, lambdaCtr)
  def nextLambdaName() = ("lambda<%d>" format lambdaCtr, Context(symbols, lambdaCtr + 1))
}

sealed trait Node
case object UnitNode extends Node
case object FalseNode extends Node
case class ListNode(list: List[Node]) extends Node
case class IdentNode(id: String) extends Node
case class LiteralNode[T](lit: T) extends Node
case class IntLitNode(value: Int) extends LiteralNode[Int](value)
case class StrLitNode(value: String) extends LiteralNode[String](value)
case class FunctionNode(id: String, bindings: List[String], body: Node) extends Node
case class InternalFunctionNode(id: String, f: (List[Node],Context) => Either[String,Node]) extends Node

object SimpleParser extends RegexParsers {

  val ID:Parser[Node] = """[a-zA-Z_+-><][a-zA-Z_0-9!/-]*""".r ^^ { IdentNode(_) }
  val FalseLit = "false" ^^ { a => FalseNode }
  val IntLit = "[0-9]+".r ^^ { s => IntLitNode(s.toInt) }
  val HexLit = "0x[0-9a-fA-F]+".r ^^ { s => IntLitNode(Integer.parseInt(s.substring(2),16)) }
  val StrLit = "\"".r ~> "[^\"]+".r <~ "\"" ^^ { a => StrLitNode(a.toString()) }

  def slist:Parser[Node] = ("(" ~> rep(sexp) <~ ")") ^^ { ListNode(_) }
  def sexp:Parser[Node] = FalseLit | HexLit | IntLit | StrLit | ID | slist 

  def program = sexp*

  def apply(s: String) = parseAll(program, s)
}

object GeoTrellis {

  import geotrellis._
  import geotrellis.operation.render.png._
  import geotrellis.operation._
  import geotrellis.process._
  import geotrellis.data._
  import geotrellis.stat._
 

  val trellisFns:Map[String, Node] = Map(
    "scala/tuple2i" -> InternalFunctionNode("tuple2i", (a,b) => a match {
      case List(IntLitNode(x),IntLitNode(y)) => Right(LiteralNode((x,y)))
      case a => Left("trellis/loadfile expects 1 argument (got %s)" format a)
    }),
    "trellis/loadfile" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(StrLitNode(file)) => Right(LiteralNode(LoadFile(file)))
      case a => Left("trellis/loadfile expects 1 argument (got %s)" format a)
    }),
    "trellis/hillshade" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[IntRaster])) => Right(LiteralNode(Hillshade(l, 45.0, 20.0)))
      case a => Left("hillshade expects 1 argument (got %s)" format a)
    }),
    "trellis/histogram" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[IntRaster])) => Right(LiteralNode(BuildMapHistogram(l)))
      case a => Left("histogram expects 1 argument (got %s)" format a)
    }),
    "trellis/stats" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[Histogram])) => Right(LiteralNode(GenerateStatistics(l)))
      case a => Left("stats expects 1 argument (got %s)" format a)
    }),      
    "trellis/image" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[IntRaster])) => Right(LiteralNode(RenderPngRgba(l)))
      case List(LiteralNode(l:Op[IntRaster]), LiteralNode(cb:Op[ColorBreaks])) =>
        Right(LiteralNode(RenderPNG(l, cb, 0, true)))
      case a => Left("trellis image expects 1 argument (got %s)" format a)
    }),
    "trellis/colorbreaks" -> InternalFunctionNode("trellis", (a,b) => a match {
      case (l:List[IntLitNode]) => Right(LiteralNode(Literal(ColorBreaks(l.grouped(2).toArray.map(z => (z(0).value,z(1).value))))))
      case a => Left("could not construct color breaks with %s" format a)
    }),
    "trellis/colorbreaks2" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:List[LiteralNode[(Int,Int)]])) => { 
        Right(LiteralNode(Literal(ColorBreaks(l.map(_.lit).toArray)))) }
      case a => Left("could not construct color breaks with %s" format a)
    }),
    "trellis/resample" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[IntRaster]), IntLitNode(width), IntLitNode(height)) => Right(LiteralNode(ResampleRaster(l,width,height)))
      case a => Left("trellis resample expects three arguments (got %s)" format a)
    }),
    "trellis/run" -> InternalFunctionNode("trellis", (a,b) => a match {
      case List(LiteralNode(l:Op[IntRaster])) => Right(LiteralNode(Server("server", Catalog("",Map())).run(l)))
      case a => Left("run expects 1 argument (got %s)" format a)
    })
  )

}

object Helpers {

  trait FromNode[T] extends (Node => Either[String,T])

  implicit def nodeIsInt:FromNode[Int] = new FromNode[Int] {
    def apply(n: Node) = n match {
      case IntLitNode(a) => Right(a)
      case z => Left("Error... expecting int got %s" format z)
    }
  }

  implicit def nodeIsString:FromNode[String] = new FromNode[String] {
    def apply(n: Node) = n match {
      case LiteralNode(a) => Right(a.toString())
      case z => Left("Error... expecting a literal got %s" format z)
    }
  }

  def seq[A,B](l: List[Either[A,B]]):Either[A,List[B]] = l.foldLeft(Right(List()):Either[A,List[B]])((lst,item) => (lst,item) match {
    case (Right(lst),Right(item)) => Right(item :: lst)
    case (l@Left(err), _) => l
    case (_, Left(err)) => Left(err)
  }).right.map(_.reverse)

  def toIntList(l: List[Node]) = l.flatMap { 
    case IntLitNode(l) => Some(l)
    case _ => None
  }

  def toStrList(l: List[Node]):Either[String,List[String]] = seq(l map {
    case IdentNode(id) => Right(id)
    case n => Left("Expected identifier, got %s" format n)
  })

  implicit def autoRaiseFn[T,R](f: List[T] => R)(implicit convert: FromNode[T]): InternalFunctionNode =
     InternalFunctionNode("ifn", ((l,z) => {
       seq(l.map(n => convert(n))).right.map(args => f(args) match {
         case z:Int => IntLitNode(z)
         case z => LiteralNode(z)
       })
     }))

  def boolNode(b: Boolean):Node = if (b) IntLitNode(1) else FalseNode
}

object SexpHandler {
  import GeoTrellis._
  import Helpers._
  import scala.io.Source._

  val eqf = InternalFunctionNode("ifn-eq", ((a:List[Node],c:Context) => Right(boolNode(a.map(_.equals(a.head)).reduceLeft(_ && _)))))

  def id[T](t:List[T]) = t

  val baseSymbolTable:Map[String,Node] = trellisFns ++ Map[String,Node](
    ">" -> InternalFunctionNode("gr", (a,b) => a match { 
      case List(IntLitNode(a), IntLitNode(b)) => Right(if (a > b) IntLitNode(1) else FalseNode)
      case s => error("> fn error with %s" format s)
    }),
    "list" -> InternalFunctionNode("list", (a,b) => a match { 
      case l:List[Node] => Right(LiteralNode(l))
      case s => error("parse error while creating list with %s" format s)
    }),
    "append" -> InternalFunctionNode("append", (a,b) => a match {
      case LiteralNode(l:List[Node]) :: rest => Right(LiteralNode(l ++ rest))
      case s => error("parse error while creating list with %s" format s)
    }),
    "prepend" -> InternalFunctionNode("append", (a,b) => a match {
      case LiteralNode(l:List[Node]) :: rest => Right(LiteralNode(rest ++ l))
      case s => error("parse error while creating list with %s" format s)
    }),
    "map" -> InternalFunctionNode("map", (a,ctxt) => a match {
      case List(f:FunctionNode, LiteralNode(l:List[Node])) =>
        seq(l.map(x => {
          val rslt = applyFn(f, List(x), ctxt)
          rslt.right.map(_._1)
        })).right.map(n => LiteralNode(n))          
      case s => error("parse error while creating list with %s" format s)
    }),
    "add" -> ((_:List[Int]).reduceLeft(_+_)),
    "times" -> ((_:List[Int]).reduceLeft(_*_)),
    "div" -> ((_:List[Int]).reduceLeft(_/_)),
    "-" -> ((_:List[Int]).reduceLeft(_-_)),
    "eq" -> eqf,
    "print" -> ((a:List[String]) => println(a.mkString(" "))))

  val defaultContext = Context(baseSymbolTable, 0)

  def applyFn(fNode: Node, args: List[Node], t: Context):Either[String,(Node,Context)] = fNode match {
    case InternalFunctionNode(id, fn) => fn(args,t).right.map((_,t))
    case FunctionNode(_, argNames, body) => handle(body, t.addSymbols(Map(argNames.zip(args):_*)))
    case _ => Left("%s is not a callback function (args: %s)" format (fNode, args.mkString(", ")))
  }

  def handle(nodes: List[Node], t: Context=defaultContext):Either[String,(Node,Context)] = nodes match {
    case Nil => Left("No input")
    case n :: Nil => handle(n, t)
    case n :: ns => handle(n, t) match {
      case l@Left(_) => l
      case Right((_, symb)) => handle(ns, symb)
    }
  }

  def mkFnNode(name: String, nodes: List[Node], body: Node) = 
    toStrList(nodes).right.map(nodeList =>
      FunctionNode(name, nodeList, body))

  def require(name: String, t: Context):Either[String,(Node, Context)] =
    handle(SimpleParser(fromFile(name).mkString).get, t).right.map(r => (UnitNode, r._2))
    
  def handle(n: Node, t: Context):Either[String,(Node, Context)] = n match {
    case IdentNode(id) => t.symbols.get(id) match {
      case None => Left[String,(Node, Context)]("could not find identifier ``%s'' in symbol table (defined keys %s)" format (id,t.symbols.keys.mkString(", ")))
      case Some(v) => Right((v, t))
    }
    case ListNode(IdentNode("if") :: test :: trueform :: falseform) =>
      handle(test, t) match {
        case Right((FalseNode,_)) => if (falseform != Nil) { handle(falseform.head, t) } else { Right((UnitNode, t)) }
        case Right(_) => handle(trueform, t)
        case e => e
      }
      
    case ListNode(IdentNode("set!") :: IdentNode(name) :: body :: Nil) =>
      handle(body, t).right.map(bodyRslt =>
        (UnitNode, t.addSymbols(Map(name -> bodyRslt._1))))
    case ListNode(IdentNode("lambda") :: ListNode(nodes) :: body :: Nil) =>
      t.nextLambdaName() match {
        case (name,ctxt) =>
          mkFnNode(name, nodes, body) match {
            case Left(err) => Left(err)
            case Right(fn) => Right((fn,ctxt))
          }
      }
    case ListNode(IdentNode("require") :: StrLitNode(file) :: Nil) =>
      require(file, t)
    case ListNode(IdentNode("def") :: IdentNode(name) :: ListNode(nodes) :: body :: Nil) => 
      mkFnNode(name, nodes, body).right.map( n =>
        (UnitNode, t.addSymbols(Map(name -> n))))
    case ListNode((idn:Node) :: args) => handle(idn, t).right.flatMap {
      case (fnNode,_) => {
        val parsedargs = seq(args.map(arg => handle(arg,t).right.map(_._1)))
        parsedargs.right.flatMap(args => applyFn(fnNode, args, t).right.map(a => (a._1, t)))
      }
    }
    case l@LiteralNode(_) => Right((l,t))
    case FalseNode => Right((FalseNode,t))
    case e => Left("unsure how to handle %s" format e)
  }

}

// object Main {
//   def main(a: Array[String]) = {
//     val simpleStr = """(- (+ (+ 4 2) (+ 5 9)) 5) (+ 9 2)"""
//     val fStr = "(def plus2 (a) (+ a 2)) (plus2 9)"
//     val lambda = "((lambda (a) (+ a 99)) 1)"
//     val set = "(set! p2 (lambda (a) (+ a 2))) (p2 9)"
//     val str = """(set! p3 "adam")"""
//     val req = """(require "prelude.gts")"""
//     val p = SimpleParser(req)
//     println(p)

//     println(SexpHandler.handle(p.get) match {
//       case Left(err) => "Error: " + err
//       case Right((rslt,st)) => "\n" + rslt.toString + "\n\n symbols: " + st.symbols
//     })
//   }
// }
