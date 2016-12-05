package sjsch.argonaut

import scalaz.~>
import scalaz.NaturalTransformation

import sjsch._
import sjsch.Schema._
import _root_.argonaut._
import Argonaut._

object JsonSchema {
  sealed trait JType[A]
  case object JNullT   extends JType[Unit]
  case object JBoolT   extends JType[Boolean]

  //case object JByteT   extends JType[Byte]
  case object JShortT  extends JType[Short]
  case object JIntT    extends JType[Int]
  case object JLongT   extends JType[Long]

  case object JFloatT  extends JType[Float]
  case object JDoubleT extends JType[Double]

  case object JCharT   extends JType[Char]
  case object JStrT    extends JType[String]
  case class JConst[A](a: A) extends JType[A]

  def jNull[E] =   prim[E, JType, Unit](JNullT)
  def jBool[E] =   prim[E, JType, Boolean](JBoolT)
  def jShort[E] =  prim[E, JType, Short](JShortT)
  def jInt[E] =    prim[E, JType, Int](JIntT)
  def jLong[E] =   prim[E, JType, Long](JLongT)
  def jFloat[E] =  prim[E, JType, Float](JFloatT)
  def jDouble[E] = prim[E, JType, Double](JDoubleT)
  def jChar[E] =   prim[E, JType, Char](JCharT)
  def jStr[E] =    prim[E, JType, String](JStrT)
  def jConst[E, A](a: A) = prim[E, JType, A](JConst(a))

  def toDecodeJson: JType ~> DecodeJson = new NaturalTransformation[JType, DecodeJson] {
    def apply[A](jtype: JType[A]): DecodeJson[A] = jtype match {
      case JNullT  => implicitly[DecodeJson[Unit]]
      case JBoolT  => implicitly[DecodeJson[Boolean]]
      case JShortT => implicitly[DecodeJson[Short]]
      case JIntT   => implicitly[DecodeJson[Int]]
      case JLongT  => implicitly[DecodeJson[Long]]
      case JFloatT => implicitly[DecodeJson[Float]]
      case JDoubleT => implicitly[DecodeJson[Double]]
      case JCharT  => implicitly[DecodeJson[Char]]
      case JStrT   => implicitly[DecodeJson[String]]
      case JConst(a) => DecodeJson(_ => DecodeResult.ok(a))
    }
  }

  def toDecodeJson[E, P[_]](primDecodeJson: P ~> DecodeJson, err: E => String): Schema[E, P, ?] ~> DecodeJson = {
    new NaturalTransformation[Schema[E, P, ?], DecodeJson] {
      def apply[A](s: Schema[E, P, A]): DecodeJson[A] = s.tail match {
        case s: PrimitiveSchema[E, P, Schema[E, P, ?], A] => primDecodeJson(s.prim)
        case s: ObjectSchema[E, P, Schema[E, P, ?], A] => ???
        case s: ArraySchema[E, P, Schema[E, P, ?], a] => ???
        case s: OneOfSchema[E, P, Schema[E, P, ?], A] => ???
        case s: ParseSchema[E, P, Schema[E, P, ?], a, b] => ???
        case s: LazySchema[E, P, Schema[E, P, ?], A] => ???
      }
    }
  }
}
