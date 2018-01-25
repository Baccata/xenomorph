package xenomorph.runtime

import monocle.Getter
import xenomorph.Schema.Schema
import xenomorph.{PropSchema, Required, Schema}

import scalaz.FreeAp

case class Fix[F[_]](unfix: F[Fix[F]])

sealed trait RuntimeModelF[+A]
sealed trait RuntimeModelSuccess[+A] extends RuntimeModelF[A]
final case class RuntimePrimitive[I](value: I) extends RuntimeModelSuccess[Nothing]
final case class RuntimeRecord[+A](values: Map[String, A]) extends RuntimeModelSuccess[A]
final case class RuntimeOneOf[+A](index: Int, value: A) extends RuntimeModelSuccess[A]
final case class RuntimeOptionalValue[+A](value : Option[A]) extends RuntimeModelSuccess[A]

sealed trait RuntimeModelError[+A] extends RuntimeModelF[A]
final case class NotRecordFieldAccess[+A](fieldName : String, model : A) extends RuntimeModelError[A]
final case class AbsentField[+A](fieldName : String, model : A) extends RuntimeModelError[A]

sealed trait DynamicSchemaF[P[_], I]
case class DynamicPrimitive[P[_]](fi: P[i] forSome { type i }) extends DynamicSchemaF[P, Fix[RuntimeModelF]]

object Runtime {

  type RuntimeModel = Fix[RuntimeModelF]

  type DSchema[P[_], A] = Schema[DynamicSchemaF[P, ?], A]

  type DPropSchema[P[_]] = PropSchema[RuntimeModel, DSchema[P, ?], RuntimeModel]

  type DProp[P[_]] = FreeAp[PropSchema[RuntimeModel, DSchema[P, ?], ?], RuntimeModel]

  def prim[P[_]](pi: P[i] forSome { type i }): DSchema[P, RuntimeModel] =
    Schema.prim[DynamicSchemaF[P, ?], RuntimeModel](DynamicPrimitive(pi))

  private def fieldGetter(fieldName : String) : Getter[RuntimeModel, RuntimeModel] = {
    case model@Fix(RuntimeRecord(map)) => map.getOrElse(fieldName, Fix(AbsentField(fieldName, model)))
    case model => Fix(NotRecordFieldAccess(fieldName, model))
  }

  def req[P[_]](fieldName: String, valueSchema: DSchema[P, RuntimeModel]) : DPropSchema[P] =
    Required(fieldName, valueSchema, fieldGetter(fieldName), None)

  def required[P[_]](fieldName: String, valueSchema: DSchema[P, RuntimeModel]): DProp[P] =
    Schema.required[DynamicSchemaF[P, ?], RuntimeModel, RuntimeModel](fieldName, valueSchema, fieldGetter(fieldName))



  private def optionalFieldGetter(fieldName : String) : Getter[RuntimeModel, RuntimeModel] = {
    case Fix(RuntimeRecord(map)) => Fix(RuntimeOptionalValue(map.get(fieldName)))
    case model => Fix(NotRecordFieldAccess(fieldName, model))
  }

  def optional[P[_]](fieldName: String, valueSchema: DSchema[P, RuntimeModel]): DProp[P] =
    Schema.required[DynamicSchemaF[P, ?], RuntimeModel, RuntimeModel](fieldName, valueSchema, optionalFieldGetter(fieldName))


  import scalaz.syntax.traverse._
  import scalaz.std.list._


  // avoid calling Schema.required ro Schema.optional too early : we need the field names
  def rec[P[_]](props : List[DPropSchema[P]]) : DSchema[P, RuntimeModel] = {
    val x : FreeAp[PropSchema[RuntimeModel, DSchema[P, ?], ?], RuntimeModel] =
      props.traverse(p => FreeAp.lift(p).map(p.fieldName -> _)).map(listKV => Fix(RuntimeRecord(listKV.toMap)))

    Schema.rec(x)
  }


}
