package eu.swdev.web.style

/**
 * Base trait for implicit conversions that enable style definitions. Style definitions allow to set, add, remove, or specify
 * default attribute values.
 *
 * The StyleDefs trait is generic because style definitions can appear in different contexts (e.g. on attribute sets
 * or styled items). The R type parameter determines the result type of a style definition.
 *
 * NB: Derivatives of the StyleDefs trait can not be value classes because StyleDefs instances are used polymorphically.
 * In addition, when a method is invoked on a value class that is inherited from a universal trait then the value class
 * is instantiated.
 *
 * @tparam R The type of the result that is returned when a style definition has been made.
 */
trait StyleDefs[R] {

  def result(f: AttrsT): R

  def noop: R

  /**
   * A style definition for setting attribute values. The new values replace an existing set of values.
   *
   * @return
   */
  def := = new AssignDef(this)

  /**
   * A style definition for adding attribute values. The new values are added to a set of values.
   *
   * @return
   */
  def += = new PlusDef(this)

  /**
   * A style definition for removing attribute values. The new values are removed from an existing set of values.
   *
   * @return
   */
  def -= = new MinusDef(this)

  /**
   * A style definition for setting attribute values only if the attribute has not yet a value.
   *
   * @return
   */
  def ~= = new TildeDef(this)
}

/**
 * Base trait for style definitions.
 *
 * The trait defines several overloaded variants of the apply method. Each variant allows to supply the arguments for
 * the operation in a different way. The apply method implementations must not be defined in the base trait because
 * if a value class inherits a method implementation from a universal trait then it gets allocated if that
 * method is invoked. (This is a current implementation limitation of value classes.)
 *
 * NB: The trait must extends Any (i.e. it must be a "universal trait") in order to be usable as a mixin for value classes.
 *
 * @tparam R
 */
trait StyleDef[R] extends Any {

  // start duplicated block of apply methods
  def apply[S: AttributeValue](attrName: String, value: Set[S]): R

  def apply[S: AttributeValue](attrName: String, value: S*): R

  def apply[S: AttributeValue](check: Boolean, attrName: String, value: S*): R

  def apply(attr: Option[Attr]): R
  // end duplicated block of apply methods

}

class AssignDef[R](val styleOps: StyleDefs[R]) extends AnyVal with StyleDef[R] {
  // start duplicated block of apply methods
  def apply[S: AttributeValue](attrName: String, value: Set[S]): R = styleOps.result(mapper(attrName, value.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](attrName: String, value: S*): R = styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](check: Boolean, attrName: String, value: S*): R = if (check) styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))) else styleOps.noop

  def apply(attr: Option[Attr]): R = attr match {
    case Some(a) => styleOps.result(mapper(a.attrName, a.attrValue))
    case None => styleOps.noop
  }
  // end duplicated block of apply methods
  def mapper(attrName: String, value: Set[String]): AttrsT = m => m + (attrName -> value)
}

class PlusDef[R](val styleOps: StyleDefs[R]) extends AnyVal with StyleDef[R] {
  // start duplicated block of apply methods
  def apply[S: AttributeValue](attrName: String, value: Set[S]): R = styleOps.result(mapper(attrName, value.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](attrName: String, value: S*): R = styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](check: Boolean, attrName: String, value: S*): R = if (check) styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))) else styleOps.noop

  def apply(attr: Option[Attr]): R = attr match {
    case Some(a) => styleOps.result(mapper(a.attrName, a.attrValue))
    case None => styleOps.noop
  }
  // end duplicated block of apply methods
  def mapper(attrName: String, value: Set[String]): AttrsT = m => m + (attrName -> (m.getOrElse(attrName, Set()) ++ value))
}

class MinusDef[R](val styleOps: StyleDefs[R]) extends AnyVal with StyleDef[R] {
  // start duplicated block of apply methods
  def apply[S: AttributeValue](attrName: String, value: Set[S]): R = styleOps.result(mapper(attrName, value.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](attrName: String, value: S*): R = styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](check: Boolean, attrName: String, value: S*): R = if (check) styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))) else styleOps.noop

  def apply(attr: Option[Attr]): R = attr match {
    case Some(a) => styleOps.result(mapper(a.attrName, a.attrValue))
    case None => styleOps.noop
  }
  // end duplicated block of apply methods
  def mapper(attrName: String, value: Set[String]): AttrsT = m => if (m.contains(attrName)) m + (attrName -> (m(attrName) -- value)) else m
}

class TildeDef[R](val styleOps: StyleDefs[R]) extends AnyVal with StyleDef[R] {
  // start duplicated block of apply methods
  def apply[S: AttributeValue](attrName: String, value: Set[S]): R = styleOps.result(mapper(attrName, value.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](attrName: String, value: S*): R = styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x))))

  def apply[S: AttributeValue](check: Boolean, attrName: String, value: S*): R = if (check) styleOps.result(mapper(attrName, value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))) else styleOps.noop

  def apply(attr: Option[Attr]): R = attr match {
    case Some(a) => styleOps.result(mapper(a.attrName, a.attrValue))
    case None => styleOps.noop
  }
  // end duplicated block of apply methods
  def mapper(attrName: String, value: Set[String]): AttrsT = m => if (m.contains(attrName)) m else m + (attrName -> value)
}
