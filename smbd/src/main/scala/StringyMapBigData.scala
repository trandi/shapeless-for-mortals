// Copyright (C) 2015 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
/**
 * TypeClass (api/impl/syntax) for marshalling objects into
 * `java.util.HashMap<String,Object>` (yay, big data!).
 */
package s4m.smbd

import shapeless._, labelled.{ field, FieldType }

/**
 * This exercise involves writing tests, only a skeleton is provided.
 *
 * - Exercise 1.1: derive =BigDataFormat= for sealed traits.
 * - Exercise 1.2: define identity constraints using singleton types.
 */
package object api {
  type StringyMap = java.util.HashMap[String, AnyRef]
  type BigResult[T] = Either[String, T] // aggregating errors doesn't add much
}

package api {
  trait BigDataFormat[T] {
    def label: String
    def toProperties(t: T): StringyMap
    def fromProperties(m: StringyMap): BigResult[T]
  }

  trait SPrimitive[V] {
    def toValue(v: V): AnyRef
    def fromValue(v: AnyRef): V
  }

  // EXERCISE 1.2
  trait BigDataFormatId[T, V] {
    def key: String
    def value(t: T): V
  }
}

package object impl {
  import api._

  // EXERCISE 1.1 goes here
  implicit object StringSPrimitive extends SPrimitive[String] {
    override def toValue(v: String): AnyRef = v

    override def fromValue(v: AnyRef): String = v.asInstanceOf[String]
  }

  implicit object IntSPrimitive extends SPrimitive[Int] {
    override def toValue(v: Int): AnyRef = java.lang.Integer.valueOf(v)

    override def fromValue(v: AnyRef): Int = v.asInstanceOf[java.lang.Integer].intValue
  }

  implicit object DoubleSPrimitive extends SPrimitive[Double] {
    override def toValue(v: Double): AnyRef = java.lang.Double.valueOf(v)

    override def fromValue(v: AnyRef): Double = v.asInstanceOf[java.lang.Double].doubleValue
  }

  implicit object BooleanSPrimitive extends SPrimitive[Boolean] {
    override def toValue(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)

    override def fromValue(v: AnyRef): Boolean = v.asInstanceOf[java.lang.Boolean].booleanValue
  }



  implicit def hNilBigDataFormat = new BigDataFormat[HNil] {
    override def label: String = "HNil"

    override def toProperties(t: HNil): StringyMap = new StringyMap()

    override def fromProperties(m: StringyMap): BigResult[HNil] = Right(HNil)
  }

  implicit def hListBigDataFormat[Key <: Symbol, Head, Tail <: HList](
     implicit key: Witness.Aux[Key],
     sprimitive: SPrimitive[Head], // what happens if we have another product nested instead of a Primitive !???
     lazyTailFormat: Lazy[BigDataFormat[Tail]])
  = new BigDataFormat[FieldType[Key, Head] :: Tail] {

    val tf = lazyTailFormat.value

    override def label: String = "HList"

    override def toProperties(t: FieldType[Key, Head] :: Tail): StringyMap = {
      val res = tf.toProperties(t.tail)
      res.put(key.value.name, sprimitive.toValue(t.head))
      res
    }

    override def fromProperties(m: StringyMap): BigResult[FieldType[Key, Head] :: Tail] = {
      // TODO fix the naked get()'s
      val r = field[Key](sprimitive.fromValue(m.get(key.value.name))) :: tf.fromProperties(m).right.get
      Right(r)
    }
  }

  implicit def cNilBigDataFormat = new BigDataFormat[CNil] {
    override def label: String = "CNil"

    override def toProperties(t: CNil): StringyMap = new StringyMap()

    override def fromProperties(m: StringyMap): BigResult[CNil] = Left("Can't create CNil from props")
  }


  implicit def coproductBigDataFormat[Key <: Symbol, Head, Tail <: Coproduct](
    implicit key: Witness.Aux[Key],
    lazyHeadFormat: Lazy[BigDataFormat[Head]], // this will be a product itself not a primitive
    lazyTailFormat: Lazy[BigDataFormat[Tail]]
    )
  = new BigDataFormat[FieldType[Key, Head] :+: Tail] {

    val hf = lazyHeadFormat.value
    val tf = lazyTailFormat.value

    override def label: String = "Coproduct"

    override def toProperties(t: FieldType[Key, Head] :+: Tail): StringyMap = {
      // TODO fix the naked "get()'s"
      val res = tf.toProperties(t.tail.get)
      res.putAll(hf.toProperties(t.head.get))
      res
    }

    override def fromProperties(m: StringyMap): BigResult[FieldType[Key, Head] :+: Tail] = {
      // TODO fix naked get()s
      val r = Inl(field[Key](hf.fromProperties(m).right.get))
      //val t = Inr(field[Key](tf.fromProperties(m).right.get))
      // TODO why take r rather than t ???
      Right(r)
    }
  }


  implicit def familyBigDataFormat[T, Repr](
     implicit gen: LabelledGeneric.Aux[T, Repr],
     lazyFormat: Lazy[BigDataFormat[Repr]],
     tpe: Typeable[T])
  = new BigDataFormat[T] {

    val f = lazyFormat.value

    override def label: String = "Family"

    override def toProperties(t: T): StringyMap = {
      f.toProperties(gen.to(t))
    }

    override def fromProperties(m: StringyMap): BigResult[T] = {
      Right(gen.from(f.fromProperties(m).right.get))
    }
  }
}

package impl {
  //import api._

  // EXERCISE 1.2 goes here
}

package object syntax {
  import api._

  implicit class RichBigResult[R](val e: BigResult[R]) extends AnyVal {
    def getOrThrowError: R = e match {
      case Left(error) => throw new IllegalArgumentException(error.mkString(","))
      case Right(r) => r
    }
  }

  /** Syntactic helper for serialisables. */
  implicit class RichBigDataFormat[T](val t: T) extends AnyVal {
    def label(implicit s: BigDataFormat[T]): String = s.label
    def toProperties(implicit s: BigDataFormat[T]): StringyMap = s.toProperties(t)
    def idKey[P](implicit lens: Lens[T, P]): String = ???
    def idValue[P](implicit lens: Lens[T, P]): P = lens.get(t)
  }

  implicit class RichProperties(val props: StringyMap) extends AnyVal {
    def as[T](implicit s: BigDataFormat[T]): T = s.fromProperties(props).getOrThrowError
  }
}
