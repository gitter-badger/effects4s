/*
 * Copyright (c) 2017 by its authors. Some rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.effects4s.laws.util

/** A type-class for testing equality. */
trait Eq[T] { def eqv(lhs: T, rhs: T): Boolean }

object Eq extends EqLevel1 {
  implicit val stringEq: Eq[String] = new Eq[String] {
    def eqv(lhs: String, rhs: String): Boolean = lhs == rhs
  }

  implicit val intEq: Eq[Int] = new Eq[Int] {
    def eqv(lhs: Int, rhs: Int): Boolean = lhs == rhs
  }

  implicit val longEq: Eq[Long] = new Eq[Long] {
    def eqv(lhs: Long, rhs: Long): Boolean = lhs == rhs
  }

  implicit val floatEq: Eq[Float] = new Eq[Float] {
    def eqv(lhs: Float, rhs: Float): Boolean = lhs == rhs
  }

  implicit val doubleEq: Eq[Double] = new Eq[Double] {
    def eqv(lhs: Double, rhs: Double): Boolean = lhs == rhs
  }

  implicit val shortEq: Eq[Short] = new Eq[Short] {
    def eqv(lhs: Short, rhs: Short): Boolean = lhs == rhs
  }

  implicit val byteEq: Eq[Byte] = new Eq[Byte] {
    def eqv(lhs: Byte, rhs: Byte): Boolean = lhs == rhs
  }

  implicit val charEq: Eq[Char] = new Eq[Char] {
    def eqv(lhs: Char, rhs: Char): Boolean = lhs == rhs
  }

  implicit val booleanEq: Eq[Boolean] = new Eq[Boolean] {
    def eqv(lhs: Boolean, rhs: Boolean): Boolean = lhs == rhs
  }

  implicit val exceptionEq: Eq[Throwable] =
    new Eq[Throwable] {
      def eqv(lhs: Throwable, rhs: Throwable): Boolean =
        lhs == rhs
    }
}

private[util] abstract class EqLevel1 extends EqLevel0 {
  implicit def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
    new Eq[Option[A]] {
      def eqv(lhs: Option[A], rhs: Option[A]): Boolean =
        (lhs.isEmpty && rhs.isEmpty) ||
          lhs.exists(a => rhs.exists(b => A.eqv(a, b)))
    }
}

private[util] abstract class EqLevel0 {
  implicit def canEqualEq[A](implicit ev: A <:< Equals): Eq[A] =
    new Eq[A] {
      def eqv(lhs: A, rhs: A): Boolean =
        rhs.canEqual(lhs) && lhs == rhs
    }
}
