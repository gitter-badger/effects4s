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

package org.effects4s
package laws
package discipline

import org.effects4s.laws.util._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}

/** Tests that have to be passed by [[org.effects4s.Deferrable]], assuming
  * that the `F[_]` data-type is also a `Monad`.
  */
trait DeferrableTests[F[_]] extends EvaluableTests[F] {
  def laws: DeferrableLaws[F]

  def deferrable[A: Arbitrary, B: Arbitrary](implicit
    AtoB: Arbitrary[A => B],
    ABtoA: Arbitrary[(A, B) => A],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFInt: Eq[F[Int]]): RuleSet = {

    val stackSafetyIterations =
      if (Platform.isJVM) 50000 else 5000

    new RuleSet {
      def name: String = "deferrable"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(evaluable[A, B])
      def props: Seq[(String, Prop)] = Seq(
        "eval equivalence with defer" -> forAll(laws.evalEquivalenceWithDefer[A, B] _),
        "eval repeats side effects" -> forAll(laws.evalRepeatsSideEffects[A, B] _),
        "defer repeats side effects" -> forAll(laws.deferRepeatsSideEffects[A, B] _),
        "flatMap stack safety" -> Prop.lzy(laws.flatMapStackSafety(stackSafetyIterations))
      )
    }
  }

  def deferrableWithError[A: Arbitrary, B: Arbitrary](implicit
    AtoB: Arbitrary[A => B],
    ABtoA: Arbitrary[(A, B) => A],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFInt: Eq[F[Int]],
    apErr: ApplicativeError[F, Throwable]): RuleSet = {

    new RuleSet {
      def name: String = "deferrableWithError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(deferrable[A, B], evaluableWithError[A, B])
      def props: Seq[(String, Prop)] = Seq.empty
    }
  }
}

object DeferrableTests {
  /** Tests that have to be passed by [[org.effects4s.Deferrable]], assuming
    * that the `F[_]` data-type is also a `Monad`.
    */
  def apply[F[_] : Deferrable : Monad]: DeferrableTests[F] =
    new DeferrableTests[F] {
      override val laws: DeferrableLaws[F] =
        DeferrableLaws[F]
    }
}