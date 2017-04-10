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

/** A shim for an `ApplicativeError` type, which we need in order to describe
  * laws for what would be otherwise lawless type-classes.
  */
trait ApplicativeError[F[_], E] extends Applicative[F] {
  /** Lift an error into the `F` context. */
  def raiseError[A](e: E): F[A]

  /** Handle any error, potentially recovering from it, by mapping it to
    * an `F[A]` value.
    */
  def handleWith[A](fa: F[A])(f: E => F[A]): F[A]
}

object ApplicativeError {
  @inline def apply[F[_], E](implicit F: ApplicativeError[F, E]): ApplicativeError[F, E] = F
}