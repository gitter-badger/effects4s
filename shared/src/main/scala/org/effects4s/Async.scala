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

import scala.annotation.implicitNotFound
import scala.language.higherKinds

/** Type-class describing `F[_]` data types capable of executing
  * asynchronous computations that produce a single result.
  */
@implicitNotFound("""Cannot find implicit value for Async[${F}].
Building this implicit value might depend on having an implicit
s.c.ExecutionContext in scope or some other equivalent type.""")
trait Async[F[_]] extends Eventual[F] with Deferrable[F] {
  /** Creates an `F[A]` instance from a provided function
    * that will have a callback injected for signaling the
    * final result of an asynchronous process.
    *
    * @param f is a function that will be called with a callback
    *          for signaling the result once it is ready
    */
  def create[A](f: (Either[Throwable, A] => Unit) => Unit): F[A]
}

object Async {
  /** Returns the [[Async]] instance for a given `F` type. */
  @inline def apply[F[_]](implicit F: Async[F]): Async[F] = F
}
