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

/** A shim for an `Applicative` type, which we need in order to describe
  * laws for what would be otherwise lawless type-classes.
  */
trait Applicative[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def pure[A](a: A): F[A]
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def unit: F[Unit] = pure(())
}

object Applicative {
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}