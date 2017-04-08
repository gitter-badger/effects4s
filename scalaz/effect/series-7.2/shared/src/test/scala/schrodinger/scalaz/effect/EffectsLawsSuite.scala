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

package schrodinger.scalaz.effect

import cats.kernel.Eq
import scalaz.{-\/, \/-}
import scalaz.syntax.all._
import scalaz.effect.IO

trait EffectsLawsSuite extends schrodinger.BaseLawsSuite {
  // Needed for property checking
  implicit def eqIO[A](implicit A: Eq[A]) =
    new Eq[IO[A]] {
      override def eqv(x: IO[A], y: IO[A]): Boolean =
        x.catchLeft.unsafePerformIO() match {
          case \/-(a) =>
            y.catchLeft.unsafePerformIO() match {
              case \/-(b) => A.eqv(a, b)
              case _ => false
            }
          case ref @ -\/(_) =>
            ref == y.catchLeft.unsafePerformIO()

        }
    }

  // Needed for law checking
  implicit object catsIOInstance extends cats.MonadError[IO, Throwable] {
    override def pure[A](x: A): IO[A] =
      IO(x)
    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] =
      IO.tailrecM[A, B](a =>
        f(a).map {
          case Left(av) => -\/(av)
          case Right(bv) => \/-(bv)
        })(a)
    override def raiseError[A](e: Throwable): IO[A] =
      IO.throwIO(e)
    override def handleErrorWith[A](fa: IO[A])(f: (Throwable) => IO[A]): IO[A] =
      fa.attempt.flatMap {
        case \/-(v) => IO(v)
        case -\/(ex) => f(ex)
      }
  }
}
