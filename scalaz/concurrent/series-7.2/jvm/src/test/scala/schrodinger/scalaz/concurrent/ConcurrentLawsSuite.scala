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

package schrodinger.scalaz.concurrent

import cats.kernel.Eq
import schrodinger.BaseLawsSuite
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task

trait ConcurrentLawsSuite extends BaseLawsSuite {
  // Needed for property checking
  implicit def eqTask[A](implicit A: Eq[A]) =
    new Eq[Task[A]] {
      override def eqv(x: Task[A], y: Task[A]): Boolean =
        x.unsafePerformSyncAttempt match {
          case \/-(a) =>
            y.unsafePerformSyncAttempt match {
              case \/-(b) => A.eqv(a, b)
              case _ => false
            }
          case ref @ -\/(_) =>
            ref == y.unsafePerformSyncAttempt
        }
    }

  // Needed for cats-laws
  implicit object taskMonad extends cats.MonadError[Task, Throwable] {
    override def pure[A](x: A): Task[A] =
      Task.point(x)
    override def flatMap[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: (A) => Task[Either[A, B]]): Task[B] =
      Task.tailrecM[A, B](a =>
        f(a).map {
          case Left(av) => -\/(av)
          case Right(bv) => \/-(bv)
        })(a)

    override def raiseError[A](e: Throwable): Task[A] =
      Task.fail(e)
    override def handleErrorWith[A](fa: Task[A])(f: (Throwable) => Task[A]): Task[A] =
      fa.attempt.flatMap {
        case \/-(v) => Task(v)
        case -\/(ex) => f(ex)
      }
  }
}
