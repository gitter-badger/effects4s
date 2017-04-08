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

import schrodinger.Async

import scalaz.{-\/, \/-}
import scalaz.concurrent.Task

/** Schrodinger integration with Scalaz's `Task`. */
class TaskInstances extends Async[Task] {
  override def create[A](f: ((Either[Throwable, A]) => Unit) => Unit): Task[A] =
    Task.async { register =>
      f {
        case Right(v) => register(\/-(v))
        case Left(ex) => register(-\/(ex))
      }
    }

  override def defer[A](fa: => Task[A]): Task[A] =
    Task.suspend(fa)

  override def unsafeExtractAsync[A](fa: Task[A])(cb: (Either[Throwable, A]) => Unit): Unit =
    fa.unsafePerformAsync {
      case \/-(v) => cb(Right(v))
      case -\/(ex) => cb(Left(ex))
    }

  override def unsafeExtractTrySync[A](fa: Task[A])(cb: (Either[Throwable, A]) => Unit): Either[Unit, A] =
    fa.unsafePerformSyncAttempt match {
      case \/-(v) => Right(v)
      case -\/(ex) => cb(Left(ex)); Left(())
    }

  override def eval[A](f: => A): Task[A] =
    Task.delay(f)
}

object TaskInstances extends TaskInstances