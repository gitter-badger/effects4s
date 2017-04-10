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

import monix.execution.schedulers.TestScheduler
import org.effects4s.laws.util._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.Laws

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait BaseLawsSuite extends FunSuite with Checkers with AllInstances {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJVM) 100 else 10,
      maxDiscardedFactor = if (Platform.isJVM) 5.0 else 50.0,
      sizeRange = 32
    )

  def checkAll(name: String, ruleSet: Laws#RuleSet) {
    for ((id, prop) ← ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
  }
}

trait AllInstances {
  implicit def futureInstances(implicit s: TestScheduler): Monad[Future] with ApplicativeError[Future, Throwable] =
    new Monad[Future] with ApplicativeError[Future, Throwable] {
      def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] =
        fa.flatMap(f)
      def map2[A, B, Z](fa: Future[A], fb: Future[B])(f: (A, B) => Z): Future[Z] =
        for (a <- fa; b <- fb) yield f(a, b)
      def pure[A](a: A): Future[A] =
        Future.successful(a)
      def ap[A, B](ff: Future[(A) => B])(fa: Future[A]): Future[B] =
        for (f <- ff; a <- fa) yield f(a)
      def map[A, B](fa: Future[A])(f: (A) => B): Future[B] =
        fa.map(f)
      def raiseError[A](e: Throwable): Future[A] =
        Future.failed(e)
      def handleWith[A](fa: Future[A])(f: (Throwable) => Future[A]): Future[A] =
        fa.recoverWith { case ex => f(ex) }
    }

  implicit def futureEq[A : Eq](implicit s: TestScheduler): Eq[Future[A]] =
    new Eq[Future[A]] {
      override def eqv(x: Future[A], y: Future[A]): Boolean = {
        val A = implicitly[Eq[A]]
        s.tick(1.day)

        x.value.exists {
          case Failure(_) =>
            y.value.exists {
              case Success(_) => false
              case Failure(_) => true
            }
          case Success(a) =>
            y.value.exists {
              case Success(b) => A.eqv(a, b)
              case Failure(_) => false
            }
        }
      }
    }
}