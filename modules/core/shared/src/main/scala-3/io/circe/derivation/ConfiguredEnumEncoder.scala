/*
 * Copyright 2024 circe
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

package io.circe.derivation

import scala.deriving.Mirror
import scala.compiletime.constValue
import io.circe.{ Encoder, Json }

trait ConfiguredEnumEncoder[A] extends Encoder[A]
object ConfiguredEnumEncoder:
  private def of[A](cases: List[SingletonCase[A]])(using conf: Configuration): ConfiguredEnumEncoder[A] =
    new ConfiguredEnumEncoder[A]:
      private val valuesMap =
        cases.map(c => (c.value, conf.transformConstructorNames(c.label))).toMap[A, String]

      def apply(a: A) = Json.fromString(valuesMap(a))

  inline final def derived[A](using conf: Configuration, mirror: Mirror.SumOf[A]): ConfiguredEnumEncoder[A] =
    ConfiguredEnumEncoder.of[A](
      cases = summonSingletonCases[mirror.MirroredElemTypes, A](constValue[mirror.MirroredLabel])
    )

  inline final def derive[A: Mirror.SumOf](
    transformConstructorNames: String => String = Configuration.default.transformConstructorNames
  ): Encoder[A] =
    derived[A](using Configuration.default.withTransformConstructorNames(transformConstructorNames))
