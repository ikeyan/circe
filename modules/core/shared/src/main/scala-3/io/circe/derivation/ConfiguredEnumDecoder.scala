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
import io.circe.{ Decoder, DecodingFailure, HCursor }

trait ConfiguredEnumDecoder[A] extends Decoder[A]
object ConfiguredEnumDecoder:
  private def of[A](name: String, cases: List[SingletonCase[A]])(using
    conf: Configuration
  ): ConfiguredEnumDecoder[A] = new ConfiguredEnumDecoder[A]:
    private val labelsMap =
      cases.map(c => (conf.transformConstructorNames(c.label), c.value)).toMap[String, A]
    
    def apply(c: HCursor) = c.as[String].flatMap { caseName =>
      labelsMap.get(caseName) match
        case None    => Left(DecodingFailure(s"enum $name does not contain case: $caseName", c.history))
        case Some(a) => Right(a)
    }

  inline final def derived[A](using conf: Configuration, mirror: Mirror.SumOf[A]): ConfiguredEnumDecoder[A] =
    ConfiguredEnumDecoder.of[A](
      constValue[mirror.MirroredLabel],
      summonSingletonCases[mirror.MirroredElemTypes, A](constValue[mirror.MirroredLabel]),
    )

  inline final def derive[R: Mirror.SumOf](
    transformConstructorNames: String => String = Configuration.default.transformConstructorNames
  ): Decoder[R] =
    derived[R](using Configuration.default.withTransformConstructorNames(transformConstructorNames))
