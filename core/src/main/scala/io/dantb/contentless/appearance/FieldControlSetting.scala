/*
 * Copyright 2023 Daniel Tattan-Birch
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

package io.dantb.contentless.appearance

import cats.{Eq, Show}
import cats.syntax.all.*
import io.circe.Json

// TODO test invalid combinations to see how API responds
sealed abstract class FieldControlSetting(val name: String)

object FieldControlSetting:
  given show: Show[FieldControlSetting] = Show.show {
    case CustomSetting(name, json) => show"CustomSetting(name = $name, json = $json)"
    case other                     => other.toString
  }

  given eq: Eq[FieldControlSetting] = Eq.instance {
    case (CustomSetting(n1, json1), CustomSetting(n2, json2)) => n1 === n2 && Eq.eqv(json1, json2)
    case (a, b)                                               => a == b
  }

  final case class CustomSetting(theName: String, toJson: Json) extends FieldControlSetting(theName)

  object Boolean:
    final case class TrueLabel(value: String) extends FieldControlSetting(TrueLabel.Name)
    object TrueLabel:
      val Name: String = "trueLabel"
    final case class FalseLabel(value: String) extends FieldControlSetting(FalseLabel.Name)
    object FalseLabel:
      val Name: String = "falseLabel"

  object Rating:
    final case class Stars(value: Int) extends FieldControlSetting(Stars.Name)
    object Stars:
      val Name: String = "stars"

  object DatePicker:
    sealed abstract case class Format(value: String) extends FieldControlSetting(Format.Name)
    object Format:
      val Name: String = "format"

      object DateOnly extends Format("dateonly")
      object Time     extends Format("time")
      object TimeZ    extends Format("timeZ")

      def parse(value: String): Option[Format] =
        value match
          case DateOnly.value => DateOnly.some
          case Time.value     => Time.some
          case TimeZ.value    => TimeZ.some
          case _              => none

    sealed abstract case class ClockType(value: String) extends FieldControlSetting(ClockType.Name)
    object ClockType:
      val Name: String = "ampm"

      object TwelveHour     extends ClockType("12")
      object TwentyFourHour extends ClockType("24")

      def parse(value: String): Option[ClockType] =
        value match
          case TwelveHour.value     => TwelveHour.some
          case TwentyFourHour.value => TwentyFourHour.some
          case _                    => none

  final case class HelpText(value: String) extends FieldControlSetting(HelpText.Name)
  object HelpText:
    val Name: String = "helpText"

  object LinksEditor:
    final case class BulkEditing(value: Boolean) extends FieldControlSetting(BulkEditing.Name)
    object BulkEditing:
      val Name: String = "bulkEditing"
    final case class ShowLinkEntityAction(value: Boolean) extends FieldControlSetting(ShowLinkEntityAction.Name)
    object ShowLinkEntityAction:
      val Name: String = "showLinkEntityAction"
    final case class ShowCreateEntityAction(value: Boolean) extends FieldControlSetting(ShowCreateEntityAction.Name)
    object ShowCreateEntityAction:
      val Name: String = "showCreateEntityAction"
