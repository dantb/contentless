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

package io.dantb.contentless

enum MimeTypeGroup(val typeName: String):
  case Archive      extends MimeTypeGroup("archive")
  case Attachment   extends MimeTypeGroup("attachment")
  case Audio        extends MimeTypeGroup("audio")
  case Code         extends MimeTypeGroup("code")
  case Image        extends MimeTypeGroup("image")
  case Markup       extends MimeTypeGroup("markup")
  case Pdfdocument  extends MimeTypeGroup("pdfdocument")
  case Plaintext    extends MimeTypeGroup("plaintext")
  case Presentation extends MimeTypeGroup("presentation")
  case Richtext     extends MimeTypeGroup("richtext")
  case Spreadsheet  extends MimeTypeGroup("spreadsheet")
  case Video        extends MimeTypeGroup("video")
