# Appearance Customisation

## Field appearance
There's an "Appearance" tab in Contentful's field editor. These configrations are exposed through [editor interfaces](https://www.contentful.com/developers/docs/references/content-management-api/#/reference/editor-interface) in the [Contentful Management API](https://www.contentful.com/developers/docs/references/content-management-api/). If appearance is customised in the UI only, some of this information is not present in the API responses. Therefore, `contentless` provides defaults for every field type in line with what you'd see in the Contentful UI by default.

Field appearance can be customised in several ways, including help text, field-specific and custom settings. `contentless` tries to make these consistent with the UI. For example, to make `displayDate` appear using a 12 hour clock along with help text:

```scala mdoc:silent
zonedDateTime(
  "displayDate",
  "Display Date",
  dateTimeControl = DateTimeControl.ZonedDefault
    .withClockType(ClockType.TwelveHour)
    .withHelpText("Enter zoned display date in 12H format")
).required
```

All controls have a default in their companion object which can be overriden through the DSL.

## Entry appearance
We can also modify the appearance of the entire entry editor using extensions, apps or sidebar customisations. Changes can be made manually in the UI using the "Sidebar" and "Entry editors" tabs. 

If using `contentless` for content management, it's also recommended to use it for appearance customisations. This avoids divergence between the code model and what we see in the CMS. `contentless` abstracts away the fact that editor interfaces and content types are exposed through different APIs. For example, we could reorder or replace the sidebar widgets using a similar declarative style:

```scala mdoc:silent
articleCodec.withSidebar(List(SidebarWidget.BuiltIn.InfoPanel, SidebarWidget.BuiltIn.Publication, SidebarWidget.BuiltIn.Users))
```

All of this is accessible via the editor interfaces API: changes are tracked and appearance customisations can be regenerated in any Contentful space or environment from code.
