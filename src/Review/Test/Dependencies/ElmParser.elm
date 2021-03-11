module Review.Test.Dependencies.ElmHtml exposing (dependency)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create
        "elm/html"
        (createElmJsonProject elmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("Failed to decode elm.json for elm/html: " ++ Debug.toString error)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "elm/html",
    "summary": "Fast HTML, rendered with virtual DOM diffing",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": {
        "HTML": [
            "Html",
            "Html.Attributes",
            "Html.Events"
        ],
        "Optimize": [
            "Html.Keyed",
            "Html.Lazy"
        ]
    },
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "elm/json": "1.0.0 <= v < 2.0.0",
        "elm/virtual-dom": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
"""


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Html"
      , comment = """ This file is organized roughly in order of popularity. The tags which you'd
expect to use frequently will be closer to the top.

# Primitives
@docs Html, Attribute, text, node, map

# Tags

## Headers
@docs h1, h2, h3, h4, h5, h6

## Grouping Content
@docs div, p, hr, pre, blockquote

## Text
@docs span, a, code, em, strong, i, b, u, sub, sup, br

## Lists
@docs ol, ul, li, dl, dt, dd

## Embedded Content
@docs img, iframe, canvas, math

## Inputs
@docs form, input, textarea, button, select, option

## Sections
@docs section, nav, article, aside, header, footer, address, main_

## Figures
@docs figure, figcaption

## Tables
@docs table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th


## Less Common Elements

### Less Common Inputs
@docs fieldset, legend, label, datalist, optgroup, output, progress, meter

### Audio and Video
@docs audio, video, source, track

### Embedded Objects
@docs embed, object, param

### Text Edits
@docs ins, del

### Semantic Text
@docs small, cite, dfn, abbr, time, var, samp, kbd, s, q

### Less Common Text Tags
@docs mark, ruby, rt, rp, bdi, bdo, wbr

## Interactive Elements
@docs details, summary, menuitem, menu

"""
      , unions = []
      , aliases =
            [ { name = "Attribute"
              , comment = """ Set attributes on your `Html`. Learn more in the
[`Html.Attributes`](Html-Attributes) module.
"""
              , args = [ "msg" ]
              , tipe = decodeType "VirtualDom.Attribute msg"
              }
            , { name = "Html"
              , comment = """ The core building block used to build up HTML. Here we create an `Html`
value with no attributes and one child:

    hello : Html msg
    hello =
      div [] [ text "Hello!" ]
"""
              , args = [ "msg" ]
              , tipe = decodeType "VirtualDom.Node msg"
              }
            ]
      , values =
            [ { name = "a"
              , comment = """ Represents a hyperlink, linking to another resource. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "abbr"
              , comment = """ Represents an abbreviation or an acronym; the expansion of the
abbreviation can be represented in the title attribute.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "address"
              , comment = """ Defines a section containing contact information. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "article"
              , comment = """ Defines self-contained content that could exist independently of the rest
of the content.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "aside"
              , comment = """ Defines some content loosely related to the page content. If it is removed,
the remaining content still makes sense.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "audio"
              , comment = """ Represents a sound or audio stream. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "b"
              , comment = """ Represents a text which to which attention is drawn for utilitarian
purposes. It doesn't convey extra importance and doesn't imply an alternate
voice.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "bdi"
              , comment = """ Represents text that must be isolated from its surrounding for
bidirectional text formatting. It allows embedding a span of text with a
different, or unknown, directionality.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "bdo"
              , comment = """ Represents the directionality of its children, in order to explicitly
override the Unicode bidirectional algorithm.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "blockquote"
              , comment = """ Represents a content that is quoted from another source. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "br"
              , comment = """ Represents a line break. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "button"
              , comment = """ Represents a button. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "canvas"
              , comment = """ Represents a bitmap area for graphics rendering. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "caption"
              , comment = """ Represents the title of a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "cite"
              , comment = """ Represents the title of a work. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "code"
              , comment = """ Represents computer code. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "col"
              , comment = """ Represents a column of a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "colgroup"
              , comment = """ Represents a set of one or more columns of a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "datalist"
              , comment = """ Represents a set of predefined options for other controls. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "dd"
              , comment = """ Represents the definition of the terms immediately listed before it. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "del"
              , comment = """ Defines a removal from the document. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "details"
              , comment = """ Represents a widget from which the user can obtain additional information
or controls.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "dfn"
              , comment = """ Represents a term whose definition is contained in its nearest ancestor
content.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "div"
              , comment = """ Represents a generic container with no special meaning. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "dl"
              , comment = """ Defines a definition list, that is, a list of terms and their associated
definitions.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "dt"
              , comment = """ Represents a term defined by the next `dd`. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "em"
              , comment = """ Represents emphasized text, like a stress accent. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "embed"
              , comment = """ Represents a integration point for an external, often non-HTML,
application or interactive content.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "fieldset"
              , comment = """ Represents a set of controls. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "figcaption"
              , comment = """ Represents the legend of a figure. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "figure"
              , comment = """ Represents a figure illustrated as part of the document. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "footer"
              , comment = """ Defines the footer for a page or section. It often contains a copyright
notice, some links to legal information, or addresses to give feedback.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "form"
              , comment = """ Represents a form, consisting of controls, that can be submitted to a
server for processing.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h1"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h2"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h3"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h4"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h5"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "h6"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "header"
              , comment = """ Defines the header of a page or section. It often contains a logo, the
title of the web site, and a navigational table of content.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "hr"
              , comment = """ Represents a thematic break between paragraphs of a section or article or
any longer content.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "i"
              , comment = """ Represents some text in an alternate voice or mood, or at least of
different quality, such as a taxonomic designation, a technical term, an
idiomatic phrase, a thought, or a ship name.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "iframe"
              , comment = """ Embedded an HTML document. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "img"
              , comment = """ Represents an image. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "input"
              , comment = """ Represents a typed data field allowing the user to edit the data. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "ins"
              , comment = """ Defines an addition to the document. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "kbd"
              , comment = """ Represents user input, often from the keyboard, but not necessarily; it
may represent other input, like transcribed voice commands.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "label"
              , comment = """ Represents the caption of a form control. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "legend"
              , comment = """ Represents the caption for a `fieldset`. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "li"
              , comment = """ Defines a item of an enumeration list. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "main_"
              , comment = """ Defines the main or important content in the document. There is only one
`main` element in the document.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "map"
              , comment = """ Transform the messages produced by some `Html`. In the following example,
we have `viewButton` that produces `()` messages, and we transform those values
into `Msg` values in `view`.

    type Msg = Left | Right

    view : model -> Html Msg
    view model =
      div []
        [ map (\\_ -> Left) (viewButton "Left")
        , map (\\_ -> Right) (viewButton "Right")
        ]

    viewButton : String -> Html ()
    viewButton name =
      button [ onClick () ] [ text name ]

This should not come in handy too often. Definitely read [this][reuse] before
deciding if this is what you want.

[reuse]: https://guide.elm-lang.org/reuse/
"""
              , tipe = decodeType "(a -> msg) -> Html.Html a -> Html.Html msg"
              }
            , { name = "mark"
              , comment = """ Represents text highlighted for reference purposes, that is for its
relevance in another context.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "math"
              , comment = """ Defines a mathematical formula. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "menu"
              , comment = """ Represents a list of commands. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "menuitem"
              , comment = """ Represents a command that the user can invoke. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "meter"
              , comment = """ Represents a scalar measurement (or a fractional value), within a known
range.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "nav"
              , comment = """ Defines a section that contains only navigation links.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "node"
              , comment = """ General way to create HTML nodes. It is used to define all of the helper
functions in this library.

    div : List (Attribute msg) -> List (Html msg) -> Html msg
    div attributes children =
        node "div" attributes children

You can use this to create custom nodes if you need to create something that
is not covered by the helper functions in this library.
"""
              , tipe = decodeType "String.String -> List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "object"
              , comment = """ Represents an external resource, which is treated as an image, an HTML
sub-document, or an external resource to be processed by a plug-in.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "ol"
              , comment = """ Defines an ordered list of items. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "optgroup"
              , comment = """ Represents a set of options, logically grouped. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "option"
              , comment = """ Represents an option in a `select` element or a suggestion of a `datalist`
element.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "output"
              , comment = """ Represents the result of a calculation. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "p"
              , comment = """ Defines a portion that should be displayed as a paragraph. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "param"
              , comment = """ Defines parameters for use by plug-ins invoked by `object` elements. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "pre"
              , comment = """ Indicates that its content is preformatted and that this format must be
preserved.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "progress"
              , comment = """ Represents the completion progress of a task. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "q"
              , comment = """ Represents an inline quotation. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "rp"
              , comment = """ Represents parenthesis around a ruby annotation, used to display the
annotation in an alternate way by browsers not supporting the standard display
for annotations.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "rt"
              , comment = """ Represents the text of a ruby annotation. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "ruby"
              , comment = """ Represents content to be marked with ruby annotations, short runs of text
presented alongside the text. This is often used in conjunction with East Asian
language where the annotations act as a guide for pronunciation, like the
Japanese furigana.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "s"
              , comment = """ Represents content that is no longer accurate or relevant. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "samp"
              , comment = """ Represents the output of a program or a computer. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "section"
              , comment = """ Defines a section in a document.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "select"
              , comment = """ Represents a control allowing selection among a set of options. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "small"
              , comment = """ Represents a side comment, that is, text like a disclaimer or a
copyright, which is not essential to the comprehension of the document.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "source"
              , comment = """ Allows authors to specify alternative media resources for media elements
like `video` or `audio`.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "span"
              , comment = """ Represents text with no specific meaning. This has to be used when no other
text-semantic element conveys an adequate meaning, which, in this case, is
often brought by global attributes like `class`, `lang`, or `dir`.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "strong"
              , comment = """ Represents especially important text. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "sub"
              , comment = """ Represent a subscript. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "summary"
              , comment = """ Represents a summary, caption, or legend for a given `details`. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "sup"
              , comment = """ Represent a superscript. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "table"
              , comment = """ Represents data with more than one dimension. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "tbody"
              , comment = """ Represents the block of rows that describes the concrete data of a table.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "td"
              , comment = """ Represents a data cell in a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "text"
              , comment = """ Just put plain text in the DOM. It will escape the string so that it appears
exactly as you specify.

    text "Hello World!"
"""
              , tipe = decodeType "String.String -> Html.Html msg"
              }
            , { name = "textarea"
              , comment = """ Represents a multiline text edit control. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "tfoot"
              , comment = """ Represents the block of rows that describes the column summaries of a table.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "th"
              , comment = """ Represents a header cell in a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "thead"
              , comment = """ Represents the block of rows that describes the column labels of a table.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "time"
              , comment = """ Represents a date and time value; the machine-readable equivalent can be
represented in the datetime attribute.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "tr"
              , comment = """ Represents a row of cells in a table. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "track"
              , comment = """ Allows authors to specify timed text track for media elements like `video`
or `audio`.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "u"
              , comment = """ Represents a non-textual annotation for which the conventional
presentation is underlining, such labeling the text as being misspelt or
labeling a proper name in Chinese text.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "ul"
              , comment = """ Defines an unordered list of items. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "var"
              , comment = """ Represents a variable. Specific cases where it should be used include an
actual mathematical expression or programming context, an identifier
representing a constant, a symbol identifying a physical quantity, a function
parameter, or a mere placeholder in prose.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "video"
              , comment = """ Represents a video, the associated audio and captions, and controls. """
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            , { name = "wbr"
              , comment = """ Represents a line break opportunity, that is a suggested point for
wrapping text in order to improve readability of text split on several lines.
"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
              }
            ]
      , binops = []
      }
    , { name = "Html.Attributes"
      , comment = """ Helper functions for HTML attributes. They are organized roughly by
category. Each attribute is labeled with the HTML tags it can be used with, so
just search the page for `video` if you want video stuff.

# Primitives
@docs style, property, attribute, map

# Super Common Attributes
@docs class, classList, id, title, hidden

# Inputs
@docs type_, value, checked, placeholder, selected

## Input Helpers
@docs accept, acceptCharset, action, autocomplete, autofocus,
    disabled, enctype, list, maxlength, minlength, method, multiple,
    name, novalidate, pattern, readonly, required, size, for, form

## Input Ranges
@docs max, min, step

## Input Text Areas
@docs cols, rows, wrap


# Links and Areas
@docs href, target, download, hreflang, media, ping, rel

## Maps
@docs ismap, usemap, shape, coords


# Embedded Content
@docs src, height, width, alt

## Audio and Video
@docs autoplay, controls, loop, preload, poster, default, kind, srclang

## iframes
@docs sandbox, srcdoc

# Ordered Lists
@docs reversed, start

# Tables
@docs align, colspan, rowspan, headers, scope

# Less Common Global Attributes
Attributes that can be attached to any HTML tag but are less commonly used.
@docs accesskey, contenteditable, contextmenu, dir, draggable, dropzone,
      itemprop, lang, spellcheck, tabindex

# Miscellaneous
@docs cite, datetime, pubdate, manifest

"""
      , unions = []
      , aliases = []
      , values =
            [ { name = "accept"
              , comment = """ List of types the server accepts, typically a file type.
For `form` and `input`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "acceptCharset"
              , comment = """ List of supported charsets in a `form`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "accesskey"
              , comment = """ Defines a keyboard shortcut to activate or add focus to the element. """
              , tipe = decodeType "Char.Char -> Html.Attribute msg"
              }
            , { name = "action"
              , comment = """ The URI of a program that processes the information submitted via a `form`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "align"
              , comment = """ Specifies the horizontal alignment of a `caption`, `col`, `colgroup`,
`hr`, `iframe`, `img`, `table`, `tbody`,  `td`,  `tfoot`, `th`, `thead`, or
`tr`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "alt"
              , comment = """ Alternative text in case an image can't be displayed. Works with `img`,
`area`, and `input`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "attribute"
              , comment = """ Create *attributes*, like saying `domNode.setAttribute('class', 'greeting')`
in JavaScript.

    class : String -> Attribute msg
    class name =
      attribute "class" name

Read more about the difference between properties and attributes [here][].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md
"""
              , tipe = decodeType "String.String -> String.String -> Html.Attribute msg"
              }
            , { name = "autocomplete"
              , comment = """ Indicates whether a `form` or an `input` can have their values automatically
completed by the browser.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "autofocus"
              , comment = """ The element should be automatically focused after the page loaded.
For `button`, `input`, `select`, and `textarea`.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "autoplay"
              , comment = """ The `audio` or `video` should play as soon as possible. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "checked"
              , comment = """ Indicates whether an `input` of type checkbox is checked. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "cite"
              , comment = """ Contains a URI which points to the source of the quote or change in a
`blockquote`, `del`, `ins`, or `q`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "class"
              , comment = """ Often used with CSS to style elements with common properties.

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "classList"
              , comment = """ This function makes it easier to build a space-separated class attribute.
Each class can easily be added and removed depending on the boolean value it
is paired with. For example, maybe we want a way to view notices:

    viewNotice : Notice -> Html msg
    viewNotice notice =
      div
        [ classList
            [ ("notice", True)
            , ("notice-important", notice.isImportant)
            , ("notice-seen", notice.isSeen)
            ]
        ]
        [ text notice.content ]

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!
"""
              , tipe = decodeType "List.List ( String.String, Basics.Bool ) -> Html.Attribute msg"
              }
            , { name = "cols"
              , comment = """ Defines the number of columns in a `textarea`. """
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "colspan"
              , comment = """ The colspan attribute defines the number of columns a cell should span.
For `td` and `th`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "contenteditable"
              , comment = """ Indicates whether the element's content is editable. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "contextmenu"
              , comment = """ Defines the ID of a `menu` element which will serve as the element's
context menu.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "controls"
              , comment = """ Indicates whether the browser should show playback controls for the `audio`
or `video`.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "coords"
              , comment = """ A set of values specifying the coordinates of the hot-spot region in an
`area`. Needs to be paired with a `shape` attribute to be meaningful.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "datetime"
              , comment = """ Indicates the date and time associated with the element.
For `del`, `ins`, `time`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "default"
              , comment = """ Indicates that the `track` should be enabled unless the user's preferences
indicate something different.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "dir"
              , comment = """ Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl
(Right-To-Left).
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "disabled"
              , comment = """ Indicates whether the user can interact with a `button`, `fieldset`,
`input`, `optgroup`, `option`, `select` or `textarea`.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "download"
              , comment = """ Indicates that clicking an `a` and `area` will download the resource
directly. The `String` argument determins the name of the downloaded file.
Say the file you are serving is named `hats.json`.

    download ""               -- hats.json
    download "my-hats.json"   -- my-hats.json
    download "snakes.json"    -- snakes.json

The empty `String` says to just name it whatever it was called on the server.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "draggable"
              , comment = """ Defines whether the element can be dragged. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "dropzone"
              , comment = """ Indicates that the element accept the dropping of content on it. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "enctype"
              , comment = """ How `form` data should be encoded when submitted with the POST method.
Options include: application/x-www-form-urlencoded, multipart/form-data, and
text/plain.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "for"
              , comment = """ The element ID described by this `label` or the element IDs that are used
for an `output`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "form"
              , comment = """ Indicates the element ID of the `form` that owns this particular `button`,
`fieldset`, `input`, `label`, `meter`, `object`, `output`, `progress`,
`select`, or `textarea`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "headers"
              , comment = """ A space separated list of element IDs indicating which `th` elements are
headers for this cell. For `td` and `th`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "height"
              , comment = """ Declare the height of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "hidden"
              , comment = """ Indicates the relevance of an element. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "href"
              , comment = """ The URL of a linked resource, such as `a`, `area`, `base`, or `link`. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "hreflang"
              , comment = """ Two-letter language code of the linked resource of an `a`, `area`, or `link`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "id"
              , comment = """ Often used with CSS to style a specific element. The value of this
attribute must be unique.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "ismap"
              , comment = """ When an `img` is a descendant of an `a` tag, the `ismap` attribute
indicates that the click location should be added to the parent `a`'s href as
a query string.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "itemprop"
              , comment = """"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "kind"
              , comment = """ Specifies the kind of text `track`. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "lang"
              , comment = """ Defines the language used in the element. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "list"
              , comment = """ Associates an `input` with a `datalist` tag. The datalist gives some
pre-defined options to suggest to the user as they interact with an input.
The value of the list attribute must match the id of a `datalist` node.
For `input`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "loop"
              , comment = """ Indicates whether the `audio` or `video` should start playing from the
start when it's finished.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "manifest"
              , comment = """ Specifies the URL of the cache manifest for an `html` tag. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "map"
              , comment = """ Transform the messages produced by an `Attribute`.
"""
              , tipe = decodeType "(a -> msg) -> Html.Attribute a -> Html.Attribute msg"
              }
            , { name = "max"
              , comment = """ Indicates the maximum value allowed. When using an input of type number or
date, the max value must be a number or date. For `input`, `meter`, and `progress`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "maxlength"
              , comment = """ Defines the maximum number of characters allowed in an `input` or
`textarea`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "media"
              , comment = """ Specifies a hint of the target media of a `a`, `area`, `link`, `source`,
or `style`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "method"
              , comment = """ Defines which HTTP method to use when submitting a `form`. Can be GET
(default) or POST.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "min"
              , comment = """ Indicates the minimum value allowed. When using an input of type number or
date, the min value must be a number or date. For `input` and `meter`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "minlength"
              , comment = """ Defines the minimum number of characters allowed in an `input` or
`textarea`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "multiple"
              , comment = """ Indicates whether multiple values can be entered in an `input` of type
email or file. Can also indicate that you can `select` many options.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "name"
              , comment = """ Name of the element. For example used by the server to identify the fields
in form submits. For `button`, `form`, `fieldset`, `iframe`, `input`,
`object`, `output`, `select`, `textarea`, `map`, `meta`, and `param`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "novalidate"
              , comment = """ This attribute indicates that a `form` shouldn't be validated when
submitted.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "pattern"
              , comment = """ Defines a regular expression which an `input`'s value will be validated
against.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "ping"
              , comment = """ Specify a URL to send a short POST request to when the user clicks on an
`a` or `area`. Useful for monitoring and tracking.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "placeholder"
              , comment = """ Provides a hint to the user of what can be entered into an `input` or
`textarea`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "poster"
              , comment = """ A URL indicating a poster frame to show until the user plays or seeks the
`video`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "preload"
              , comment = """ Control how much of an `audio` or `video` resource should be preloaded. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "property"
              , comment = """ Create *properties*, like saying `domNode.className = 'greeting'` in
JavaScript.

    import Json.Encode as Encode

    class : String -> Attribute msg
    class name =
      property "className" (Encode.string name)

Read more about the difference between properties and attributes [here][].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md
"""
              , tipe = decodeType "String.String -> Json.Encode.Value -> Html.Attribute msg"
              }
            , { name = "pubdate"
              , comment = """ Indicates whether this date and time is the date of the nearest `article`
ancestor element. For `time`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "readonly"
              , comment = """ Indicates whether an `input` or `textarea` can be edited. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "rel"
              , comment = """ Specifies the relationship of the target object to the link object.
For `a`, `area`, `link`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "required"
              , comment = """ Indicates whether this element is required to fill out or not.
For `input`, `select`, and `textarea`.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "reversed"
              , comment = """ Indicates whether an ordered list `ol` should be displayed in a descending
order instead of a ascending.
"""
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "rows"
              , comment = """ Defines the number of rows in a `textarea`. """
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "rowspan"
              , comment = """ Defines the number of rows a table cell should span over.
For `td` and `th`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "sandbox"
              , comment = """ A space separated list of security restrictions you'd like to lift for an
`iframe`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "scope"
              , comment = """ Specifies the scope of a header cell `th`. Possible values are: col, row,
colgroup, rowgroup.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "selected"
              , comment = """ Defines which `option` will be selected on page load. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "shape"
              , comment = """ Declare the shape of the clickable area in an `a` or `area`. Valid values
include: default, rect, circle, poly. This attribute can be paired with
`coords` to create more particular shapes.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "size"
              , comment = """ For `input` specifies the width of an input in characters.

For `select` specifies the number of visible options in a drop-down list.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "spellcheck"
              , comment = """ Indicates whether spell checking is allowed for the element. """
              , tipe = decodeType "Basics.Bool -> Html.Attribute msg"
              }
            , { name = "src"
              , comment = """ The URL of the embeddable content. For `audio`, `embed`, `iframe`, `img`,
`input`, `script`, `source`, `track`, and `video`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "srcdoc"
              , comment = """ An HTML document that will be displayed as the body of an `iframe`. It will
override the content of the `src` attribute if it has been specified.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "srclang"
              , comment = """ A two letter language code indicating the language of the `track` text data.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "start"
              , comment = """ Defines the first number of an ordered list if you want it to be something
besides 1.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "step"
              , comment = """ Add a step size to an `input`. Use `step "any"` to allow any floating-point
number to be used in the input.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "style"
              , comment = """ Specify a style.

    greeting : Node msg
    greeting =
      div
        [ style "background-color" "red"
        , style "height" "90px"
        , style "width" "100%"
        ]
        [ text "Hello!"
        ]

There is no `Html.Styles` module because best practices for working with HTML
suggest that this should primarily be specified in CSS files. So the general
recommendation is to use this function lightly.
"""
              , tipe = decodeType "String.String -> String.String -> Html.Attribute msg"
              }
            , { name = "tabindex"
              , comment = """ Overrides the browser's default tab order and follows the one specified
instead.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "target"
              , comment = """ Specify where the results of clicking an `a`, `area`, `base`, or `form`
should appear. Possible special values include:

  * _blank &mdash; a new window or tab
  * _self &mdash; the same frame (this is default)
  * _parent &mdash; the parent frame
  * _top &mdash; the full body of the window

You can also give the name of any `frame` you have created.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "title"
              , comment = """ Text to be displayed in a tooltip when hovering over the element. """
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "type_"
              , comment = """ Defines the type of a `button`, `input`, `embed`, `object`, `script`,
`source`, `style`, or `menu`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "usemap"
              , comment = """ Specify the hash name reference of a `map` that should be used for an `img`
or `object`. A hash name reference is a hash symbol followed by the element's name or id.
E.g. `"#planet-map"`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "value"
              , comment = """ Defines a default value which will be displayed in a `button`, `option`,
`input`, `li`, `meter`, `progress`, or `param`.
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            , { name = "width"
              , comment = """ Declare the width of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.
"""
              , tipe = decodeType "Basics.Int -> Html.Attribute msg"
              }
            , { name = "wrap"
              , comment = """ Indicates whether the text should be wrapped in a `textarea`. Possible
values are "hard" and "soft".
"""
              , tipe = decodeType "String.String -> Html.Attribute msg"
              }
            ]
      , binops = []
      }
    , { name = "Html.Events"
      , comment = """
It is often helpful to create an [Union Type][] so you can have many different kinds
of events as seen in the [TodoMVC][] example.

[Union Type]: https://elm-lang.org/learn/Union-Types.elm
[TodoMVC]: https://github.com/evancz/elm-todomvc/blob/master/Todo.elm

# Mouse
@docs onClick, onDoubleClick,
      onMouseDown, onMouseUp,
      onMouseEnter, onMouseLeave,
      onMouseOver, onMouseOut

# Forms
@docs onInput, onCheck, onSubmit

# Focus
@docs onBlur, onFocus

# Custom
@docs on, stopPropagationOn, preventDefaultOn, custom

## Custom Decoders
@docs targetValue, targetChecked, keyCode
"""
      , unions = []
      , aliases = []
      , values =
            [ { name = "custom"
              , comment = """ Create an event listener that may [`stopPropagation`][stop] or
[`preventDefault`][prevent].

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation
[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

**Note:** If you need something even more custom (like capture phase) check
out the lower-level event API in `elm/virtual-dom`.
"""
              , tipe = decodeType "String.String -> Json.Decode.Decoder { message : msg, stopPropagation : Basics.Bool, preventDefault : Basics.Bool } -> Html.Attribute msg"
              }
            , { name = "keyCode"
              , comment = """ A `Json.Decoder` for grabbing `event.keyCode`. This helps you define
keyboard listeners like this:

    import Json.Decode as Json

    onKeyUp : (Int -> msg) -> Attribute msg
    onKeyUp tagger =
      on "keyup" (Json.map tagger keyCode)

**Note:** It looks like the spec is moving away from `event.keyCode` and
towards `event.key`. Once this is supported in more browsers, we may add
helpers here for `onKeyUp`, `onKeyDown`, `onKeyPress`, etc.
"""
              , tipe = decodeType "Json.Decode.Decoder Basics.Int"
              }
            , { name = "on"
              , comment = """ Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Decode

    onClick : msg -> Attribute msg
    onClick message =
      on "click" (Decode.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.

The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.

If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really helps!

[aEL]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
[decoder]: /packages/elm/json/latest/Json-Decode
[tutorial]: https://github.com/evancz/elm-architecture-tutorial/

**Note:** This creates a [passive][] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
"""
              , tipe = decodeType "String.String -> Json.Decode.Decoder msg -> Html.Attribute msg"
              }
            , { name = "onBlur"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onCheck"
              , comment = """ Detect [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)
events on checkboxes. It will grab the boolean value from `event.target.checked`
on any input event.

Check out [`targetChecked`](#targetChecked) for more details on how this works.
"""
              , tipe = decodeType "(Basics.Bool -> msg) -> Html.Attribute msg"
              }
            , { name = "onClick"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onDoubleClick"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onFocus"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onInput"
              , comment = """ Detect [input](https://developer.mozilla.org/en-US/docs/Web/Events/input)
events for things like text fields or text areas.

For more details on how `onInput` works, check out [`targetValue`](#targetValue).

**Note 1:** It grabs the **string** value at `event.target.value`, so it will
not work if you need some other information. For example, if you want to track
inputs on a range slider, make a custom handler with [`on`](#on).

**Note 2:** It uses `stopPropagationOn` internally to always stop propagation
of the event. This is important for complicated reasons explained [here][1] and
[here][2].

[1]: /packages/elm/virtual-dom/latest/VirtualDom#Handler
[2]: https://github.com/elm/virtual-dom/issues/125
"""
              , tipe = decodeType "(String.String -> msg) -> Html.Attribute msg"
              }
            , { name = "onMouseDown"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onMouseEnter"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onMouseLeave"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onMouseOut"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onMouseOver"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onMouseUp"
              , comment = """"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "onSubmit"
              , comment = """ Detect a [submit](https://developer.mozilla.org/en-US/docs/Web/Events/submit)
event with [`preventDefault`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault)
in order to prevent the form from changing the page’s location. If you need
different behavior, create a custom event handler.
"""
              , tipe = decodeType "msg -> Html.Attribute msg"
              }
            , { name = "preventDefaultOn"
              , comment = """ Create an event listener that may [`preventDefault`][prevent]. Your decoder
must produce a message and a `Bool` that decides if `preventDefault` should
be called.

For example, the `onSubmit` function in this library *always* prevents the
default behavior:

[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

    onSubmit : msg -> Attribute msg
    onSubmit msg =
      preventDefaultOn "submit" (Json.map alwaysPreventDefault (Json.succeed msg))

    alwaysPreventDefault : msg -> ( msg, Bool )
    alwaysPreventDefault msg =
      ( msg, True )
"""
              , tipe = decodeType "String.String -> Json.Decode.Decoder ( msg, Basics.Bool ) -> Html.Attribute msg"
              }
            , { name = "stopPropagationOn"
              , comment = """ Create an event listener that may [`stopPropagation`][stop]. Your decoder
must produce a message and a `Bool` that decides if `stopPropagation` should
be called.

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation

**Note:** This creates a [passive][] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
"""
              , tipe = decodeType "String.String -> Json.Decode.Decoder ( msg, Basics.Bool ) -> Html.Attribute msg"
              }
            , { name = "targetChecked"
              , comment = """ A `Json.Decoder` for grabbing `event.target.checked`. We use this to define
`onCheck` as follows:

    import Json.Decode as Json

    onCheck : (Bool -> msg) -> Attribute msg
    onCheck tagger =
      on "input" (Json.map tagger targetChecked)
"""
              , tipe = decodeType "Json.Decode.Decoder Basics.Bool"
              }
            , { name = "targetValue"
              , comment = """ A `Json.Decoder` for grabbing `event.target.value`. We use this to define
`onInput` as follows:

    import Json.Decode as Json

    onInput : (String -> msg) -> Attribute msg
    onInput tagger =
      stopPropagationOn "input" <|
        Json.map alwaysStop (Json.map tagger targetValue)

    alwaysStop : a -> (a, Bool)
    alwaysStop x =
      (x, True)

You probably will never need this, but hopefully it gives some insights into
how to make custom event handlers.
"""
              , tipe = decodeType "Json.Decode.Decoder String.String"
              }
            ]
      , binops = []
      }
    , { name = "Html.Keyed"
      , comment = """ A keyed node helps optimize cases where children are getting added, moved,
removed, etc. Common examples include:

  - The user can delete items from a list.
  - The user can create new items in a list.
  - You can sort a list based on name or date or whatever.

When you use a keyed node, every child is paired with a string identifier. This
makes it possible for the underlying diffing algorithm to reuse nodes more
efficiently.

# Keyed Nodes
@docs node

# Commonly Keyed Nodes
@docs ol, ul
"""
      , unions = []
      , aliases = []
      , values =
            [ { name = "node"
              , comment = """ Works just like `Html.node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.
"""
              , tipe = decodeType "String.String -> List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
              }
            , { name = "ol"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
              }
            , { name = "ul"
              , comment = """"""
              , tipe = decodeType "List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
              }
            ]
      , binops = []
      }
    , { name = "Html.Lazy"
      , comment = """ Since all Elm functions are pure we have a guarantee that the same input
will always result in the same output. This module gives us tools to be lazy
about building `Html` that utilize this fact.

Rather than immediately applying functions to their arguments, the `lazy`
functions just bundle the function and arguments up for later. When diffing
the old and new virtual DOM, it checks to see if all the arguments are equal
by reference. If so, it skips calling the function!

This is a really cheap test and often makes things a lot faster, but definitely
benchmark to be sure!

@docs lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8

"""
      , unions = []
      , aliases = []
      , values =
            [ { name = "lazy"
              , comment = """ A performance optimization that delays the building of virtual DOM nodes.

Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
it. Calling `(lazy view model)` delays the call until later. During diffing, we
can check to see if `model` is referentially equal to the previous value used,
and if so, we just stop. No need to build up the tree structure and diff it,
we know if the input to `view` is the same, the output must be the same!
"""
              , tipe = decodeType "(a -> Html.Html msg) -> a -> Html.Html msg"
              }
            , { name = "lazy2"
              , comment = """ Same as `lazy` but checks on two arguments.
"""
              , tipe = decodeType "(a -> b -> Html.Html msg) -> a -> b -> Html.Html msg"
              }
            , { name = "lazy3"
              , comment = """ Same as `lazy` but checks on three arguments.
"""
              , tipe = decodeType "(a -> b -> c -> Html.Html msg) -> a -> b -> c -> Html.Html msg"
              }
            , { name = "lazy4"
              , comment = """ Same as `lazy` but checks on four arguments.
"""
              , tipe = decodeType "(a -> b -> c -> d -> Html.Html msg) -> a -> b -> c -> d -> Html.Html msg"
              }
            , { name = "lazy5"
              , comment = """ Same as `lazy` but checks on five arguments.
"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> Html.Html msg) -> a -> b -> c -> d -> e -> Html.Html msg"
              }
            , { name = "lazy6"
              , comment = """ Same as `lazy` but checks on six arguments.
"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> Html.Html msg"
              }
            , { name = "lazy7"
              , comment = """ Same as `lazy` but checks on seven arguments.
"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> g -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> g -> Html.Html msg"
              }
            , { name = "lazy8"
              , comment = """ Same as `lazy` but checks on eight arguments.
"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> g -> h -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Html.Html msg"
              }
            ]
      , binops = []
      }
    ]


decodeType : String -> Elm.Type.Type
decodeType type_ =
    case Decode.decodeString Elm.Type.decoder type_ of
        Ok resultType ->
            resultType

        Err _ ->
            Elm.Type.Var "unknown"
