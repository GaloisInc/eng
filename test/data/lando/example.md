<!-- BEGIN System The foo system -->
## <a id="FOO">System: The foo system (FOO)</a>
This is a mandatory description. It is a PARAGRAPH, and so it can consist of several sentences and be spread over several lines. It must be terminated by blank line.

<!-- BEGIN Index Entry -->
- Indexed **key**:
  - an item for this key
  - another item for this key (separated by the blank line)
<!-- END Index Entry -->
<!-- BEGIN Index Entry -->
- Indexed **url**: http://notice.the.internal.colon.html
<!-- END Index Entry -->
<!-- BEGIN Index Entry -->
- Indexed **misc**:
  - but words here must not end with a colon
  - and this:might not parse as you might expect
<!-- END Index Entry -->

<!-- BEGIN Subsystem The Bar subsystem -->
### <a id="BAR">Subsystem: The Bar subsystem (BAR)</a>
- Qualified: *[The foo system](#FOO)**:**[The Bar subsystem](#BAR)*

Again, a mandatory description PARAGRAPH.


<!-- BEGIN Component AAAA -->
#### <a id="A4">Component: AAAA (A4)</a>
- Qualified: *[The foo system](#FOO)**:**[The Bar subsystem](#BAR)**:**[AAAA](#A4)*

And yet another mandatory description PARAGRAPH.

- Inherits [BB](#BB)
- Client of [DD](#DD)
- Client of [FF](#FF)
<!-- END Component AAAA -->

<!-- BEGIN Component B2 -->
#### <a id="BB">Component: B2 (BB)</a>
- Qualified: *[The foo system](#FOO)**:**[The Bar subsystem](#BAR)**:**[B2](#BB)*

Description here.

- Inherits [A4](#A4)
<!-- BEGIN Constraint -->
- **Constraint**: constraint.
<!-- END Constraint -->
<!-- BEGIN Query -->
- **Query**: query?
<!-- END Query -->
<!-- BEGIN Command -->
- **Command**: command!
<!-- END Command -->
<!-- END Component B2 -->

<!-- BEGIN Component DD -->
#### <a id="DD">Component: DD</a>
- Qualified: *[The foo system](#FOO)**:**[The Bar subsystem](#BAR)**:**[DD](#DD)*

Description.

- Inherits [SS](#SS)**:**[Baz](#Baz)
<!-- END Component DD -->

<!-- BEGIN Relation [DD](#DD) -->
#### Relation: [DD](#DD)
- Client of [B2](#BB)
<!-- END Relation [DD](#DD) -->

<!-- BEGIN Relation [DD](#DD) -->
#### Relation: [DD](#DD)
- Inherits [B2](#BB)
<!-- END Relation [DD](#DD) -->

<!-- BEGIN Imported Component [FF](#FF) -->
#### Imported Component: [FF](#FF)
- Client of [DD](#DD)
<!-- END Imported Component [FF](#FF) -->

<!-- BEGIN Imported Component [FF](#FF) -->
#### Imported Component: [FF](#FF)
<!-- END Imported Component [FF](#FF) -->

<!-- BEGIN Imported Component [FF](#FF) -->
#### Imported Component: [FF](#FF) (myFF)
<!-- END Imported Component [FF](#FF) -->

<!-- BEGIN Imported Component [SS](#SS)**:**[Baz](#Baz) -->
#### Imported Component: [SS](#SS)**:**[Baz](#Baz)
<!-- END Imported Component [SS](#SS)**:**[Baz](#Baz) -->

<!-- BEGIN Imported Component [SS](#SS)**:**[Baz](#Baz) -->
#### Imported Component: [SS](#SS)**:**[Baz](#Baz) (myBaz)
<!-- END Imported Component [SS](#SS)**:**[Baz](#Baz) -->
<!-- END Subsystem The Bar subsystem -->
<!-- END System The foo system -->

<!-- BEGIN Component FF -->
## <a id="FF">Component: FF</a>
This is a top-level component, which can be referenced from anywhere, including from imports.

<!-- END Component FF -->

<!-- BEGIN Subsystem SS -->
## <a id="SS">Subsystem: SS</a>
This is a top-level subsystem, which can be referenced from anywhere, including from imports.


<!-- BEGIN Component Baz -->
### <a id="Baz">Component: Baz</a>
- Qualified: *[SS](#SS)**:**[Baz](#Baz)*

Description of Baz.

<!-- END Component Baz -->
<!-- END Subsystem SS -->

<!-- BEGIN Subsystem UU -->
## <a id="UU">Subsystem: UU</a>
Description.


<!-- BEGIN Component A -->
### <a id="A">Component: A</a>
- Qualified: *[UU](#UU)**:**[A](#A)*

Inside this description, I will talk about "FF" and "SS:Baz" (or "SS : Baz") and "The foo system" but also "utter nonsense".

<!-- END Component A -->

<!-- BEGIN Events myevents -->
### <a id="myevents">Events: myevents</a>
- Qualified: *[UU](#UU)**:**[myevents](#myevents)*

<!-- BEGIN Item -->
- **one** the first event.
<!-- END Item -->
<!-- BEGIN Item -->
- **two** the second event.
<!-- END Item -->
<!-- END Events myevents -->

<!-- BEGIN Scenarios myscenarios -->
### <a id="myscenarios">Scenarios: myscenarios</a>
- Qualified: *[UU](#UU)**:**[myscenarios](#myscenarios)*

<!-- BEGIN Item -->
- **a** "one" comes before "two".
<!-- END Item -->
<!-- BEGIN Item -->
- **b** "two" comes before "three".
<!-- END Item -->
<!-- END Scenarios myscenarios -->
<!-- END Subsystem UU -->
