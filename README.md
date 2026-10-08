# ArtScript
*A specialized programming language for creating geometrically intricate, SVG-based artwork.*

Built by Andrew Ansah and Henok Misgina Fisseha for CSCI 334, Williams College.

<p align="center">
  <img src="docs/image.png" alt="Josef Albers Artwork Recreation" />
  <br />
  <em>A recreation of a Josef Albers artwork found at the Williams College Museum of Art (WCMA) using ArtScript.</em>
</p>

```text
setlocation 10 10
rect 200 200 yellow yellow
setlocation 50 50
rect 100 100 gold gold
setlocation 70 70
rect 50 50 none yellow
```

## Problem and Approach
ArtScript was designed to blend the elegance of mathematical shapes with the creative expression of visual art. Inspired by the minimalist, geometric artwork at the Williams College Museum of Art, we wanted to create an accessible platform for beginners, kids, and aspiring artists to explore the fundamentals of coding while unleashing their creativity. 

Our approach was to build a custom Domain-Specific Language (DSL) that simulates a pen drawing with mathematical precision. Users interact with a concise set of core commands (like `go`, `setlocation`, `toright`) and powerful combining forms (like the `repeat` block, bounded loops, and grouped shapes) to generate complex, generative patterns with minimal code.

## Tech Stack
* **Language:** F#
* **Framework:** .NET
* **Output Format:** SVG (Scalable Vector Graphics)
* **Architecture:** Custom Parser and AST Evaluator utilizing functional state management.

## Setup Steps
To run an ArtScript program, ensure you have the .NET SDK installed. Navigate to the project directory and use `dotnet run`, passing the text file containing your ArtScript code as an argument.

```bash
cd code/ArtScript
dotnet run examples/rect.txt > rect.svg
```
This evaluates the commands in the text file and outputs the generated image in SVG format. There are various example inputs provided in the `code/ArtScript/examples` folder.

## Notes and Trade-offs

### Formal Syntax & Semantics
Our primary primitives are commands executed in order. The main combining form is an expression (`expr`), which is a list of commands including the repeat function. 

```text
<expr> ::= <command> <expr> | <command> | <expr> <repeat> <expr> | <empty>
<repeat> ::= repeat <n> ( <expr> )
<direction> ::= up | down | left | right
<color> ::= red | green | blue | purple | black | yellow | gold | white | pink | brown | orange | RGB( <n> <n> <n>) | none
<num_pair> ::= <n> <n>
<command> ::= <command> <expr> | <command> | <empty>
            | go <n> <color> | setlocation <n> <n> | toright | toleft
            | penup | pendown | shift <n> <direction>
            | rect <n> <n> <color> <color> | circle <n> <color> <color>
            | poly <color> <color> <num_pair>*
```

### Extended Features vs. Scope
The initial scope of the class project focused purely on static turtle-graphics commands. Post-submission, the language was independently extended to include dynamic features like variables (`set`), loops (`for`), grouped transformations (`rotate`, `scale`), and development helpers (`grid`). A trade-off of this functional expansion was migrating the purely static coordinate state into a dynamic environment map capable of scoping variables during AST evaluation.

**Extended Formal Syntax:**
With the introduction of dynamic state, static numbers (`<n>`) in all base commands were upgraded to support fully evaluated arithmetic expressions (`<num_expr>`).

```text
<num_expr> ::= <n> | <var> 
             | ( <num_expr> + <num_expr> )
             | ( <num_expr> - <num_expr> )
             | ( <num_expr> * <num_expr> )
             | ( <num_expr> / <num_expr> )
<var> ::= (any valid string identifier)
<string> ::= "(any text)"

<extended_command> ::= set <var> <num_expr>
                     | for <var> <num_expr> <num_expr> ( <expr> )
                     | rotate <num_expr> ( <expr> )
                     | scale <num_expr> <num_expr> ( <expr> )
                     | grid <num_expr>
                     | text <string> <num_expr> <color>
                     | ellipse <num_expr> <num_expr> <color> <color>
```

*For a detailed breakdown of the development journey, architecture, and these technical trade-offs, see the [Case Study](CASE_STUDY.md).*

---

### More Examples Showcase


**Bunny**
![Bunny](docs/bunny.png)
```text
setlocation 270 300
rect 300 550 pink black

setlocation 300 570
rect 240 100 brown black

setlocation 350 400
circle 50 white black
circle 10 black black

setlocation 480 400
circle 50 white black
circle 10 black black

poly brown black
     415 550
     380 500
     450 500

setlocation 415 670
toright toright
go 70 black
toright
go 70 black
toright toright
go 140 black

poly none black
     485 740
     530 700

poly none black
     345 740
     300 700

setlocation 415 740
rect 50 70 white black
setlocation 365 740
rect 50 70 white black

poly pink black
     270 100
     150 100
     270 500

poly pink black
     570 100
     690 100
     570 500

setlocation 500 260
rect 40 40 gold black

poly purple black
     500 260
     500 300
     400 350
     400 210

poly purple black
     540 260
     540 300
     640 350
     640 210

poly none black 300 600  180 570
poly none black 300 620  180 620
poly none black 300 640  180 670
poly none black 540 600  660 570
poly none black 540 620  660 620
poly none black 540 640  660 670

setlocation 415 610
circle 5 black black
setlocation 370 600
circle 5 black black
setlocation 460 600
circle 5 black black
setlocation 390 640
circle 5 black black
setlocation 440 640
circle 5 black black

setlocation 415 550
toright
go 20 black 

poly orange black
     740 690
     830 710
     730 920

poly green black
     770 700
     760 650
     780 680
     800 650
     800 680
     840 660
     800 710

poly none black
     740 730
     800 740
poly none black
     760 760
     800 770
poly none black
     735 790
     770 800
poly none black
     750 830
     770 838
```

**Repeating Lines Pattern**
![Repeating Pattern](docs/repeating.png)
```text
setlocation 20 380

repeat 9 (

go 360 blue
toright
go 10 green
toright
go 360 purple
toleft
go 10 red
toleft

go 360 green
toright
go 10 blue
toright
go 360 red
toleft
go 10 purple
toleft )
```

**Variables and Loops**
![Loop Test](docs/loop_test.svg)
```text
setlocation 500 500
set length 10
for i 1 20 (
  go length red
  toright
  set length (length + 10)
)
```

**Text Support**
![Text Test](docs/text_test.svg)
```text
setlocation 100 100
text "Hello ArtScript!" 40 blue
```

**Coordinate Helpers (Grid)**
![Grid Test](docs/grid_test.svg)
```text
grid 100
setlocation 100 100
circle 20 red black
```

**Transformations**
![Transform Test](docs/transform_test.svg)
```text
rotate 45 (
  setlocation 100 100
  rect 50 50 blue red
)
scale 2 2 (
  setlocation 50 50
  circle 10 green black
)
```
