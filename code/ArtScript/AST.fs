module AST


type Coordinate = { x: int; y: int }

type Color =
| NoColor
| Red
| Green
| Blue
| Purple
| Black
| Yellow
| Gold
| White
| Orange
| Pink
| Brown
| RGB of int*int*int

type Direction =
| North
| South
| East
| West


type IntExpr =
| Num of int
| Var of string
| Add of IntExpr * IntExpr
| Sub of IntExpr * IntExpr
| Mul of IntExpr * IntExpr
| Div of IntExpr * IntExpr

type Command =
| Forward of IntExpr * Color
| SetLocation of IntExpr * IntExpr
| TurnRight
| TurnLeft
| Shift of IntExpr * Direction
| Penup
| Pendown
| Rect of IntExpr * IntExpr * Color * Color
| Circle of IntExpr * Color * Color
| Polygon of Color * Color * (IntExpr * IntExpr) list
| Assign of string * IntExpr
| ForLoop of string * IntExpr * IntExpr * Command list
| DrawText of string * IntExpr * Color

type Drawing = Command list

type State = { position: Coordinate; direction: Direction; pen_up: bool; env: Map<string, int> }


let CANVAS_SZ = 1000
let origin = { x = CANVAS_SZ/2; y = CANVAS_SZ/2}

