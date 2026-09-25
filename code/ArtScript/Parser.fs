module Parser


open AST
open Combinator

let pad p = pbetween pws0 p pws0
let n = pmany1 pdigit |>> (fun digits -> stringify digits |> int)

let pvar = pmany1 pletter |>> stringify

let pintexpr, pintexprRef = recparser()

let pnum = n |>> Num
let pvar_expr = pvar |>> Var

let padd =
    pbetween
        (pad (pchar '('))
        (pseq (pad pintexpr) (pright (pad (pchar '+')) (pad pintexpr)) (fun (a, b) -> Add(a,b)))
        (pad (pchar ')'))

let psub =
    pbetween
        (pad (pchar '('))
        (pseq (pad pintexpr) (pright (pad (pchar '-')) (pad pintexpr)) (fun (a, b) -> Sub(a,b)))
        (pad (pchar ')'))

let pmul =
    pbetween
        (pad (pchar '('))
        (pseq (pad pintexpr) (pright (pad (pchar '*')) (pad pintexpr)) (fun (a, b) -> Mul(a,b)))
        (pad (pchar ')'))

let pdiv =
    pbetween
        (pad (pchar '('))
        (pseq (pad pintexpr) (pright (pad (pchar '/')) (pad pintexpr)) (fun (a, b) -> Div(a,b)))
        (pad (pchar ')'))

pintexprRef.Value <- pnum <|> pvar_expr <|> padd <|> psub <|> pmul <|> pdiv <!> "pintexpr"

let color =
    (pstr "none" |>> (fun _ -> NoColor)) <|>
    (pstr "red" |>> (fun _ -> Red)) <|>
    (pstr "green" |>> (fun _ -> Green)) <|>
    (pstr "blue" |>> (fun _ -> Blue)) <|>
    (pstr "purple" |>> (fun _ -> Purple)) <|>
    (pstr "black" |>> (fun _ -> Black)) <|>
    (pstr "yellow" |>> (fun _ -> Yellow)) <|>
    (pstr "gold" |>> (fun _ -> Gold)) <|>
    (pstr "white" |>> (fun _ -> White)) <|>
    (pstr "pink" |>> (fun _ -> Pink)) <|>
    (pstr "brown" |>> (fun _ -> Brown)) <|>
    (pstr "orange" |>> (fun _ -> Orange)) <|>
    ((pbetween
            (pad (pstr "RGB("))
            (pseq
                (pad n)
                (pseq (pad n) (pad n) (fun (y, z) -> y,z))
                (fun (x,(y,z)) -> (x,y,z)))
            (pad (pstr ")"))

            ) |>> (fun (a,b,c) -> RGB(a,b,c)))
let direction =
    (pstr "up" |>> (fun _ -> North)) <|>
    (pstr "down" |>> (fun _ -> South)) <|>
    (pstr "left" |>> (fun _ -> West)) <|>
    (pstr "right" |>> (fun _ -> East)) 
let pcoord = pseq (pad pintexpr) (pad pintexpr) (fun (x_loc,y_loc) -> x_loc,y_loc)

let pcommand = 
        (pright (pad (pstr "go")) 
                        (pseq (pad pintexpr) (pad color) (fun (len,col) -> len,col))
                       |>> (fun (l, c) -> Forward(l, c))) <|>
        (pad (pstr "toright")  |>> (fun _ -> TurnRight)) <|> 
        (pad (pstr "toleft") |>> (fun _ -> TurnLeft)) <|>
        (pright (pad (pstr "shift")) 
                        (pseq (pad pintexpr) (pad direction) (fun (len, dir) -> Shift(len,dir))))<|>
        (pright (pstr "setlocation") 
                        pcoord
                        |>> (fun (x, y) -> SetLocation(x,y))) <|>
        (pright (pstr "rect")
                        (pseq (pad pintexpr)  
                            (pseq (pad pintexpr)
                                    (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                                    (fun (l,(fill,color)) -> l, fill, color))
                                (fun (w, (l,fill,color)) -> w, l, fill, color))        
                        |>> (fun (w, l, fill, color) -> Rect(w, l, fill, color)))<|>
        (pright (pstr "circle")
                        (pseq (pad pintexpr)
                                (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                                (fun (l,(fill,color)) -> l, fill, color))                 
                        |>> (fun (r, fill, color) -> Circle(r, fill, color)))<|>
        (pright (pstr "poly")
                        (pseq
                            (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                            (pmany1 pcoord)
                            (fun ((fill, color), coords) -> fill, color, coords)
                            )
                        
                        |>> (fun (fill, color, coord_list) -> Polygon(fill, color, coord_list)) )<|>
        (pright (pad (pstr "set"))
                        (pseq (pad pvar) (pad pintexpr) (fun (v, e) -> v, e))
                        |>> (fun (v, e) -> Assign(v, e))) <|>
        (pad (pstr "penup") |>> (fun _ -> Penup)) <|>
        (pad (pstr "pendown") |>> (fun _ -> Pendown)) <!> "pcommand"

let expr, exprRef = recparser()

let repeat =
    pseq
        (pright (pad (pstr "repeat")) (pad n))
        (pbetween
            (pad (pchar '('))
            expr
            (pad (pchar ')'))
        )
        (fun (i, commands) ->
            [0..i-1] |> List.fold (fun acc _ -> commands @ acc) []
        ) <!> "repeat"

let forLoop =
    pseq (pright (pad (pstr "for")) (pad pvar))
         (pseq (pad pintexpr)
               (pseq (pad pintexpr)
                     (pbetween (pad (pchar '(')) expr (pad (pchar ')')))
                     (fun (e2, cmds) -> e2, cmds))
               (fun (e1, (e2, cmds)) -> e1, e2, cmds))
         (fun (v, (e1, e2, cmds)) -> ForLoop(v, e1, e2, cmds))
         |>> (fun cmd -> [cmd]) <!> "forLoop"

exprRef.Value <-
    pmany1 (
        (pcommand |>> (fun c -> [c])) <|>
        repeat <|>
        forLoop
    ) |>> List.concat

let grammar = pleft expr peof <!> "grammar"

let parse (input: string) : Drawing option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None
