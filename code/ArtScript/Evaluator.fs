module Evaluator

open AST

let evalColor (color: Color) : string =
    match color with
    | NoColor -> "none"
    | Red -> "rgb(255,0,0)"
    | Green -> "rgb(0,255,0)"
    | Blue -> "rgb(0,0,255)"
    | Purple -> "rgb(128,8,165)"
    | Black -> "rgb(0,0,0)"
    | Yellow -> "rgb(255,255,0)"
    | Gold -> "rgb(255,215,0)"
    | White -> "rgb(255,255,255)"
    | Orange -> "rgb(255,165,0)"
    | Pink -> "rgb(255,182,193)"
    | Brown -> "rgb(222,184,135)"
    | RGB(x,y,z) -> "rgb(" + (x|> string) + "," 
                                            + (y|> string) + "," 
                                            + (z|> string) + ")"

let rec evalExpr (expr: IntExpr) (env: Map<string, int>) : int =
    match expr with
    | Num n -> n
    | Var x -> 
        match Map.tryFind x env with
        | Some v -> v
        | None -> 0
    | Add(e1, e2) -> evalExpr e1 env + evalExpr e2 env
    | Sub(e1, e2) -> evalExpr e1 env - evalExpr e2 env
    | Mul(e1, e2) -> evalExpr e1 env * evalExpr e2 env
    | Div(e1, e2) -> 
        let denom = evalExpr e2 env
        if denom = 0 then 0 else evalExpr e1 env / denom

let rec polygoner (coordinates:(int*int) list) = 
                  match coordinates with
                  | [] -> ""
                  | (x, y)::xs -> (x |> string) + "," + (y|> string) + "  " + polygoner xs

let rec evalCommand (command: Command)(state:State): string * State =
    match command, state with
    | Assign(varName, expr), _ ->
        let value = evalExpr expr state.env
        "", { state with env = Map.add varName value state.env }
    | ForLoop(varName, startExpr, endExpr, commands), _ ->
        let startVal = evalExpr startExpr state.env
        let endVal = evalExpr endExpr state.env
        let mutable currState = state
        let mutable svgOutput = ""
        for i in startVal .. endVal do
            currState <- { currState with env = Map.add varName i currState.env }
            let loopOutput, nextState = evalDraw commands currState
            svgOutput <- svgOutput + loopOutput
            currState <- nextState
        svgOutput, currState
    | Forward(lenExpr, color), { position = start; direction = dir; pen_up = false}
            ->  let len = evalExpr lenExpr state.env
                let end_point =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}
                let line = "<line x1=\"" + ((start.x) |> string) + "\"" +
                                    " y1=\"" +        (start.y |> string) + "\"" +
                                    " x2=\"" +        (end_point.x |> string) + "\"" +
                                    " y2=\"" +        (end_point.y |> string) + "\"" +
                                    " style=\"stroke:" +
                                    (evalColor color) + ";stroke-width:2\" />\n"

                line, {state with position = end_point}
    | DrawText(txt, sizeExpr, color), { position = start; direction = _; pen_up = false}
           -> let size = evalExpr sizeExpr state.env
              let textSvg = "<text x=\"" +  ((start.x) |> string) + "\"" +
                                 " y=\"" +       (start.y |> string) + "\"" +
                                 " font-size=\"" + (size |> string) + "\"" +
                                 " fill=\"" +   (evalColor color) + "\"" +
                                 ">" + txt + "</text>\n"
              textSvg, state
    | Rect(wExpr, lExpr, fill, color), { position = start; direction = _; pen_up = false}
           -> let w = evalExpr wExpr state.env
              let l = evalExpr lExpr state.env
              let rect = "<rect x=\"" +  ((start.x) |> string) + "\"" +
                                 " y=\"" +       (start.y |> string) + "\"" +
                                 " width=\"" +   (w |> string) + "\"" +
                                 " height =\"" + (l |> string) + "\"" +
                                 " fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " stroke-width =\"2\" />\n"  
              rect, state
    | Circle(rExpr, fill, color), { position = start; direction = _; pen_up = false}
           -> let r = evalExpr rExpr state.env
              let circ = "<circle cx=\"" +  ((start.x) |> string) + "\"" +
                                 " cy=\"" +       (start.y |> string) + "\"" +
                                 " r=\"" +   (r |> string) + "\"" +
                                 " fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " stroke-width =\"2\" />\n"  
              circ, state
    | Ellipse(rxExpr, ryExpr, fill, color), { position = start; direction = _; pen_up = false}
           -> let rx = evalExpr rxExpr state.env
              let ry = evalExpr ryExpr state.env
              let elSvg = "<ellipse cx=\"" +  ((start.x) |> string) + "\"" +
                                 " cy=\"" +       (start.y |> string) + "\"" +
                                 " rx=\"" +   (rx |> string) + "\"" +
                                 " ry=\"" +   (ry |> string) + "\"" +
                                 " fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " stroke-width =\"2\" />\n"  
              elSvg, state
    | Polygon(fill, color, coords), { position = start; direction = _; pen_up = false}
           -> let evalCoords = coords |> List.map (fun (cx, cy) -> (evalExpr cx state.env, evalExpr cy state.env))
              let poly = "<polygon fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " points=\" " +
                                 (polygoner evalCoords) +
                                 "\"" +
                                 " stroke-width =\"2\" />\n"
              poly, state
    | Penup, { position = _; direction = _; pen_up = x}
            -> "", {state with pen_up = true}
    | Pendown, { position = _; direction = _; pen_up = x}
            -> "", {state with pen_up = false}
    | SetLocation(xExpr, yExpr), { position = _; direction = _; pen_up =_}
            -> let x_loc = evalExpr xExpr state.env
               let y_loc = evalExpr yExpr state.env
               "", {state with position = {x = x_loc; y = y_loc}}
    | Shift(lenExpr,dir), {position = start; direction = _; pen_up =_}
            ->  let len = evalExpr lenExpr state.env
                let new_position =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}

                "", {state with position = new_position}

    | Forward(lenExpr, _), { position = start; direction = dir; pen_up = true}
            ->  let len = evalExpr lenExpr state.env
                let end_point =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}

                "", {state with position = end_point}
    
    | TurnLeft, { position = _ ; direction = dir; pen_up = _}
            ->  let new_direction =
                    match dir with
                    | North -> West
                    | South -> East
                    | East -> North
                    | West -> South

                "", {state with direction = new_direction}
                    
    | TurnRight, { position = _ ; direction = dir; pen_up = _}
            ->  let new_direction =
                    match dir with
                    | North -> East
                    | South -> West
                    | East -> South
                    | West -> North

                "", {state with direction = new_direction}
    | _,_ -> "", state 

and evalDraw (drawing: Drawing)(state:State) =
    match drawing with
    | [] -> "", state
    | d::ds -> let line, curr_state = (evalCommand d state) 
               let next_drawing, next_state = (evalDraw ds curr_state)
               (line + next_drawing), next_state

let eval (drawing: Drawing) : string =
    let start_state = { position = origin; direction = North; pen_up = false; env = Map.empty }
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    ((evalDraw drawing start_state) |> (fun (x, y) -> x))
    + "</svg>\n"