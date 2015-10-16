module Re

open Absyn

let rec eval (e : re) : string = 
    match e with
    | _ -> "lol"