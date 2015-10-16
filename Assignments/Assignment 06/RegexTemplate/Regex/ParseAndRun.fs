module ParseAndRun

let fromString s = Parse.fromString s;;

let eval (re:Absyn.re) = Re.eval re;;

let run e = eval e;;