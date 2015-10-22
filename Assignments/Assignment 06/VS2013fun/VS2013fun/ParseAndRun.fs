(* File Fun/ParseAndRun.fs *)

module ParseAndRun

let fromString s = Parse.fromString s;;

let eval (expr:Absyn.expr) e = Fun.eval expr e;;

let run e = eval e [];;
