#Exercise 03
Anders Wind Steffensen - awis@itu.dk

## Exercise 4.1
Heres the code I ran and tested.
// marks printed result

open ParseAndRun;;

fromString("5")
// val it : Absyn.expr = CstI 5
run (fromString("5"));;
// val it : int = 5

fromString("true");;
// val it : Absyn.expr = CstB true
run (fromString("true"));;
// val it : int = 1

fromString("x");;
// val it : Absyn.expr = Var "x"

fromString("let x = 5 in x end");;
// val it : Absyn.expr = Let ("x",CstI 5,Var "x")
run (fromString("let x = 5 in x end"));;
// val it : int = 5

fromString("5+5");;
// val it : Absyn.expr = Prim ("+",CstI 5,CstI 5)
fromString("5-5");;
// val it : Absyn.expr = Prim ("-",CstI 5,CstI 5)
fromString("5*5");;
val it : Absyn.expr = Prim ("*",CstI 5,CstI 5)

fromString("if false then 5 else 10");;
// val it : Absyn.expr = If (CstB false,CstI 5,CstI 10)
run (fromString("if false then 5 else 10"));;
//val it : int = 10


fromString("let f x = x*x in f 3 end");;
// val it : Absyn.expr = Letfun ("f","x",Prim ("*",Var "x",Var "x"),Call (Var "f",CstI 3))
run (fromString("let f x = x*x in f 3 end"));;
// val it : int = 9


## Exercise 4.2
1)
fromString("let sum n = if n=1 then n else n + sum (n-1) in sum 1000 end");;
// val it : Absyn.expr =
//   Letfun
//     ("sum","n",
//      If
//        (Prim ("=",Var "n",CstI 1),Var "n",
//         Prim ("+",Var "n",Call (Var "sum",Prim ("-",Var "n",CstI 1)))),
//      Call (Var "sum",CstI 1000))
run (fromString("let sum n = if n=1 then n else n + sum (n-1) in sum 1000 end"));;
// val it : int = 500500

2)
fromString("let power n = if n=0 then 1 else 3 * power (n-1) in power 8 end");;
// val it : Absyn.expr =
//   Letfun
//     ("power","n",
//      If
//        (Prim ("=",Var "n",CstI 0),CstI 1,
//         Prim ("*",CstI 3,Call (Var "power",Prim ("-",Var "n",CstI 1)))),
//      Call (Var "power",CstI 8))
run (fromString("let power n = if n=1 then 3 else 3 * power (n-1) in power 8 end"));;
// val it : int = 6561

3)
fromString("let power n = if n=0 then 1 else 3 * power (n-1) in let sumDaPower x = if x=0 then power x else power x + sumDaPower (x-1) in sumDaPower 11 end end");;
// val it : Absyn.expr =
//   Letfun
//     ("power","n",
//      If
//        (Prim ("=",Var "n",CstI 0),CstI 1,
//         Prim ("*",CstI 3,Call (Var "power",Prim ("-",Var "n",CstI 1)))),
//      Letfun
//        ("sumDaPower","x",
//         If
//           (Prim ("=",Var "x",CstI 0),Call (Var "power",Var "x"),
//            Prim
//              ("+",Call (Var "power",Var "x"),
//               Call (Var "sumDaPower",Prim ("-",Var "x",CstI 1)))),
//         Call (Var "sumDaPower",CstI 11)))
run (fromString("let power n = if n=0 then 1 else 3 * power (n-1) in let sumDaPower x = if x=0 then power x else power x + sumDaPower (x-1) in sumDaPower 11 end end"));;
// val it : int = 265720


4)
fromString("let power n = n*n*n*n*n*n*n*n in let sumDaPower x = if x=1 then power x else power x + sumDaPower (x-1) in sumDaPower 10 end end");;
// val it : Absyn.expr =
//   Letfun
//     ("power","n",
//      Prim
//        ("*",
//         Prim
//           ("*",
//            Prim
//              ("*",
//               Prim
//                 ("*",
//                  Prim
//                    ("*",Prim ("*",Prim ("*",Var "n",Var "n"),Var "n"),Var "n"),
//                  Var "n"),Var "n"),Var "n"),Var "n"),
//      Letfun
//        ("sumDaPower","x",
//         If
//           (Prim ("=",Var "x",CstI 1),Call (Var "power",Var "x"),
//            Prim
//              ("+",Call (Var "power",Var "x"),
//               Call (Var "sumDaPower",Prim ("-",Var "x",CstI 1)))),
//         Call (Var "sumDaPower",CstI 10)))
> run (fromString("let power n = n*n*n*n*n*n*n*n in let sumDaPower x = if x=1 then power x else power x + sumDaPower (x-1) in sumDaPower 10 end end"));;
// val it : int = 167731333

## Exercise 4.3

## Exercise 4.4