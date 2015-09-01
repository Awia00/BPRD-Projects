module Exercise01

(*
Exercise 1.1
*)
module ex1_1 =
    type expr = 
      | CstI of int
      | Var of string
      | Prim of string * expr * expr
      | If of expr * expr * expr // added in subexercise (iv)
    
    let rec lookup env x =
        match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x=y then v else lookup r x
    
    let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)]
    let emptyenv = []; (* the empty environment *)
    
    let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a")
    
    //(i)
    let rec eval e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x 
        | Prim("+", e1, e2) -> eval e1 env + eval e2 env
        | Prim("*", e1, e2) -> eval e1 env * eval e2 env
        | Prim("-", e1, e2) -> eval e1 env - eval e2 env
        | Prim("Min", e1, e2) -> let x = eval e1 env
                                 let y = eval e2 env
                                 if x > y then y else x 
        | Prim("Max", e1, e2) -> let x = eval e1 env
                                 let y = eval e2 env
                                 if x < y then y else x 
        | Prim("==", e1, e2) ->  let x = eval e1 env
                                 let y = eval e2 env
                                 if x = y then 1 else 0 
        | Prim _            -> failwith "unknown primitive"
    
    //(ii)
    let testEqualsTrue  = eval (Prim("==",CstI 5,CstI 5)) env
    let testEqualsFalse = eval (Prim("==",CstI 5,CstI 6)) env
    let testMinFirst    = eval (Prim("Min",CstI 5,CstI 6)) env
    let testMinLast     = eval (Prim("Min",CstI 6,CstI 5)) env
    let testMaxFirst    = eval (Prim("Max",CstI 6,CstI 5)) env
    let testMaxLast     = eval (Prim("Max",CstI 5,CstI 6)) env
    
    //(iii)
    let rec eval2 e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x 
        | Prim(ope, e1,e2)  -> 
            let i1 = eval2 e1 env
            let i2 = eval2 e2 env
            match ope with 
            | "+"   -> i1 + i2
            | "*"   -> i1 * i2
            | "-"   -> i1 - i2
            | "Min" -> if i1 > i2 then i1 else i2 
            | "Max" -> if i1 < i2 then i1 else i2 
            | "=="  -> if i1 = i2 then 1 else 0 
            | _     -> failwith "unknown primitive"

    // (iv)
    // the last type is added in the top of the exercise

    // (v)
    let rec eval3 e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x 
        | Prim(ope, e1,e2)  -> 
            let i1 = eval3 e1 env
            let i2 = eval3 e2 env
            match ope with 
            | "+"   -> i1 + i2
            | "*"   -> i1 * i2
            | "-"   -> i1 - i2
            | "Min" -> if i1 > i2 then i1 else i2 
            | "Max" -> if i1 < i2 then i1 else i2 
            | "=="  -> if i1 = i2 then 1 else 0 
            | _     -> failwith "unknown primitive"
        | If (e1, e2, e3) -> if (eval3 e1 env)=0 then eval3 e2 env else eval3 e3 env

    let test = If(Var "a", CstI 11, CstI 22) // this returns 22 when the environment env is used

(*
Exercise 1.2
*)
//(i)
module ex1_2 =
    type aexpr = 
        | CstI of int
        | Var of string
        | Add of aexpr * aexpr
        | Mul of aexpr * aexpr
        | Sub of aexpr * aexpr
    
    
    //(ii)
    let exampleAexpr01 = Sub (Var "v", Add (Var "w", Var "z"))
    let exampleAexpr02 = Mul (CstI 2, exampleAexpr01)
    
    //(iii)
    let rec fmt aexpr : string = 
        match aexpr with
        | CstI i        -> i.ToString()
        | Var s         -> s
        | Add (e1,e2)   -> "(" + fmt e1 + " + " + fmt e2 + ")"
        | Mul (e1,e2)   -> "(" + fmt e1 + " * " + fmt e2 + ")"
        | Sub (e1,e2)   -> "(" + fmt e1 + " - " + fmt e2 + ")"

    //(iv)
    let rec simplify aexpr : aexpr = 
        match aexpr with
        | Sub (e,CstI 0)    -> e
        | Sub (e1, e2) when e1=e2 -> CstI 0
        | Add (e,CstI 0)    -> e
        | Add (CstI 0,e)    -> e
        | Mul (e, CstI 1)   -> e
        | Mul (CstI 1, e)   -> e
        | Mul (e, CstI 0)   -> CstI 0
        | Mul (CstI 0, e)   -> CstI 0
        | Mul (Add(CstI 1, CstI 0), Add(e, CstI 0)) -> e
        | CstI i        -> aexpr
        | Var s         -> aexpr
        | Add (e1,e2)   -> aexpr
        | Mul (e1,e2)   -> aexpr
        | Sub (e1,e2)   -> aexpr

    //(v)

(*
Exercise 1.4
*)
// See .cs file

(*
Exercise 2.1
*)
module ex2_1 =
    type expr = 
      | CstI of int
      | Var of string
      | Let of (string * expr) list * expr
      | Prim of string * expr * expr

    let rec lookup env x =
        match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x=y then v else lookup r x

    let e1 = Let(["z", CstI 17], Prim("+", Var "z", Var "z"))
    
    let e2 = Let(["z", CstI 17], 
                 Prim("+", Let(["z", CstI 22], Prim("*", CstI 100, Var "z")),
                           Var "z"))
    
    let e3 = Let(["z", Prim("-", CstI 5, CstI 4)], 
                 Prim("*", CstI 100, Var "z"))
    
    let e4 = Prim("+", Prim("+", CstI 20, Let(["z", CstI 17], Prim("+", Var "z", CstI 2))), CstI 30)
    
    let e5 = Prim("*", CstI 2, Let(["x", CstI 3], Prim("+", Var "x", CstI 4)))
    
    let e6 = Let(["z", Var "x"], Prim("+", Var "z", Var "x"))
    let e7 = Let(["z", CstI 3], Let(["y", Prim("+", Var "z", CstI 1)], Prim("+", Var "z", Var "y")))
    let e8 = Let(["z", Let(["x", CstI 4], Prim("+", Var "x", CstI 5))], Prim("*", Var "z", CstI 2))
    let e9 = Let(["z", CstI 3], Let(["y", Prim("+", Var "z", CstI 1)], Prim("+", Var "x", Var "y")))
    let e10 = Let(["z", Prim("+", Let(["x", CstI 4], Prim("+", Var "x", CstI 5)), Var "x")], Prim("*", Var "z", CstI 2))
    let e11 = Let(["z", CstI 17;"x",CstI 3], Prim("+", Var "z", Var "x"))//Let(["x", CstI 17;"z",Prim("+", CstI 3, CstI 3)], Prim("+", Var "x", Var "x"))
    let e12 = Prim("+", Var "x", Var "z")

    let rec eval e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x
        | Let(list, ebody) ->  // only this match is changed
            let rec buildEnv list accEnv =
                match list with
                | []    -> eval ebody accEnv
                | (x,ehrs)::xs -> let xval = eval ehrs accEnv
                                  let env1 = (x, xval) :: accEnv
                                  buildEnv xs env1
            buildEnv list env
        | Prim("+", e1, e2) -> eval e1 env + eval e2 env
        | Prim("*", e1, e2) -> eval e1 env * eval e2 env
        | Prim("-", e1, e2) -> eval e1 env - eval e2 env
        | Prim _            -> failwith "unknown primitive"

    let run e = eval e []
    let res = List.map run [e1;e2;e3;e4;e5;e7;e8;e11]

(*
Exercise 2.2
*)
    let rec mem x vs = 
        match vs with
        | []      -> false
        | v :: vr -> x=v || mem x vr

    let rec union (xs, ys) = 
        match xs with 
        | []    -> ys
        | x::xr -> if mem x ys then union(xr, ys)
                   else x :: union(xr, ys)
    let rec minus (xs, ys) = 
        match xs with 
        | []    -> []
        | x::xr -> if mem x ys then minus(xr, ys)
                   else x :: minus (xr, ys)

    let rec freevars e : string list =
        match e with
        | CstI i -> []
        | Var x  -> [x]
        | Let(list, ebody) -> // only this match is changed
            let rec lets letsList accList =
                match letsList with
                | [] -> accList
                | (x,ehrs)::xs -> lets xs ((freevars ehrs)@accList)
            union ((lets list []), minus (freevars ebody, List.map (fun (x,y) -> x) list))
        | Prim(ope, e1, e2) -> union (freevars e1, freevars e2)
    
    let closed2 e = (freevars e = [])
    let test = List.map closed2 [e1;e2;e3;e4;e5;e6;e7;e8;e9;e10;e11;e12]
(*
Exercise 2.3
*)
    type texpr =                            (* target expressions *)
      | TCstI of int
      | TVar of int                         (* index into runtime environment *)
      | TLet of texpr * texpr               (* erhs and ebody                 *)
      | TPrim of string * texpr * texpr

    let rec getindex vs x = 
        match vs with 
        | []    -> failwith "Variable not found"
        | y::yr -> if x=y then 0 else 1 + getindex yr x

    let rec teval (e : texpr) (renv : int list) : int =
        match e with
        | TCstI i -> i
        | TVar n  -> List.nth renv n
        | TLet(erhs, ebody) -> 
            let xval = teval erhs renv
            let renv1 = xval :: renv 
            teval ebody renv1 
        | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
        | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
        | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
        | TPrim _            -> failwith "unknown primitive"

    let rec tcomp (e : expr) (cenv : string list) : texpr =
        match e with
        | CstI i -> TCstI i
        | Var x  -> TVar (getindex cenv x)
        | Let(list, ebody) ->  // Only this match is changed
            let cenv1 = (List.map (fun (x,y) -> x) list) @ cenv
            let convertedList = List.map (fun (x,erhs) -> tcomp erhs cenv) list
            List.foldBack (fun x y -> TLet (x,y)) convertedList (tcomp ebody cenv1)
        | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv)

(*
Exercise 2.6
*)
    let simE = Let(["x",CstI 11], Let(["x", CstI 22; "y", Prim("+",Var "x", CstI 1)], Prim("+",Var "x", Var "y")))
    
    let rec evalSim e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x
        | Let(list, ebody) ->  // only this match is changed
            let rec buildEnv list accEnv =
                match list with
                | []    -> evalSim ebody accEnv
                | (x,ehrs)::xs -> let xval = evalSim ehrs accEnv
                                  let env1 = (x, xval) :: accEnv
                                  buildEnv xs env1
            buildEnv list env
        | Prim("+", e1, e2) -> evalSim e1 env + evalSim e2 env
        | Prim("*", e1, e2) -> evalSim e1 env * evalSim e2 env
        | Prim("-", e1, e2) -> evalSim e1 env - evalSim e2 env
        | Prim _            -> failwith "unknown primitive"

    let testSim = evalSim simE [] // should return 22