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

//    let rec closedin (e : expr) (vs : string list) : bool =
//        match e with
//        | CstI i -> true
//        | Var x -> List.exists (fun y -> x=y) vs
//        | Let(x, erhs, ebody) -> let vs1 = x :: vs
//                                 closedin erhs vs && closedin ebody vs1
//        | Prim(ope, e1, e2) -> closedin e1 vs && closedin e2 vs

    let rec 
    let rec eval e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x
        | Let(list, ebody) -> 
            let inner (x,ehrs:expr) env1 =
                let xval = eval ehrs env
                let env2 = (x, xval) :: env1
                eval ebody env2
            let rec iterateList list acc : int = 
                match list with
                | [] -> acc
                | x::xs -> (iterateList xs ((inner x env)+acc))
            iterateList list 0
        | Prim("+", e1, e2) -> eval e1 env + eval e2 env
        | Prim("*", e1, e2) -> eval e1 env * eval e2 env
        | Prim("-", e1, e2) -> eval e1 env - eval e2 env
        | Prim _            -> failwith "unknown primitive";;

    let run e = eval e [];;
(*
Exercise 2.2
*)


(*
Exercise 2.3
*)


(*
Exercise 2.6
*)