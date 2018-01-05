
module type ZEXPR = 
sig 
  exception Error of string 
  type id = string 
  type expr = 
    | NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr 
    | MULT of expr * expr 
    | DIVIDE of expr * expr 
    | MAX of expr list 
    | VAR of id 
    | LET of id * expr * expr 

  type environment 
  type value 

  val emptyEnv : environment 
  val eval : environment * expr -> value 

  val print_value : value -> unit 
end 

module Zexpr : ZEXPR = 
struct 
    exception Error of string
    type id = string 
    type expr = 
        | NUM of int 
        | PLUS of expr * expr 
        | MINUS of expr * expr 
        | MULT of expr * expr 
        | DIVIDE of expr * expr 
        | MAX of expr list 
        | VAR of id 
        | LET of id * expr * expr 
   
    type environment = (string * int) list
    type value = int

    let emptyEnv : environment = []
    let rec eval : environment * expr -> value = fun(env, e) ->
        let getMax: environment * (expr list) -> value = fun(env, l) ->
            let rec getMaxInternal: value * environment * (expr list) -> value =
                fun(cur, env, l) ->
                    if l == [] then cur
                    else if cur < eval(env, List.hd(l)) then getMaxInternal(eval(env,
                            List.hd(l)), env, List.tl(l))
                    else getMaxInternal(cur, env, List.tl(l))
            in

            if l == [] then 0
            else getMaxInternal(eval(env, List.hd(l)), env, List.tl(l))         
        in

        let rec getVar: environment * string -> value = fun(env, name) ->
            if env == [] then raise (Error "FreeVariable")
            else if (String.compare name (fst(List.hd(env)))) == 0 then snd(List.hd(env))
            else getVar(List.tl(env), name)
        in

        match e with
        | NUM(i) -> i
        | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
        | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
        | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
        | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
        | MAX(l) -> getMax(env, l)
        | VAR(n) -> getVar(env, n)
        | LET(n, v, ex) -> eval((n, eval(env, v))::env, ex)

    let print_value : value -> unit = fun(v) ->
        print_endline(string_of_int(v))
(*
    let _ = print_value (eval (emptyEnv, NUM 1))
*)

end 


(*
open Zexpr
let _ = print_value(eval(emptyEnv, (NUM 1))) 
*)

(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1))) 

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) 
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x"))))
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1)) 

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX []))) 

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)]))) 

*)

(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR
                   "x", Zexpr.VAR "x")),
                   Zexpr.VAR "x")
)))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2 , Zexpr.PLUS(Zexpr.VAR "x",
                   Zexpr.VAR "y")),
                                            Zexpr.VAR "x")
)))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR
                   "y", Zexpr.VAR "x")),
                                            Zexpr.VAR "y")
)))
*)

(*
let _ = print_value (eval (emptyEnv, NUM 1)) 
*)
