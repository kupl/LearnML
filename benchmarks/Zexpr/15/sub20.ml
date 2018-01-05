(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-7 *)

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

  type environment = (id * int) list
  type value = string

  let emptyEnv : environment = []
  let eval : environment * expr -> value = fun(env, e) ->

    let rec find : environment * string -> int option = fun(senv, k) ->
      match senv with
      | [] -> None
      | (s, i)::tl -> if(s = k) then Some(i) else find(tl, k)
    in

    let rec evalInt : environment * expr -> int = fun(senv, se) ->
      match se with
      | NUM i -> i
      | PLUS(x, y) -> evalInt(senv, x) + evalInt(senv, y)
      | MINUS(x, y) -> evalInt(senv, x) - evalInt(senv, y)
      | MULT(x, y) -> evalInt(senv, x) * evalInt(senv, y)
      | DIVIDE(x, y) -> 
         let y = evalInt(senv, y) in
          if(y = 0) then raise (Error("Divided by Zero"))
          else evalInt(senv, x) / y
      | MAX l -> 
        (match l with
        | [] -> 0
        | hd::[] -> evalInt(senv, hd)
        | hd::tl -> if(evalInt(senv, hd) > evalInt(senv, MAX(tl))) then evalInt(senv, MAX(tl)) else evalInt(senv, hd))
      | VAR v ->
(
        match find(senv, v) with
        | Some x -> x
        | _ -> raise (Error("FreeVariable"))
)
      | LET(v, e1, e2)  -> evalInt((v, evalInt(senv, e1))::senv, e2)
    in

    string_of_int(evalInt(env, e))

  let print_value : value -> unit = fun(v) -> print_endline(v)
end 

(*
Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)));; 

  let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) in 
  let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) in 
  Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1));; 

  Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])));; 

  Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])));;

let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1)) 
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv,
Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")), Zexpr.VAR "x"))))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv,
Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "x"))))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv,
Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "y", Zexpr.VAR "x")), Zexpr.VAR "y"))))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)));; 

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) in 
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) in
 Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])));; 

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])));;
*)
