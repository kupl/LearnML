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

  type value = int
  type environment = (id * value) list

  let emptyEnv : environment = []
  let rec eval : environment * expr -> value = fun(env, x) ->
    let rec findVarFromEnv: environment * id -> value = fun (env, varId) ->
      match env with
        |[] -> raise(Error "FreeVariable")
        |(envId, envVar) :: et ->
          if (envId = varId) then envVar
          else findVarFromEnv(et, varId)
    in
    let getMaxFromExprList: expr list -> value = fun (el) ->
      let rec foldMax: value * expr list -> value = fun(currentMax, residualList) ->
        match residualList with
          |[] -> currentMax
          |h :: t -> 
            let headEval = eval(env, h) in
            if (currentMax > headEval) then foldMax(currentMax, t)
            else foldMax(headEval, t)
      in
      match el with
        |[] -> 0
        |eh :: et -> foldMax(eval(env, eh), et)
    in
    match x with
      |NUM i -> i
      |PLUS(a,b) -> eval(env, a) + eval(env, b)
      |MINUS(a,b) -> eval(env, a) - eval(env, b)
      |MULT(a,b) -> eval(env, a) * eval(env, b)
      |DIVIDE(a,b) -> eval(env, a) / eval(env, b)
      |MAX(el) -> getMaxFromExprList(el)
      |VAR(varId) -> findVarFromEnv(env, varId)
      |LET(varId, varValue, varEval) -> eval((varId, eval(env, varValue)) :: env, varEval)

  let print_value : value -> unit = fun(v) -> print_endline(string_of_int v)
end 

(* test *)
(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.NUM 1)), print_string "Case 1 : 1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")), Zexpr.VAR "x")))), print_string "Case 2 : 5 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX []))), print_string "Case 3 : 0 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)]))), print_string "Case 4 : -1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.DIVIDE(Zexpr.NUM 3, Zexpr.NUM 2)))), print_string "Case 5 : 1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.PLUS(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 6 : 16 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MINUS(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 7 : -2 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MULT(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 8 : 63 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "x")))), print_string "Case 9 : 4 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.LET("x", Zexpr.NUM 1, Zexpr.LET("y", Zexpr.NUM 2, Zexpr.LET("z", Zexpr.NUM (-1), Zexpr.MAX[Zexpr.VAR "x"; Zexpr.VAR "y" ; Zexpr.VAR "z"])))))), print_string "Case 10 : 2 vs " 
let _ = try Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.LET("x", Zexpr.NUM 1, Zexpr.LET("y", Zexpr.NUM 2, Zexpr.LET("z", Zexpr.NUM (-1), Zexpr.MAX[Zexpr.VAR "x"; Zexpr.VAR "y" ; Zexpr.VAR "z"; Zexpr.VAR "a"])))))) with Zexpr.Error x -> 
  if (x = "FreeVariable") then print_endline("Error Case 1 : Pass") 
  else print_endline("Error Case 1 : Failure") 
let _ = try Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "y")))) with Zexpr.Error x -> 
  if (x = "FreeVariable") then print_endline("Error Case 2 : Pass") 
  else print_endline("Error Case 2 : Failure")
*)
