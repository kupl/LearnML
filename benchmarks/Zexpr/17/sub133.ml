(*
 * 2017 - 09 - 22
 * PL Homework 2-7
 * Joonmo Yang
*)

module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr
  type value
  type env
  val emptyEnv: env
  val eval: env * expr -> value
  val print_value: value -> unit
end

module Zexpr : ZEXPR =
struct
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr
  type value = int
  type env = (id * value) list
  let emptyEnv = [] 
  let rec eval (e, exp) =
    ( match exp with
      | NUM n -> n
      | PLUS (e1, e2) -> eval(e, e1) + eval(e, e2)
      | MINUS (e1, e2) -> eval(e, e1) - eval(e, e2) 
      | MULT (e1, e2) -> eval(e, e1) * eval(e, e2)
      | DIVIDE (e1, e2) -> eval(e, e1) / eval(e, e2)
      | MAX elist -> if (List.length elist) = 0  then 0
                     else List.fold_left (fun x y -> if x > eval(e, y) then x else eval(e,y)) (eval(e, (List.hd elist))) elist
      | VAR v -> if not(List.mem_assoc v e) then raise (Error "Free variable")
                 else List.assoc v e
      | LET (var, e1, e2) -> if (List.mem_assoc var e) then eval(((var, eval(e, e1))::(List.remove_assoc var e)), e2)
                             else eval((var, eval(e, e1))::e, e2)
    )
  let print_value v = print_int v; print_endline " "
end

(* test cases
let print = fun x -> Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, x)) 
let var = fun x -> Zexpr.VAR x 
let num = fun x -> Zexpr.NUM x 
let set = fun (x, y, z) -> Zexpr.LET(x, y, z) 
let plus = fun (x, y) -> Zexpr.PLUS(x, y) 
let minus = fun (x, y) -> Zexpr.MINUS(x, y) 
let div = fun (x, y) -> Zexpr.DIVIDE(x, y) 
let mul = fun (x, y) -> Zexpr.MULT(x, y) 
let max = fun x -> Zexpr.MAX x 

let _ = print(num 1), print_string "Case 1 : 1 vs " 
let _ = print(set("x", num 1, plus(set("x", num 2, plus(var "x", var "x")), var "x"))), print_string "Case 2 : 5 vs " 
let _ = print(max []), print_string "Case 3 : 0 vs " 
let _ = print(max [num(-1); num(-2); num(-3)]), print_string "Case 4 : -1 vs " 
let _ = print(div(num 3, num 2)), print_string "Case 5 : 1 vs " 
let _ = print(plus(num 7, num 9)), print_string "Case 6 : 16 vs " 
let _ = print(minus(num 7, num 9)), print_string "Case 7 : -2 vs " 
let _ = print(mul(num 7, num 9)), print_string "Case 8 : 63 vs " 
let _ = print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "x"))), print_string "Case 9 : 4 vs " 
let _ = print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"])))), print_string "Case 10 : 2 vs " 
*)
