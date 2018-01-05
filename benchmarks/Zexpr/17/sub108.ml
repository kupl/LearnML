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

  type environment = (id * expr) list
  type value = int

  let emptyEnv = []
  let rec eval (env, exp) : value = 
    match exp with
    | NUM i -> i
    | PLUS (a, b) -> eval(env, a) + eval(env, b)
    | MINUS (a, b) -> eval(env, a) - eval(env, b)
    | MULT (a, b) -> eval(env, a) * eval(env, b)
    | DIVIDE (a, b) when eval(env, b) = 0 -> raise (Error "divide by zero")
    | DIVIDE (a, b) -> eval(env, a) / eval(env, b)
    | MAX l -> ( 
        let comp a b: int = b - a in
        let ev exp : value = eval (env, exp) in
        match l with
        | [] -> 0
        | _ -> List.hd(List.sort comp (List.map ev l))
    )
    | VAR id
        when (let f v : bool = (id = fst v) in
        List.exists f env) -> (let f v : bool = (id = (fst v)) in
            eval (env, snd (List.find f env))
        )
    | VAR id -> raise (Error "FreeVariable")
    | LET(id, ex1, ex2) 
        when (let f v : bool = (id = fst v) in
        List.exists f env) -> (let f v : bool = (id <> fst v) in
            eval((id, NUM (eval(env, ex1)))::(List.filter f env), ex2)
        )
    | LET(id, ex1, ex2) -> eval ((id, NUM (eval(env,ex1)))::env, ex2)

  let print_value v : unit = print_int v
end

(*let print = fun x -> Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, x))
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
let _ = try print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"; var "a"])))) with Zexpr.Error x ->
          if (x = "FreeVariable") then print_endline("Error Case 1 : Pass")
          else print_endline("Error Case 1 : Failure")
let _ = try print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "y"))) with Zexpr.Error x ->
          if (x = "FreeVariable") then print_endline("Error Case 2 : Pass")
          else print_endline("Error Case 2 : Failure")*)
