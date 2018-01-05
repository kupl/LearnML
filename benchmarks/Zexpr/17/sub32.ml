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
        type expr = NUM of int
                  | PLUS of expr * expr
                  | MINUS of expr * expr
                  | MULT of expr * expr
                  | DIVIDE of expr * expr
                  | MAX of expr list
                  | VAR of id
                  | LET of id * expr * expr
        
        type value = int
        type environment = (id * value) list

        let emptyEnv = []
        let rec eval ((env : environment), (exp : expr)) : value =
            match exp with
            | NUM n -> n
            | PLUS (exp1, exp2) -> (eval (env, exp1)) + (eval (env, exp2))
            | MINUS (exp1, exp2) -> (eval (env, exp1)) - (eval (env, exp2))
            | MULT (exp1, exp2) -> (eval (env, exp1)) * (eval (env, exp2))
            | DIVIDE (exp1, exp2) -> (eval (env, exp1)) / (eval (env, exp2))
            | MAX exps -> 
                    (match exps with
                     | [] -> 0
                     | [hd] -> eval (env, hd)
                     | hd::tl -> let max = eval (env, MAX tl) in
                                 let tmp = eval (env, hd) in
                                 if tmp > max then tmp else max)
            | VAR id -> 
                    (try List.assoc id env with
                    Not_found -> raise (Error "FreeVariable"))
            | LET (id, exp1, exp2) ->
                    eval ((id, (eval (env, exp1)))::env, exp2)
        
        let print_value value = 
            print_int value;
            print_newline ();

end 

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
let _ = try print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"; var "a"])))) with Zexpr.Error x -> 
          if (x = "FreeVariable") then print_endline("Error Case 1 : Pass") 
          else print_endline("Error Case 1 : Failure") 
let _ = try print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "y"))) with Zexpr.Error x -> 
          if (x = "FreeVariable") then print_endline("Error Case 2 : Pass") 
          else print_endline("Error Case 2 : Failure")
