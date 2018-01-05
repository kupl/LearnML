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

  type environment = expr list
  type value = int

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

  type environment = expr list

  type value = int
  let emptyEnv = []
  let rec addtolist inputlist element = List.append [element] inputlist
  let convertexpr a = match a with
    |NUM b -> b
    |_ -> -100000000000
  let rec findindex value inputlist : int= match inputlist with
    |[] -> -1
    |h :: t -> if h = value then 0 else 1 + findindex value t
  let rec findmax list1 = match list1 with
    |[] -> []
    |hd::tl -> (convertexpr hd)::(findmax tl)

  let findgreater (x:int) (y:int):int= if x>y then x else y
  let rec eval ((env:expr list), input) = match (env, input) with
    |(_, NUM a) -> a
    |(env, PLUS(a,b)) -> eval(env, a) + eval(env, b)
    |(env, MINUS(a,b)) -> eval(env, a) - eval(env, b)
    |(env, MULT(a,b)) -> eval(env, a) * eval(env,b)
    |(env, DIVIDE(a,b)) -> eval(env, a)/eval(env,b)
    |(env, MAX a) -> if List.length a = 0 then 0 else
      (if List.length env != 0 then let g1 = List.nth (List.rev (List.sort compare (findmax a))) 0 in let g2 = List.nth (List.rev (List.sort compare (findmax env))) 0 in findgreater g1 g2 else List.nth (List.rev (List.sort compare (findmax a))) 0)
    |(env, VAR a) -> (match env with
        |_ -> if (List.mem (VAR a) env) then let indexnumber = (findindex (VAR a) env -1) in eval(env,(List.nth env indexnumber)) else raise(Error "FreeVariable"))
    |(env, (LET(id, e1, e2))) -> (match e1 with
      |NUM a -> eval(addtolist (addtolist env (VAR id)) e1 , e2)
      |VAR a -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, VAR a))) , e2)
      |MINUS(a,b) -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, MINUS(a,b)))) , e2)
      |MULT(a,b) -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, MULT(a,b) ))) , e2)
      |DIVIDE(a,b) -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, DIVIDE(a,b) ))) , e2)
      |MAX a -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, MAX a))) , e2)
      |PLUS(a,b) -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, PLUS(a,b)))) , e2)
      |LET(id1, e3, e4) -> eval(addtolist (addtolist env (VAR id)) (NUM (eval(env, LET(id1, e3, e4)))) , e2))



  let print_value a = print_endline(string_of_int(a))
  end


let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1))

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
