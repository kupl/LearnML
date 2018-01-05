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
	
let rec list_max xs =
      match xs with
      | [] ->   (* empty list: fail *)
         failwith "list_max called on empty list"
      | [x] -> (* single element list: return the element *)
         x
      | x :: remainder -> (* multiple element list: recursive case *)
         max x (list_max remainder)
(*
let rec var (x, s) = 
match x with
| [] -> raise Error "FreeVariable"
| x1::x2 -> 
	if(x1.fst = s) then x1
	else var(x2, s)

let rec maxExpr (e1, l) = 
match l with
| [] -> []
| hd::tl -> Zexpr.eval(e1,hd) :: maxExpr(e1, tl)
*)
let rec filter (l, s) =
match l with
| [] -> []
| hd::tl -> if(fst hd = s) then tl
	else hd:: filter (tl, s)
 
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
	type value = int
	let emptyEnv : environment = []
	let rec eval e =
		match e with
		| (e1, NUM n) -> n
		| (e1, PLUS (n1, n2)) -> eval (e1, n1) + eval (e1, n2)
		| (e1, MINUS (n1, n2)) -> eval (e1, n1) - eval (e1, n2)
		| (e1, MULT (n1, n2)) -> eval (e1, n1) * eval (e1, n2)
		| (e1, DIVIDE (n1, n2)) -> eval (e1, n1) / eval (e1, n2)
		| (e1, VAR s) -> 
			let rec var : environment * id -> int = fun (x, s) -> 
			match x with
			| [] -> raise (Error "FreeVariable")
			| x1::[] -> if(fst x1 = s) then (snd x1)
					else raise (Error "FreeVariable")
			| x1::x2 -> 
				if(fst x1 = s) then (snd x1)
				else var(x2, s) in
			var(e1,s)
		| (e1, MAX x) -> 
			let rec maxExpr (e1, l) = 
			match l with
			| [] -> []
			| hd::tl -> eval(e1,hd) :: maxExpr(e1, tl) in
			if(x = []) then 0
			else list_max(maxExpr (e1, x))
		| (e1, LET(x, y, z)) ->  	
			eval((x, eval (e1, y))::filter(e1, x), z)
	let print_value : value -> unit = fun a -> print_endline(string_of_int(a))
end 

(*
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
*)
