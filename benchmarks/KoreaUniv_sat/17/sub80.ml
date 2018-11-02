(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> 
	let rec makeBoolTable f table = 
		match f with 
		| True | False -> table
		| Var p -> let rec find_var var var_list =
					match var_list with
					| [] -> ((var, true) :: table)
					| hd :: tl -> if hd = (var, true) then table else find_var var tl
				in find_var p table
		| Neg p -> makeBoolTable p table
		| And (p, q) | Or (p, q) | Imply (p, q) | Iff (p, q) 
		-> makeBoolTable q (makeBoolTable p table)
	in

	let rec loopTable t = 
		match t with
		| hd :: tl -> (match hd with
						| (v, true) -> ((v, false) :: tl)
						| (v, false) -> ((v, true) :: (loopTable tl))
						)
		| [] -> []
	in 

	let rec sat_f f table value =
		match f with
		| True -> if value = true then true else false
		| False -> if value = false then true else false
		| Var p -> let rec find_var var table =
					match table with
					| hd :: tl -> if hd = (var, value) then true else if hd = (var, not(value)) then false else find_var var tl
					| [] -> raise (Failure "RunTimeError: why [] is here?")
				in find_var p table
		| Neg p -> sat_f p table (not(value))
		| And (p, q) -> if value = true then ((sat_f p table true) && (sat_f q table true))
						else (* value = false *)
						let caseFT = ((sat_f p table false) && (sat_f q table true))
						in let caseTF = ((sat_f p table true) && (sat_f q table false))
						in let caseFF = ((sat_f p table false) && (sat_f q table false))
						in (((caseFT) || (caseTF)) || caseFF)

		| Or (p, q) -> sat_f (And ((Neg p), (Neg q))) table (not(value))
		| Imply (p, q) -> sat_f (And (p, (Neg q))) table (not(value))
		| Iff (p, q) -> sat_f (And ((Imply (p, q)), (Imply (q, p)))) table value
	in

	let rec solve_sat f table =
		match table with
		| [] -> false
		| _ -> if (sat_f f table true) then true
					else (sat_f f (loopTable table) true)
	in

	let solve_sat_init f table = 
	(* this init function is for No variable formula like "True and False" *)
		match table with
		| [] -> sat_f f table true
		| _ -> solve_sat f table

	in solve_sat_init f (makeBoolTable f [])


