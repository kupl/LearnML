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

let sat 
= fun f -> let rec env : formula -> string list
			= fun form -> match form with
						| True -> []
						| False -> []
						| Var x -> [x]
						| Neg x -> env x
						| And (x, y) -> (env x)@(env y)
						| Or (x, y) -> (env x)@(env y)
						| Imply (x, y) -> (env x)@(env y)
						| Iff (x, y) -> (env x)@(env y) in
			let rec env2 : string list -> string list
			=fun l -> match l with
			| [] -> []
			| hd::tl -> if List.mem hd tl then env2 tl else hd::(env2 tl)
		in
			let rec mapping : formula -> string -> bool -> formula
			=fun l s b -> match l with
			  | True -> True
			  | False -> False
			  | Var s -> if b then True else False
			  | Neg f -> mapping f s (not(b))
			  | And (f1,f2) -> And (mapping f1 s b, mapping f2 s b)
			  | Or (f1,f2) -> Or (mapping f1 s b, mapping f2 s b)
			  | Imply (f1,f2) -> Imply (mapping f1 s b, mapping f2 s b)
			  | Iff (f1,f2) -> Iff (mapping f1 s b, mapping f2 s b)
			in
			let rec make_flist : formula list -> string -> formula list
			=fun fl s -> match fl with
			| [] -> []
			| hd::tl -> [mapping hd s true;mapping hd s false]@(make_flist tl s)
			in
			let rec pop_slist : formula list -> string list -> formula list
			= fun fl st -> match st with
			| [] -> []
			| hd::tl -> (make_flist fl hd)@(pop_slist fl tl)
			in
			let rec eval : formula -> bool
			=fun l -> match l with
			  | True -> true
			  | False -> false
			  | Var s -> raise (Failure "Error!")
			  | Neg f -> if eval f then false else true
			  | And (f1,f2) -> (eval f1) && (eval f2)
			  | Or (f1,f2) -> (eval f1) || (eval f2)
			  | Imply (f1,f2) -> eval (Or ((Neg f1),f2))
			  | Iff (f1,f2) -> (eval (Imply (f1,f2)))&&(eval (Imply (f2,f1)))
			in
			let rec real_sat : formula list -> bool
			= fun fl -> match fl with
			| [] -> false
			| hd::tl -> if eval hd then true else real_sat tl
		in
		real_sat (pop_slist [f] (env2 (env f)));;
