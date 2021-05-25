
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
		| Var x -> if x = var then Const 1 else Const 0
		| Power (x, y) -> if x = var then Times[Const y; Power(x, y - 1)] else Const 0
		| Const x -> Const 0
		| Sum x -> let rec sum : aexp list -> aexp list 
							= fun li -> match li with
								| [] -> []
								| hd::tl -> (diff (hd, var))::(sum tl)
								in Sum (sum x)
		| Times x -> let rec product : aexp list -> int -> aexp list
							= fun li n -> match li with
								| [] -> []
								| hd::tl -> if n = 0 then (diff (hd, var))::(product tl (n-1)) else hd::(product tl (n-1))
							in let rec time : aexp list -> aexp list -> int -> aexp list
							= fun li chli n -> match chli with
								| [] -> []
								| hd::tl -> (Times (product li n))::(time li tl (n+1))
							in Sum (time x x 0)