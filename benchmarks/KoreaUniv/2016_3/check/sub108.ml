(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
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
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec getWeight : mobile -> int
	= fun mo -> match mo with
	  				| SimpleBranch(a,b), SimpleBranch(c,d) -> b + d
	  				| SimpleBranch(a,b), CompoundBranch(c,d) -> b + (getWeight d)
     				| CompoundBranch(a,b), SimpleBranch(c,d) -> (getWeight b) + d
						| CompoundBranch(a,b), CompoundBranch(c,d) -> (getWeight b) + (getWeight d)

	let branchval : branch -> int 
	= fun br -> match br with
	| SimpleBranch (x, y) -> x * y
	| CompoundBranch (x, y) -> x * (getWeight y)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (x,y) -> match x, y with
						| SimpleBranch(a,b), SimpleBranch(c,d) -> branchval x = branchval y
						| SimpleBranch(a,b), CompoundBranch(c,d) -> branchval x = branchval y && (balanced d)
						| CompoundBranch(a,b), SimpleBranch(c,d) -> branchval x = branchval y && (balanced b)
						| CompoundBranch(a,b), CompoundBranch(c,d) -> branchval x = branchval y && (balanced b) && (balanced d)
end
(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec getVal ex va sigma calculator  = match ex with
	| X -> va
	| INT x -> x
	| ADD (x, y) -> (getVal x va sigma calculator) + (getVal y va sigma calculator)
	| SUB (x, y) -> (getVal x va sigma calculator) - (getVal y va sigma calculator)
	| MUL (x, y) -> (getVal x va sigma calculator) * (getVal y va sigma calculator)
	| DIV (x, y) -> (getVal x va sigma calculator) / (getVal y va sigma calculator)  
	| SIGMA (x, y, z) -> sigma (calculator x) (calculator y) z calculator
	
	let rec sigma st en ex calculator = if st <= en then (getVal ex st sigma calculator) + (sigma (st+1) en ex  calculator) else 0  

  let rec calculator : exp -> int
  = fun exp -> match exp with
	| INT x -> x
	| ADD (x, y) -> calculator x + calculator y
	| SUB (x, y) -> calculator x - calculator y
	| MUL (x, y) -> calculator x * calculator y
	| DIV (x, y) -> calculator x / calculator y
	| SIGMA (x, y, z) -> sigma (calculator x) (calculator y) z calculator
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> let rec inList li str = match li with
								| [] -> false
								| hd::tl -> if hd = str then true else inList tl str
							in let rec chk l ex = match ex with
								| V x -> inList l x
								| P (x,y) -> chk (x::l) y
								| C (x,y) -> chk l x && chk l y
							in chk [] exp
end
