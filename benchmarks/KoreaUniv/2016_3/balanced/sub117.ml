
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight x=
	match x with 
	|(SimpleBranch(a, b), SimpleBranch(c,d)) -> b+d
	|(SimpleBranch(a, b), CompoundBranch(c,d)) -> b+(weight d)
	|(CompoundBranch(a, b), SimpleBranch(c,d)) -> (weight b) + d
	|(CompoundBranch(a, b), CompoundBranch(c,d)) -> (weight b)+(weight d)

  let rec balanced : mobile -> bool
  = fun mob ->
	match mob with 
	|(SimpleBranch(a,b), SimpleBranch(c,d)) -> if a*b=c*d then true else false
	|(SimpleBranch(a,b), CompoundBranch(c,d)) -> if (a*b=c*(weight d)) && balanced d then true else false
	|(CompoundBranch(a,b), SimpleBranch(c,d)) -> if (a*(weight b)=c*d) && balanced b then true else false
	|(CompoundBranch(a,b), CompoundBranch(c,d)) -> if (a*(weight b)=c*(weight d)) && balanced b && balanced d then true else false

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

  let calculator : exp -> int
  = fun exp -> raise NotImplemented  (* TODO *)
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

	let empty = []
	
	let rec lookup env var =
		match env with
		|[] -> false
		|hd::tl -> if hd = var then true else lookup tl var

	let extend env v = v::env

	let check : exp -> bool
  = fun exp ->
	let rec c_env env ex = 
	match ex with
	|V a -> lookup env a
	|P(a,b) -> let env2 = extend env a in c_env env2 b
	|C(a,b) -> if c_env env a && c_env env b then true else false
	in c_env empty exp
	
end

