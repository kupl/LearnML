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


  let diff : aexp * string -> aexp
  = fun (exp, var) -> exp;;
	
	let rec diff2 : aexp * string -> aexp
	= fun (exp, var) ->
		(match exp with
		 Sum s ->
				(match s with
				[]->Const 0
				|hd::[] -> diff2(hd, var)
				|hd::tl -> Sum[diff2 (hd, var); diff2 (Sum tl, var)])
				
		| Times s ->
				(match s with
				[]->Const 1
				|hd::[] -> (diff2(hd,var))
				|hd::tl ->  
						(match hd with
						Const n -> Times [Const n; diff2(Times tl, var)]
						|_ -> (Sum[Times [hd; diff2(Times tl, var)] ; Times (diff2 (hd,var) :: tl)])))
		| Power (s, n) -> 
				(if s=var then ( match n with
					0 -> Const 0
				|1 -> Const 1
				|_ -> Times [Const n; Power(s,n-1)])
				else Const 0)
		| Const n -> Const 0
		| Var x -> 
				if(x=var) then Sum[Const 1; Times[Var x;Const 0]]
				else Const 0);;
	let diff : aexp * string -> aexp
	=fun (exp, var) -> diff2 (exp, var);;
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

(*  let balanced : mobile -> bool
  = fun mob -> true;;
*)
	let rec multi n i
	= if(i=0) then 0 else n + (multi n (i-1));;

	let rec caltorque : mobile -> int
	= fun mob ->
			(match mob with
			|(SimpleBranch(l1,w1), CompoundBranch(l2,m))
					->(w1) + (caltorque m)
			|(SimpleBranch(l1,w1), SimpleBranch(l2,w2))
					-> w1 + w2
			|(CompoundBranch(l1,m1),CompoundBranch(l2,m2))
					->(caltorque m1)+(caltorque m2)
			|(CompoundBranch(l1,m), SimpleBranch(l2,w))
					->(caltorque m)+(w));;

	let rec balanced2 : mobile -> bool
	= fun mob ->
		(match mob with
		|(SimpleBranch(l1,w1), CompoundBranch(l2,m))
				->if (((multi l1 w1) = multi l2 (caltorque m))&&(balanced2 m))
							then true else false
		|(CompoundBranch(l1,m), CompoundBranch(l2,m2))
				->if (((multi l1 (caltorque m)) = (multi l2 (caltorque m2)))&& (balanced2 m)&&(balanced2 m2))
							then true else false
		|(SimpleBranch(l1,w1), SimpleBranch(l2,w2))
				->if (multi l1 w1) = (multi l2 w2)
							then true else false
		|(CompoundBranch(l1,m), SimpleBranch(l2,w1))
				->if (((multi l1 (caltorque m)) = (multi l2 w1))&&(balanced2 m))
							then true else false);;
	let balanced : mobile -> bool
	= fun mob -> balanced2 mob;;

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
  = fun exp -> raise NotImplemented
	
	let rec sigmaf
	= fun (e1,e2,e3) ->
		if(e1>e2) then 0
			else (e3 e1) + (sigmaf(e1+1,e2,e3));;

	let rec calculator2 
	= fun exp->
		match exp with
		|X -> fun e -> e
		|INT n -> fun e -> n
		|ADD(exp1,exp2) -> fun e -> ((calculator2 exp1) e) + ((calculator2 exp2) e)
		|SUB(exp1,exp2) -> fun e -> ((calculator2 exp1) e) - ((calculator2 exp2) e)
		|MUL(exp1,exp2) -> fun e -> ((calculator2 exp1) e) * ((calculator2 exp2) e)
		|DIV(exp1,exp2) -> fun e -> ((calculator2 exp1) e) mod ((calculator2 exp2)e)
	|_-> fun e -> 0;;

	let rec calculator1 : exp -> int
	= fun exp -> 
		match exp with
		|X -> failwith "Error"
		|INT e -> e
		|ADD (e1,e2) -> (calculator1 e1) + (calculator1 e2)
		|SUB (e1,e2) -> (calculator1 e1) - (calculator1 e2)
		|MUL (e1,e2) -> (calculator1 e1) * (calculator1 e2)
		|DIV (e1,e2) -> (calculator1 e1) mod (calculator1 e2)
 		|SIGMA(e1,e2,e)
			->sigmaf((calculator1 e1),(calculator1 e2),calculator2 e) ;;

	let calculator : exp -> int
	= fun exp -> calculator1 exp;;

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
  = fun exp -> true;;

	let rec check2
	= fun (exp,lis) ->
	match  exp with
	|C (exp1,exp2) -> if(check2(exp1,lis)&&check2(exp2,lis)) then true else false
	|V vars -> (match lis with
			|hd::tl -> if (vars=hd) then true else check2(V vars, tl)
			|[] -> false)
	|P (vars, exp2) -> check2(exp2, vars::lis);;
	

	let check : exp -> bool
	= fun exp -> check2 (exp,[]);; 
end

