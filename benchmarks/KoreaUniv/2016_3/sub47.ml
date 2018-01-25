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

	let rec map f( l,var) = match l with
										|[]->[]
										|hd :: tl->(f (hd,var)) :: (map f (tl,var))

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
			match exp with 
			|Const n -> Const 0
			|Var a -> if var=a then Const 1 else Const 0
			|Power(a,n) ->if var=a
											then(match n with
													|0 -> Const 0
													|1 -> Const 1
													|2 -> Times[Const 2; Var a]
													|_ -> Times[Const n; Power(a,n-1)])
										else Const 0
			|Sum l -> Sum(map diff (l,var))
			|Times l -> begin match l with
									|[]->Const 1
									|hd :: tl -> 
											begin match hd with
											|Power(a,n) -> 
												if var= a then 
													(if tl=[] then diff(Power(a,n),var)
													 else Times[diff(Power(a,n),var);Times tl])
												else (if tl=[] then hd
															else Times[hd;diff(Times tl,var)])
											|Var a -> 
												if var=a then 
															(if tl=[] then Const 1
															 else Times[Const 1;Times tl])
												else Times[hd;diff(Times tl,var)]
											| _ -> if tl=[] then Const 1
														 else Times[hd;diff(Times tl,var)]
											end	
									end
										(*	|Times(hd::tl)->match hd with
																			|Power("x",n)->Times[Times[Power("x",n);Const n];diff(Power("x",n),var)]
																			|_->Times(hd :: Times(tl))   *)
(*	and sup_t : aexp list * string -> aexp
	= fun (l,var) ->
			match l with 
				|hd :: tl ->
						match hd with
						|Power("x",n)->if var="x" then Times [diff(Power("x",n),var);Times tl]
													 else hd :: Times tl
						| _ -> hd::Times tl  *)
								

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
 

 let rec balanced : mobile -> bool
  = fun mob ->
		 match mob  with
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->
						if l1>=1 || l2>=1 || w1>=1 || w2>=1 
							then (if w1/l2=w2/l1 then true else false)
					  else raise NotImplemented
				|(CompoundBranch(l1,w1),SimpleBranch(l2,w2))->
						if eval(w1)/l2=w2/l1 then true else false
				|(SimpleBranch(l1,w1),CompoundBranch(l2,w2))->
						if w1/l2=eval(w2)/l1 then true else false
				|(CompoundBranch(l1,w1),CompoundBranch(l2,w2))->
						if eval(w1)/l2=eval(w2)/l1 then true else false
	and eval : mobile->int
	=fun m ->
		 match m with
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->w1+w2
				|(CompoundBranch(l1,w1),SimpleBranch(l2,w2))->eval(w1)+w2
				|(SimpleBranch(l1,w1),CompoundBranch(l2,w2))->w1+eval(w2)			
				|(CompoundBranch(l1,w1),CompoundBranch(l2,w2))->eval(w1)+eval(w2)

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

  let calculator exp=
  	 let rec eval exp env=
			match (exp,env) with
				|(X,[])->raise NotImplemented
				|(X,hd::tl)->hd
				|(INT i,_)->i
				|(ADD(e1,e2),env)->
					let v1=eval e1 env in
					let v2=eval e2 env in
						v1+v2
				|(SUB(e1,e2),env)->
					let v1=eval e1 env in
					let v2=eval e2 env in
						v1-v2
				|(MUL(e1,e2),env)->
					let v1=eval e1 env in
					let v2=eval e2 env in
						v1*v2
				|(DIV(e1,e2),env)->
					let v1=eval e1 env in
					let v2=eval e2 env in
						if v2=0 then raise NotImplemented
						else v1/v2
				|(SIGMA(e1,e2,e3),env)->
					let v1=eval e1 env in
					let v2=eval e2 env in
						if v1>v2 then raise NotImplemented
						else if (v2-v1)<1 then (eval e3(v1::env))
						else (eval e3(v1::env))+(eval(SIGMA(INT(v1+1),INT v2,e3)) env)
					in eval exp[] 
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

  let rec check : exp -> bool
  = fun exp -> 
			match exp with	
			|V _ -> false
			|C(_,_) -> false
			|P(v1, V v2)-> if v1=v2 then true else false
			|P(v1, P (v2, e1))-> if v1=v2 && check (P(v1,e1)) then true else false
			|P(v1, C (e1, e2))-> check (P(v1,e1)) && check (P(v1,e2))

end

