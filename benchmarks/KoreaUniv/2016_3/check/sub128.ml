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
| Const x -> Const 0
| Var x -> if x = var then Const 1
           else Const 0 
| Power (x,y) -> if x = var then Times [Const y;Power (x,y-1)]
                 else Const 0
| Times x -> Const 0 
| Sum x -> let rec diff1 : aexp list -> aexp list
           = fun l ->  
           match l with
           | [] -> []
           | hd::tl -> diff (hd, var)::diff1(tl) 
in Sum (diff1 x) 
end

(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

 let rec balanced : mobile -> bool
  = fun mob -> match mob  with
| (SimpleBranch(l1,w1),SimpleBranch(l2,w2))->
						if l1>=1 || l2>=1 || w1>=1 || w2>=1 
							then (if w1/l2=w2/l1 then true else false)
					  else raise NotImplemented
| (SimpleBranch(l1,w1), CompoundBranch(l2,w2)) ->
            if w1/l2 = eval(w2)/l1 then true else false
| (CompoundBranch(l1,w1), SimpleBranch(l2,w2))->
						if eval(w1)/l2 = w2/l1 then true else false
| ( CompoundBranch(l1,w1), CompoundBranch(l2,w2))->
						if eval(w1)/l2 = eval(w2)/l1 then true else false
	and eval : mobile->int
	=fun mob1 -> match mob1 with
		    | (SimpleBranch(l1,w1), SimpleBranch(l2,w2))->w1 + w2
        | (SimpleBranch(l1,w1), CompoundBranch(l2,w2)) -> w1 + eval(w2)
       	| (CompoundBranch(l1,w1), SimpleBranch(l2,w2))->eval(w1) + w2
        | (CompoundBranch(l1,w1), CompoundBranch(l2,w2))->eval(w1) + eval(w2)

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
					let v1 = eval e1 env in
					let v2 = eval e2 env in v1 + v2	
				|(SUB(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in v1 - v2
				|(MUL(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in v1 * v2 
				|(DIV(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in
						if v2 = 0 then raise (Failure "Divide by 0")
						else v1 / v2
				|(SIGMA(e1,e2,e3),env)->
					let v1 = eval e1 env in
					let v2 = eval e2 env in
						if v1 > v2 then raise NotImplemented
						else if (v2-v1) < 1 then (eval e3(v1::env))
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
| V _ -> false
| C(_,_) -> false
| P(x, V y) -> if x = y then true else false
| P(x, P (y, z)) -> if x = y && check (P(x,z)) then true else false
| P(x, C (y, z)) -> check (P(x,y)) && check (P(x,z))
			
end


