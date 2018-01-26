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
  |Const a-> Const 0
  |Var v->if v=var then Const 1 else Var v
  |Power (v,a)-> if v=var then(match a with 
	|2 -> Times[Const 2; Var v]
	|1 -> Const 1
	|0 -> Const 0
	|_ -> Times[Const a; Power (v, a-1)])
	else Power(v,a)
  |Times l->( match l with 
	h::t-> if t=[] then diff(h,var)
					else Sum[Times(diff(h,var)::t);Times[h;diff(Times t,var)]])
  |Sum l-> let rec subsum l = match l with
              [] -> [] 
              | h::t -> match h with 
                   Const 0 -> subsum(t)
                  |Times [Const 0] -> subsum(t)
                  | _ -> diff(h,var)::subsum(t)
              in Sum (subsum l)
  
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
  = fun mob -> let rec subbalanced m=match m with 
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1=l2*w2 then (true,w1+w2) else (false,0)
				|(SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> (match subbalanced(m2)with
																|(true,w2)-> if(l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0) )
				|(CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> ( match  subbalanced(m1)with 
																|(true,w1)-> if( l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0))
				|(CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> (match subbalanced(m1),subbalanced(m2) with
																|(true,w1),(true,w2)->if( l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0))
			in (match subbalanced mob with |(true,x)-> true
											|_-> false	)													
			
  
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

  let rec calculator : exp -> int
  = fun exp ->  let rec calc : exp * exp -> int 
=fun (v, e)-> match e with 
X -> calc(INT 0, v)
| INT(i) -> i
| ADD (e1,e2) -> calc(v,e1)+calc(v,e2)
| SUB (e1,e2) -> calc(v,e1)-calc(v,e2)
| MUL (e1,e2) -> calc(v,e1)*calc(v,e2)
| DIV (e1,e2) -> calc(v,e1)/calc(v,e2)
| SIGMA (i,j,e3) -> if i=j then calc(i,e3) else calc(v, (ADD(INT (calc(i,e3)), SIGMA( (INT (calc(INT 0,i)+1)),j,e3))))
in calc (INT 0,exp)
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
  = fun exp -> let rec envcheck: exp * exp list -> bool = fun (exp,li)->
		begin match exp with 
			V x->( match li with 
				[]->false
				|h::t-> if h=V x then true else envcheck(V x,t) )
			|P(v,e)->envcheck (e, (V v)::li)
			|C(e1,e2)-> envcheck(e1,li)&&envcheck(e2,li)
		end in envcheck(exp,[])
end

