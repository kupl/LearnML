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
	|Const c -> Const 0
	|Var x -> if x= var then Const 1 else Const 0
	|Power(x,n)->(if x=var then (if n>2||n<0 then Power(x,n-1) else if n=2 then Times[Const 2; Var x] else if n=1 then Const 1 else Const 0) else Const 0)
	|Times lexp ->(match lexp with
							|[]-> Const 1
							|hd::[]->diff(hd, var)
							|hd::tl ->(match hd with
												|Const 0 -> Const 0
												|Const 1 -> diff(Times tl, var)
												|Const c -> Times[Const c; diff(Times tl, var)]
												|_ -> Sum[Times (diff(hd,var)::tl);Times[hd;diff(Times tl, var)]]
												)
							)
	|Sum lexp -> match lexp with
						|[] -> Const 0
						|hd::[] ->diff(hd, var)
						|hd::tl -> Sum[diff(hd, var);diff(Sum tl, var)]												
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


	let rec cal_weight : branch->weight
	= fun br ->
	match br with
	|SimpleBranch(l, w) -> w
	|CompoundBranch(l, m) -> (match m with (lb,rb) -> (cal_weight lb)+(cal_weight rb))
	
	let rec cal_torque : branch->int
	=fun br -> match br with
	|SimpleBranch (l,w) -> l*w
	|CompoundBranch(l,m) -> (match m with (lb, rb) -> l*(cal_weight lb + cal_weight rb))


  let rec balanced : mobile -> bool
  = fun mob -> match mob with (lb, rb)->
(match (lb, rb) with 
|SimpleBranch(l1, w1), SimpleBranch(l2, w2) -> if l1*w1=l2*w2 then true
else false
|SimpleBranch(l1, w1),CompoundBranch(l2, m) -> (match m with (lb,rb) ->
(if (balanced m) then (if l2*(cal_weight(lb)+cal_weight(rb))=l1*w1 then true else false)else false))
|CompoundBranch(l1, m), SimpleBranch(l2, w2) -> (match m with (lb, rb) ->
	(if (balanced m) then (if l1*(cal_weight(lb)+cal_weight(rb)) = l2*w2 then true else false) else false))
|CompoundBranch(l1, m1), CompoundBranch(l2, m2) ->(match m1, m2 with (lb1, rb1),(lb2,rb2) ->
(if (balanced m1)&&(balanced m2) then (if l1*(cal_weight(lb1)+cal_weight(rb1))=l2*(cal_weight(lb2)+cal_weight(rb2)) then true else false) else false))
|_ -> raise NotImplemented
)
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

let rec sigma
=fun (f, a, b)
->if a>b then raise NotImplemented
else if a=b then (f a)
else (f b) + sigma (f, a, (b-1))
and forcal : exp -> int
	= fun ex -> match ex with
	|X->raise NotImplemented
	|INT n -> n
	|ADD(INT n1, INT n2) -> n1+n2
	|ADD(e1, e2) -> (forcal e1 + forcal e2)
	|SUB(INT n1, INT n2) -> n1-n2
	|SUB(e1, e2) -> forcal(e1)-forcal(e2)
	|MUL(INT n1, INT n2) -> n1*n2
	|MUL(e1, e2) -> forcal(e1)*forcal(e2)
	|DIV(INT n1, INT n2) -> n1/n2
	|DIV(e1, e2) -> forcal(e1)/forcal(e2)
	|SIGMA(INT n1, INT n2, e) -> sigma(tofun(e), n1, n2)
	|SIGMA(e1, e2, e3) ->sigma(tofun(e3), forcal(e1), forcal(e2))
and tofun : exp->(int->int)
	= fun ex -> match ex with
	|X ->(fun x->x)
	|INT n ->(fun x ->n )
	|ADD(e1, e2) ->(fun x->(((tofun e1) x) +((tofun e2) x )))
	|SUB(e1, e2) ->(fun x->(((tofun e1) x )-((tofun e2) x )))
	|MUL(e1, e2) ->(fun x->(((tofun e1) x )*((tofun e2) x )))
	|DIV(e1, e2) ->(fun x->(((tofun e1) x )/((tofun e2) x )))
	|_->(fun x->0)

let rec calculator : exp->int
	= fun exp -> match exp with
	|X->0
	|INT n ->n
	|ADD(INT n1, INT n2) ->n1+n2
	|ADD(e1, e2)->calculator(e1)+calculator(e2)
	|SUB(INT n1, INT n2) -> n1-n2
	|SUB(e1, e2) -> calculator(e1)-calculator(e2)
	|MUL(INT n1, INT n2) -> n1*n2
	|MUL(e1, e2)-> calculator(e1)*calculator(e2)
	|DIV(INT n1, INT n2) -> n1/n2
	|DIV(e1, e2)->calculator(e1)/calculator(e2)
	|SIGMA(INT n1, INT n2, e ) -> sigma(tofun(e), n1, n2)
	|SIGMA(e1, e2, e3) ->sigma(tofun(e3), calculator(e1), calculator(e2))
	|_-> raise NotImplemented



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

let rec forcheck 
=fun (ex, l) -> match ex with
	|V v-> (match l with []->false
											|hd::tl -> if v=hd then true else forcheck(V v, tl))
	|P(v, e1) -> forcheck(e1, v::l)
	|C(e1, e2) -> if forcheck(e1, l)=true && forcheck(e2, l)=true then true else false

  let check : exp -> bool
  = fun exp -> forcheck(exp,[])

end

