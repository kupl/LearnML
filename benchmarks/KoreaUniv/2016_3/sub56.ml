(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented
exception InputError
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

  let rec listt1 : aexp list*string -> aexp list
  = fun (lst,var) -> match lst with
  | [] -> []
  | hd::tl -> diff(hd,var)::listt1(tl,var)

  and listt2 : aexp list*string*aexp list -> aexp list
  = fun (lst,var,temp) -> match lst with
  | [] -> []
  | hd::tl -> Times((temp@diff(hd,var)::tl))::listt2(tl,var,(temp@[hd]))

  and diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Sum(lst) -> Sum(listt1(lst,var))
  | Times(lst) -> Sum(listt2(lst,var,[]))
  | Power(v,n) -> if(var=v) then if n>2 then Times([Const n;Power(v,n-1)]) else if n=2 then Times([Const 2;Var v]) else if n=1 then Const 1 else Const 0
                  else Const(0)
  | Const(x) -> Const(0)
  | Var(v) -> if(var=v) then Const(1)
              else Const(0)
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
  let rec temp : mobile -> int
  = fun mob -> match mob with
      | (SimpleBranch(l,w),SimpleBranch(ll,ww)) -> w+ww
	  | (SimpleBranch(l,w),CompoundBranch(ll,mm)) -> w + temp(mm)
	  | (CompoundBranch(l,m),SimpleBranch(ll,ww)) -> temp(m) + ww
	  | (CompoundBranch(l,m),CompoundBranch(ll,mm)) -> temp(m) + temp(mm)
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
      | (SimpleBranch(l,w),SimpleBranch(ll,ww)) -> if (l*w)=(ww*ll) then true else false
	  | (SimpleBranch(l,w),CompoundBranch(ll,mm)) -> if (l*w)=(ll*temp(mm)) && balanced(mm)=true then true else false
	  | (CompoundBranch(l,m),SimpleBranch(ll,ww)) -> if (l*temp(m))=(ll*ww) && balanced(m)=true then true else false
	  | (CompoundBranch(l,m),CompoundBranch(ll,mm)) -> if (l*temp(m))=(ll*temp(mm)) && balanced(m)=true && balanced(mm)=true then true else false
	
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
  let rec exchange : exp * int -> exp
  = fun (e,v) -> match e with
  | X -> INT(v)
  | INT(x) -> INT(x)
  | ADD(e1,e2) -> ADD(exchange(e1,v),exchange(e2,v))
  | SUB(e1,e2) -> SUB(exchange(e1,v),exchange(e2,v))
  | MUL(e1,e2) -> MUL(exchange(e1,v),exchange(e2,v))
  | DIV(e1,e2) -> DIV(exchange(e1,v),exchange(e2,v))
  | SIGMA(e1,e2,e3) -> SIGMA(exchange(e1,v),exchange(e2,v),e3)

  let rec calculator : exp -> int
  = fun exp -> match exp with
  | INT(x) -> x
  | ADD(a,b) -> calculator(a)+calculator(b)
  | SUB(a,b) -> calculator(a)-calculator(b)
  | MUL(a,b) -> calculator(a)*calculator(b)
  | DIV(a,b) -> calculator(a)/calculator(b)
  | SIGMA(a,b,c) -> if calculator(a)=(calculator(b)+1) then 0 else (calculator(SIGMA(INT(calculator(a)+1),b,c))+calculator(exchange(c,calculator(a))))
  | X -> raise InputError 
						
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

  let rec exist : var*var list -> bool
  = fun (var,lst) -> match lst with
  | [] -> false
  | hd::tl -> if hd=var then true else exist(var,tl)

  let rec checkk : exp*var list -> bool
  = fun (exp,lst) -> match exp with
  | V(v) -> if(exist(v,lst)) then true else false
  | C(e1,e2) -> if(checkk(e1,lst) && checkk(e2,lst)) then true else false
  | P(v,e) -> checkk(e,v::lst)

  let rec check : exp -> bool
  = fun exp -> match exp with
  | V(v) -> false
  | C(e1,e2) -> if(check(e1) && check(e2)) then true else false
  | P(v,e) -> checkk(e,[v])
end