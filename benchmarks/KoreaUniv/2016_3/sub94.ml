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
|Const n -> Const 0
|Var s -> if s=var then Const 1 else Const 0
|Power (s,n) -> if s=var then Times [Const n;Power(s,n-1)] else Const 0 

|Times t -> (match t with [] -> Const 0
												|hd::tl -> Sum[Times ([(diff (hd,var))]@tl); Times[hd;diff (Times tl,var)]])
|Sum l -> (match l with [] -> Const 0
                        |hd::tl -> Sum[diff(hd,var);diff(Sum tl,var)])
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

  let rec weight : mobile -> int
  = fun mob -> match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d))->b+d
	|(SimpleBranch(a,b),CompoundBranch(c,d))->b+(weight d)
	|(CompoundBranch(a,b),SimpleBranch(c,d))->(weight b)+d
	|(CompoundBranch(a,b),CompoundBranch(c,d))->(weight b)+(weight d)

  let rec  balanced : mobile -> bool
  = fun mob -> match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b = c*d then true else false
  |(SimpleBranch(a,b),CompoundBranch(c,d))-> if (balanced d)=true then if a*b=c*(weight d) then true else false 
																					   else false 
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> if (balanced b)=true then if a*(weight b)=c*d then true else false
																						 else false 
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> if (balanced b)&&(balanced d)=true then if a*(weight b)=c*(weight d) then true else false
																						 else false  
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
= fun exp -> match exp with
|INT n->n
|ADD (left,right) -> calculator left + calculator right
|SUB (left,right) -> calculator left - calculator right
|MUL (left,right) -> calculator left * calculator right
|DIV (left,right) -> if calculator right = 0 then raise NotImplemented else calculator left / calculator right
|SIGMA(left,middle,right) -> proc(calculator left,calculator middle, right) 

and  proc2 : int * exp -> int
= fun (a,b) -> match b with
|X -> a
|INT n->n
|ADD (left,right) -> proc2 (a,left) + proc2 (a,right) 
|SUB (left,right) -> proc2 (a,left) - proc2 (a,right)
|MUL (left,right) -> proc2 (a,left) * proc2 (a,right)
|DIV (left,right) -> if proc2 (a,right) = 0 then raise NotImplemented else proc2(a,left) / proc2(a,right) 
|SIGMA (left,middle,right) -> calculator b 

and  proc : int*int*exp -> int
= fun (a,b,c) -> if a<=b then proc2(a,c) + proc(a+1,b,c) else 0

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
    
  let rec checklist : (var list)*(var) -> bool
  = fun (l,v) -> match l with
  |[] -> false
  |hd::tl -> if hd=v then true else checklist (tl,v)

  let rec check2 : exp*(var list) -> bool
  = fun (exp,l) -> match exp with
  |V v -> if (checklist(l,v)) then true else false
  |P (v,e) -> check2 (e,v::l)
  |C (e1,e2) -> check2(e1,l)&&check2 (e2,l) 


  let check : exp -> bool
  = fun exp -> check2 (exp,[]) 
end

