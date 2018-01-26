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
  = fun (exp, var) -> 

match exp  with
|Var x -> if x=var then Const 1 else Const 0
|Const _ -> Const 0
|Power (x, a) -> if not(x=var) then Const 0
 								 else  Times [Const a; Power (var, a-1)]
|Sum lst -> Sum (List.map (fun y -> diff (y,var)) lst)
|Times [] -> Const 0
|Times (hd::tl) -> Sum [Times (diff (hd,var)::tl); Times [hd; diff(Times tl, var)]]


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

let rec sum : mobile -> int
	=fun mob ->
match mob with

|SimpleBranch(a,b), SimpleBranch(c,d) -> b+d
|SimpleBranch(a,b), CompoundBranch(c,m) -> b + (sum m)
|CompoundBranch(c,m), SimpleBranch(a,b) -> b + (sum m)
|CompoundBranch(a,m1), CompoundBranch(b,m2) -> (sum m1) + (sum m2)


let rec balanced : mobile -> bool
	= fun mob ->
match mob with
|SimpleBranch(a,b), SimpleBranch(c,d) -> 
if (a<0)||(b<0)||(c<0)||(d<0) then raise NotImplemented 
else if  (a*b==c*d) then true else false


|SimpleBranch(a,b), CompoundBranch(c,m) ->
if (a<0)||(b<0)||(c<0) then raise NotImplemented 
else if ((sum m)*c == a*b)&&(balanced m) then true else false

|CompoundBranch(c,m), SimpleBranch (a,b) ->
if (a<0)||(b<0)||(c<0) then raise NotImplemented
else if ((sum m)*c == a*b)&&(balanced m) then true else false

|CompoundBranch(a,m1), CompoundBranch(b,m2) ->
if (a<0)||(b<0) then raise NotImplemented 
else if ((sum m1)*a == (sum m2)*b)&&(balanced m1)&&(balanced m2) then true
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

let rec sigma n exp =
match exp with
|INT i -> i
|X -> n
|ADD (e1,e2) -> (sigma n e1) + (sigma n e2)
|MUL (e1,e2) -> (sigma n e1) * (sigma n e2)
|DIV (e1,e2) -> (sigma n e1) / (sigma n e2)
|SUB (e1,e2) -> (sigma n e1) - (sigma n e2)
|_ -> raise NotImplemented

let rec calculator : exp -> int
  = fun exp ->
match exp with
|X -> raise NotImplemented
|INT n -> n
|ADD (e1, e2) -> (calculator e1) + (calculator e2)
|SUB (e1, e2) -> (calculator e1) - (calculator e2)
|MUL (e1, e2) -> (calculator e1)*(calculator e2)
|DIV (e1, e2) -> (calculator e1)/(calculator e2)
|SIGMA (e1, e2, e3) ->
if ((calculator e1) > (calculator e2)) then raise NotImplemented
else if ((calculator e1) == (calculator e2)) then sigma (calculator e2) e3
else sigma (calculator e1) e3 + calculator (SIGMA ((ADD(e1, INT 1), e2, e3)))
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

let rec str exp
=match exp with
|V a -> []
|P (a, e1) -> a::(str e1 )
|C (e1,e2) -> (str e1)@(str e2)

let rec v_str exp
=match exp with
|V a -> [a]
|P (a, e1) -> (v_str e1)
|C (e1, e2) -> (v_str e1)@(v_str e2)

let rec search var exp
=match exp with
|[] -> false
|hd::tl -> if hd = var then true else search var tl

let rec compare e1 e2
=match e2 with
|[] -> true
|hd::tl -> if (search hd e1)&&(compare e1 tl) then true else false




let check : exp -> bool
  = fun exp -> 
compare (str exp) (v_str exp)


end
