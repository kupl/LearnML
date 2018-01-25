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
  match exp with
  |Const i -> Const 0
  |Var s -> if (s=var) then Const 1 else Const 0
  |Power(v, i) -> if (v=var) then Times [Const i; Times[Power(var,i-1)]] else Const 0
  |Times [] -> Const 0
  |Times (hd::tl) -> Sum [Times(diff (hd, var)::tl); Times[hd; diff(Times tl, var)]]
  |Sum [] -> Const 0
  |Sum lst         -> 
     begin match lst with
      |hd::[] -> diff(hd,var)
      |hd::tl -> Sum [diff(hd, var); diff(Sum tl, var)]
      |[]-> Const 0
    end

 
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

  let rec cal : branch -> int
  = fun br ->
  match br with
  |SimpleBranch(l, w) -> w 
  |CompoundBranch(l, (b1, b2)) -> (cal (b1))+ (cal (b2))


  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) ->
    if (l1*w1=l2*w2) then true else false
  |(CompoundBranch( l, m ) ,SimpleBranch(l1, w1) ) -> 
    if (l*(cal (CompoundBranch(l,m))) = l1*w1 ) then true&&(balanced m) else false
  |( SimpleBranch(l1, w1), CompoundBranch( l, m )) ->  
    if (l*(cal (CompoundBranch(l,m))) = l1*w1 ) then true&&(balanced m) else false
  | ( CompoundBranch( l1, m1 ), CompoundBranch( l2, m2 )) ->
    if ( l1*(cal (CompoundBranch(l1, m1))) = l2*(cal (CompoundBranch(l2, m2)))) then true&&((balanced m1)&&(balanced m2)) else false
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

  let rec sigma : int -> int -> exp -> int
    =  fun s e f ->

    let rec calc : exp -> int -> int 
    =fun exp i ->
    match exp with
    | X -> i
    |INT n -> n
    |ADD (e1, e2) -> (calc e1 i) + (calc e2 i)
    |SUB (e1, e2) -> (calc e1 i) - (calc e2 i)
    |MUL (e1, e2) -> (calc e1 i) * (calc e2 i)
    |DIV (e1, e2) -> (calc e1 i) / (calc e2 i)
    |SIGMA (e1, e2, e3) -> (sigma (calc e1 i) (calc e2 i) e3) in
    if (s<=e) then ( sigma (s+1) e f )+( calc f s)
    else  0
    
  let rec calculator : exp -> int
  = fun exp ->
  match exp with
  |INT n -> n
  |ADD (e1, e2) -> (calculator (e1)) + (calculator (e2))
  |SUB (e1, e2) -> (calculator (e1)) - (calculator (e2))
  |MUL (e1, e2) -> (calculator (e1)) * (calculator (e2))
  |DIV (e1, e2) -> (calculator (e1)) / (calculator (e2))
  |SIGMA (e1, e2, e3) ->  sigma (calculator (e1)) (calculator (e2)) e3
  |_ -> raise NotImplemented

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
  |P(s, V a)                -> if (s= a) then true else false
  |P(s, P(s1, e1))          -> 
    begin match e1 with 
        |C(e2, e3)->   ( ( check(P(s, e2)) ||check (P(s1, e2)) )&&( check (P(s, e3))||check (P(s1, e3)) ) )
        |_        ->   if ((check (P(s1,e1)) =true)||(check (P(s,e1)))) then true else false
      end
  |P(s, C(e1, e2))          ->
    begin match (e1, e2) with 
      |(P(s1, e3), e)    -> if (check (P(s, P(s1,e3))) &&check(P(s, P(s1,e)))) then true else false
      |(e, P(s1, e3))    -> if (check (P(s, P(s1,e3))) &&check(P(s, P(s1,e)))) then true else false
      |_                 -> if (check (P(s, e1))&& check(P(s,e2)))             then true else false
      end 
  |_ -> false 

end

