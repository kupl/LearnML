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
  = fun (exp, var) -> (* TODO *)
  
    match exp with
    Const sth -> Const 0
    |Var sth -> if (sth = var) then (Const 1) else (Const 0)
    |Power (sth, n) -> if (sth <> var) then (Const 0)
                       else Times [Const n; Power (sth, (n-1))] 
    |Times sth ->
			(
      match sth with
      [] -> Const 0
      |hd::tl -> Sum [ Times ( (diff (hd, var))::tl ) ;
								 Times [ hd ; diff ((Times tl), var) ] ]
   		)
    |Sum sth ->
       let rec map f l =
       (
       match l with
       [] -> []
       |hd::tl -> (f hd)::(map f tl)
       )
       in
			 (
       match sth with
       [] -> Const 0
       |_ -> Sum ( map (fun element -> diff (element, var)) sth )
       )  
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
  = fun mob -> (* TODO *)
  
  let rec cal = fun mob ->
  match mob with
  |(left,right) ->
    (
    match left with
    |SimpleBranch (ll,lw) ->		
      (
      match right with
      |SimpleBranch (rl,rw) -> (lw+rw) 
      |CompoundBranch (rcl,rcm) -> (lw  + (cal rcm))
      )
    |CompoundBranch (lcl,lcm) ->
      (
      match right with
      |SimpleBranch (rl,rw) -> ((cal lcm) + rw)
      |CompoundBranch (rcl,rcm) -> ((cal lcm) + (cal rcm))
      )
    )
  in
  match mob with
  |(left,right)->
  (
    match left with
    |SimpleBranch (ll,lw) ->
    (
      match right with
      |SimpleBranch(rl,rw)->
        if rl*rw= ll*lw then true else false
      |CompoundBranch(rcl,rcm)->
        if balanced(rcm) then
          (if rcl*cal(rcm)= ll*lw then true else false)
        else false
    )
    |CompoundBranch(lcl,lcm)->
    if balanced(lcm) then
      (
      match right with
      |SimpleBranch(rl,rw)->
        if ( lcl*cal(lcm) = rl*rw ) then true else false
      |CompoundBranch(rcl,rcm)->
        if ( lcl*cal(lcm) = rcl*cal(rcm) ) then true else false
      )
    else false
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

  let calculator : exp -> int
  = fun exp -> (* TODO *)
  
  let rec map f l =
    (
    match l with
    |[] -> []
    |hd::tl -> (f hd)::(map f tl)
    )
  in
  
  let rec fold f l a =
    (
    match l with
    |[] -> a
    |hd::tl -> f hd (fold f tl a)
    )
  in
  
  let sumlist list = fold (fun x y -> x+y) list 0
  in
  
  let rec makelist startp endp =
    if (startp=endp) then [startp]
    else startp::( makelist (startp+1) endp )  
  in     
  
  let rec xcalculator = fun exp element ->
    (
    match exp with
    |X ->
      (
      match element with
      |[] -> raise (Failure "Must Use Value!")
      |hd::tl -> hd
      )
    |INT n -> n  
    |ADD (exp1, exp2) -> (xcalculator exp1 element) + (xcalculator exp2 element)
    |SUB (exp1, exp2) -> (xcalculator exp1 element) - (xcalculator exp2 element)
    |MUL (exp1, exp2) -> (xcalculator exp1 element) * (xcalculator exp2 element)
    |DIV (exp1, exp2) -> (xcalculator exp1 element) / (xcalculator exp2 element)  
    |SIGMA (exp1, exp2, exp3) ->
      let spoint = (xcalculator exp1 element) in
      let epoint = (xcalculator exp2 element) in
      let mylist = makelist spoint epoint in
      
      sumlist ( (map (fun element -> (xcalculator exp3 [element]) ) mylist) )
    ) 
  in  
  
  xcalculator exp []
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
  = fun exp -> (* TODO *)
  
  let rec checkvar exp mylist=
  match exp with 
  |V(var) -> if List.mem var mylist then true else false
  |P(var, myexp) -> checkvar myexp (mylist@[var])
  |C(exp1,exp2) -> (checkvar exp1 mylist) && (checkvar exp2 mylist)
	in
	checkvar exp [] 
end

