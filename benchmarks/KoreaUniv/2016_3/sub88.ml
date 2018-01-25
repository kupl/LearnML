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

  let rec simp : aexp -> aexp
  = fun (exp) ->
      match exp with
      Const n -> Const n
      | Var x -> Var x
      | Power (s,i) -> 
          (match i with 
            0 -> Const 1
            | 1 -> Var s
            | _ -> Power(s,i))
      | Times expli ->
          (match expli with
            [] -> Const 1
            | h::[] -> simp(h)
            | h::t -> match h with Const 0 -> Const 0
            					| Const 1 -> Times(t)
                                | _ -> Times(h:: t))
            (* h: aexp, t: aexp list *)
      | Sum (expli) ->
          (match expli with
            [] -> Const 0
            | h::[] -> simp(h)
            | h::t -> match h with Const 0 -> Sum t
                                    | _ -> Sum(h:: t))
              (* h: aexp, t: aexp list *)

 
  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
    Const n -> Const 0
    | Var x -> if x=var then Const 1 else Const 0
    | Power (s,n) ->
      if s=var then (match n with 0 -> Const 0
                                | 1 -> Const 1
                                | _ -> (Times [Const n; Power(s, n-1)]))
      else Const 0
    | Times li ->   (* li// aexp list *)
        (match li with [] -> Const 1
                       | h::[] -> diff(h, var)
                       | h::t -> (match h with Const n -> simp(Times [Const n; diff(Times t,var)])
                                              (* | _ -> (Sum [Times (diff(h,var)::t); Times [h; diff(Times t,var)]])) *)
                                              | _ -> simp(Sum [Times (diff(h,var)::t); Times [h; diff(Times t,var)]])))
    | Sum li ->
          (match li with
            [] -> Const 0
            (* | h::t -> (Sum [diff(h,var); diff(Sum t, var)])) *)
            | h::t -> simp(Sum [diff(h,var); diff(Sum t, var)]))
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

  let rec brWeight
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> wgh
        | CompoundBranch(len, mob) -> 
            match mob with (lb, rb) -> brWeight(lb) + brWeight(rb)
  let rec brTorq
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> len*wgh
        | CompoundBranch(len, mob) -> 
            match mob with (lb, rb) -> len*(brWeight(lb) + brWeight(rb))

  let rec brBal
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> true
        | CompoundBranch(len, mob) ->
            match mob with (lb, rb) -> 
              if(brBal lb && brBal rb && (brTorq lb = brTorq rb)) then true else false
  let rec balanced : mobile -> bool
  = fun mob ->
      match mob with
          (lb, rb) -> 
            if (brBal lb && brBal rb && (brTorq lb = brTorq rb) ) then true else false
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
 
  let rec calSigma
  = fun (a, b, ope, sum) -> if a=b then sum+ ope a
                            else calSigma(a+1, b, ope, sum+(ope a))

  let rec makeOpe : exp -> (int -> int)
  = fun e ->
    match e with
      | X -> (fun x -> x)
      | INT n -> (fun x -> n)
      | ADD (e1,e2) -> (fun x -> (((makeOpe e1) x) + ((makeOpe e2) x)))
      | SUB (e1,e2) -> (fun x -> (((makeOpe e1) x) - ((makeOpe e2) x)))
      | MUL (e1,e2) -> (fun x -> (((makeOpe e1) x) * ((makeOpe e2) x)))
      | DIV (e1,e2) -> (fun x -> (((makeOpe e1) x) / ((makeOpe e2) x)))
      | _ -> (fun x -> 0);;

  let rec calculator : exp -> int
  = fun exp ->
    match exp with
      | X -> 0
      | INT n -> n
      | ADD(e1, e2) -> calculator(e1)+calculator(e2)
      | SUB(e1, e2) -> calculator(e1)-calculator(e2)
      | MUL(e1, e2) -> calculator(e1)*calculator(e2)
      | DIV(e1, e2) -> calculator(e1)/calculator(e2)
      | SIGMA(e1, e2, ope) -> calSigma(calculator(e1), calculator(e2), makeOpe ope, 0) 

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

  let rec chkList
    = fun (e,li) ->
        match e with
        | V var -> (match li with 
                      [] -> false
                      | h::t -> if (var = h) then true else chkList (V var, t) )
        | P (var,exp) -> chkList (exp, var::li)
        | C (exp1, exp2) -> if(chkList (exp1,li) && chkList (exp2,li)) then true else false
  let check : exp -> bool
  = fun exp -> chkList (exp, [])
  
end

