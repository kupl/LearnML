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
    | Const i -> Const 0
    | Var x ->
      if x = var then Const 1
      else Var x
    | Power(x,i) ->
      if x = var then Times[Const(i);Power(x,i-1)]
      else Power(x,i)
    | Sum(hd::tl) -> 
      begin
        match tl with
         | [] -> Sum[diff(hd,var)]
         | _ -> concat (Sum[diff(hd,var)]) (diff(Sum(tl),var))
      end
    | Times l -> asdf (l,var) 0
  and asdf (l,var) i =
    if i = ((lenght l) - 1) then Sum[diffTimes (l,var) 0 i]
    else concat (Sum[diffTimes (l,var) 0 i]) (asdf (l,var) (i + 1))
  and diffTimes ((hd::tl),var) now_i target_i = 
    if now_i = target_i then Times(diff(hd,var)::tl)
    else concat (Times[hd]) (diffTimes (tl,var) (now_i + 1) target_i)
  and lenght l =
    match l with
    | [] -> 0
    | hd::tl -> 1 + lenght tl
  and concat s1 s2 = 
    match s1 with
    | Sum l1 -> 
      begin
        match s2 with
        | Sum l2 -> Sum(l1@l2)
      end
    | Times l1 ->
      begin
        match s2 with
        | Times l2 -> Times(l1@l2)
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

  let rec balanced : mobile -> bool
  = fun mob -> 
    let rec weightOfMobile (b1, b2) = 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        begin
        match b2 with
        | SimpleBranch(l2,w2) -> w1 + w2
        | CompoundBranch(l2,m2) -> w1 + weightOfMobile m2
        end
      | CompoundBranch(l1,m1) -> 
        begin
        match b2 with
        | SimpleBranch(l2,w2) -> weightOfMobile m1 + w2
        | CompoundBranch(l2,m2) -> weightOfMobile m1 + weightOfMobile m2
        end
      in match mob with
        | (b1,b2) ->
          match b1 with
            | SimpleBranch(l1,w1) -> 
              begin
              match b2 with
              | SimpleBranch(l2,w2) -> (l1 * w2) = (l2 * w2)
              | CompoundBranch(l2,m2) -> 
                begin
                if(balanced m2) then (l1 * w1) = (l2 * weightOfMobile m2)
                else false
                end
              end
            | CompoundBranch(l1,m1) -> 
              begin
              if(balanced m1) then
                match b2 with
                | SimpleBranch(l2,w2) -> (l1 * weightOfMobile m1) = (l2 * w2)
                | CompoundBranch(l2,m2) -> 
                  begin
                  if(balanced m2) then (l1 * weightOfMobile m1) = (l2 * weightOfMobile m2)
                  else false
                  end
              else false
              end
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
  = fun exp -> 
      let rec substitute : exp -> int -> int
      = fun exp n ->
        match exp with
        | X -> n
        | INT i -> i
        | ADD(e1,e2) -> substitute e1 n + substitute e2 n
        | SUB(e1,e2) -> substitute e1 n - substitute e2 n
        | MUL(e1,e2) -> substitute e1 n * substitute e2 n
        | DIV(e1,e2) -> substitute e1 n / substitute e2 n
        | _ -> raise (Failure "can not substitute SIGMA")
    in match exp with
    | X -> raise (Failure "can not calculator")
    | INT i -> i
    | ADD(e1,e2) -> calculator e1 + calculator e2
    | SUB(e1,e2) -> calculator e1 - calculator e2
    | MUL(e1,e2) -> calculator e1 * calculator e2
    | DIV(e1,e2) -> calculator e1 / calculator e2
    | SIGMA(e1,e2,e3) -> 
      if calculator e1 > calculator e2 then 0
      else substitute e3 (calculator e1) + calculator(SIGMA(INT((calculator e1) + 1),e2,e3))
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
  = fun exp -> 
    let rec removeMatch : (var list) -> var -> (var list)
    = fun l v ->
      match l with
      | [] -> []
      | hd::tl -> 
        if hd = v then removeMatch tl v
        else hd::(removeMatch tl v)
    and eval : exp -> (var list)
    = fun exp ->
      match exp with
      | V v -> [v]
      | P(v,e) -> removeMatch (eval e) v
      | C(e1,e2) -> (eval e1)@(eval e2)
    in if (eval exp) = [] then true
       else false 
end

