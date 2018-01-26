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
    | Const n -> Const 0
    | Var str -> if str = var then Const 1 else Const 0
    | Power (str, n) ->
      (if str = var then
        if n = 0 then Const 0
        else Times [Const n; Power (str, n-1)]
      else Const 0)
    | Times lst ->
      (match lst with
      | [] -> Const 0
      | _ -> Sum (product [] lst var))
    | Sum lst ->
      (match lst with
      | [] -> Const 0
      | _ -> Sum (add [] lst var))

  and product : aexp list -> aexp list -> string -> aexp list
  = fun lst1 lst2 var ->
    match lst2 with
    | [] -> []
    | h::t -> Times (lst1 @ [diff (h, var)] @ t)::product (lst1 @ [h]) t var

  and add : aexp list -> aexp list -> string -> aexp list
  = fun lst1 lst2 var ->
    match lst2 with
    | [] -> []
    | h::t -> diff (h, var)::add (lst1 @ [h]) t var
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
  = fun mob ->
    let (br1, br2) = mob in
    let w1 =
      (match br1 with
      | SimpleBranch (len, wgh) -> wgh
      | CompoundBranch (len, m) -> weight m) in
    let w2 =
      (match br2 with
      | SimpleBranch (len, wgh) -> wgh
      | CompoundBranch (len, m) -> weight m) in
      w1 + w2

  let rec torque : branch -> int
  = fun br ->
    match br with
    | SimpleBranch (len, wgh) -> len * wgh
    | CompoundBranch (len, mob) -> len * weight mob

  let rec balanced : mobile -> bool
  = fun mob ->
    let (br1, br2) = mob in
    let toq1 = torque br1 in
    let toq2 = torque br2 in
    let chk1 =
      (match br1 with
      | SimpleBranch (_, _) -> true
      | CompoundBranch (_, m) -> balanced m) in
    let chk2 =
      (match br2 with
      | SimpleBranch (_, _) -> true
      | CompoundBranch (_, m) -> balanced m) in
      if chk1 then
        if chk2 then
          if toq1 = toq2 then true else false
        else false
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

  let calculator : exp -> int
  = fun exp -> raise NotImplemented  (* TODO *)
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
  = fun exp -> raise NotImplemented (* TODO *)
end

