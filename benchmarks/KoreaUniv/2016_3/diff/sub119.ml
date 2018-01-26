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
    | Const n -> Const 0
    | Var n -> if n=var then Const 1
                else Const 0
    | Power(a,b) ->if a=var && b!=0 then Times([Const b; Power( a , b-1 )])
                   else Const 0
    | Times n ->( match n with
          | [] -> Times([])
          | hd::tl ->if tl = [] then diff (hd,var)
                    else Sum ([Times (diff (hd,var)::tl) ; Times ([hd ; diff ((Times tl),var)])])
        )

    | Sum n ->( match n with
          | [] -> Sum([])
          | hd::tl -> if tl = [] then diff (hd,var)
                      else Sum( [ diff (hd,var) ; diff ((Sum tl),var) ] ) 
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

  let rec w : branch -> int
  = fun br -> match br with
        | SimpleBranch(a,b) -> b
        | CompoundBranch(a,b) ->  match b with
                |(c,d) -> w c + w d 

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
        | (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*w(SimpleBranch(a,b)) = c*w(SimpleBranch(c,d)) then true else false
        | (SimpleBranch(a,b),CompoundBranch(c,d)) -> if a*w(SimpleBranch(a,b)) = c*w(CompoundBranch(c,d)) then balanced(d) else false
        | (CompoundBranch(a,b),SimpleBranch(c,d)) -> if a*w(CompoundBranch(a,b)) = c*w(SimpleBranch(c,d)) then balanced(b) else false
        | (CompoundBranch(a,b),CompoundBranch(c,d)) -> if a*w(CompoundBranch(a,b)) = c*w(CompoundBranch(c,d)) then balanced(d) && balanced(b) else false
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

  let rec cal : exp * int -> int
  = fun (exp,num) ->
    match exp with
    | X -> cal (INT(num), num)
    | INT(a) -> a
    | ADD (a,b) -> cal (a,num) + cal (b,num)
    | SUB (a,b) -> cal (a,num) - cal (b,num)
    | MUL (a,b) -> cal (a,num) * cal (b,num)
    | DIV (a,b) -> cal (a,num) / cal (b,num)
    | SIGMA(a,b,c) ->
      if cal (a,num) > cal (b,num) then 0
      else
        cal (( SIGMA (INT(cal (a,num) + 1),b,c) ) , num) + cal (c, cal(a,num))

  let rec calculator : exp -> int
  = fun exp -> (* TODO *)
    cal (exp,0)
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

  let rec ch : exp * exp list -> bool
  = fun (exp,list) -> match exp with
  | V (a) ->( match list with
        | [] -> false
        | hd::tl -> if hd = V a then true
                else if tl = [] then false
                else ch (V a, tl)
  )
  | P (a,b) -> ch (b, V a::list)
  | C (a,b) -> ch (a, list) && ch (b, list)

  let check : exp -> bool
  = fun exp -> ch (exp , [])
end

