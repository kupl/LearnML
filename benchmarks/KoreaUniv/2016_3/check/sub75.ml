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
    | Const(a) -> Const(0)
    | Var(a) ->
        if a=var then Const 1
        else Const(0)
    | Power(a,b) ->
        if a=var then (
          if b = 1 then Times([Const b; Var a])
          else if b = 0 then Const 0
          else Times([Const b; Power( a , b-1 )])
        )
        else Const(0)
    | Times(li) ->
        (
          match li with
          | [] -> Times([])
          | hd::tl ->
              if tl = [] then diff (hd,var)
              else if hd = Const 0 then Const 0
              else
                let h = diff (hd,var) 
                  in let t = diff ((Times tl),var)
                    in (
                      if h = Const 0 then Times ([hd ; t])
                      else if t = Const 0 then Times (h::tl)
                      else
                        Sum ([Times (h::tl) ; Times ([hd ; t])])
                   )
        )

    | Sum(li) ->
        (
          match li with
          | [] -> Sum([])
          | hd::tl ->
              if tl = [] then diff (hd,var)
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

  let rec wint : branch -> int
  = fun bra ->
      match bra with
        | SimpleBranch(a,b) -> b
        | CompoundBranch(a,b) -> 
            match b with
                |(c,d) -> wint c + wint d 

  let tint : branch -> int
  = fun bra ->
      match bra with
        | SimpleBranch(a,b) -> a*b
        | CompoundBranch(a,b) -> a*wint(bra)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
      match mob with
      | (a,b) ->
          if tint(a) = tint(b) then
              match mob with
              | (SimpleBranch(a,b),SimpleBranch(c,d)) -> true
              | (SimpleBranch(a,b),CompoundBranch(c,d)) -> balanced(d)
              | (CompoundBranch(a,b),SimpleBranch(c,d)) -> balanced(b)
              | (CompoundBranch(a,b),CompoundBranch(c,d)) -> balanced(d) && balanced(b)
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

  let rec cal : exp * int -> int
  = fun (exp,num) ->
    match exp with
    | X -> num
    | INT(a) -> a
    | ADD (a,b) -> cal (a,num) + cal (b,num)
    | SUB (a,b) -> cal (a,num) - cal (b,num)
    | MUL (a,b) -> cal (a,num) * cal (b,num)
    | DIV (a,b) -> cal (a,num) / cal (b,num)
    | SIGMA(a,b,c) ->
      if cal (a,num) > cal (b,num) then 0
      else
        cal (c, cal(a,num))+ cal (( SIGMA (INT(cal (a,num) + 1),b,c) ) , num)

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

  let rec lcheck : exp * string list -> bool
  = fun (exp,list) ->
  match exp with
  | V (a) ->(
    match list with
    | [] -> false
    | hd::tl ->
      if hd = a then true
      else lcheck (V a, tl)
  )
  | P (a,b) ->
    lcheck (b, a::list)
  | C (a,b) ->
    lcheck (a, list) && lcheck (b, list)

  let check : exp -> bool
  = fun exp -> (* TODO *)
  lcheck (exp , [])
end

