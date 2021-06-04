type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var y -> Var x
  | Power (x, b) ->
      if b = 2 then Times [ Const 2; Var x ]
      else Times [ Const b; Power (x, b - 1) ]
  | Times l -> (
      match l with
      | h :: t -> (
          match t with
          | [ Var x ] -> h
          | [ Power (a, b) ] -> Times [ h; diff (Power (a, b), a) ]
          | _ -> raise Failure "False" ) )
  | Sum k -> (
      match k with
      | hd :: tl ->
          let rec diffe a =
            match a with
            | h :: t -> (
                match t with
                | [] -> [ diff (h, x) ]
                | _ -> diff (h, x) :: diffe t )
          in
          Sum (diffe k) )


let _ = diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
