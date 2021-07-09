type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const int1 -> Const 0
  | Var x -> Const 1
  | Power (var, int1) -> Times [ Const int1; Power (var, int1 - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 1
      | hd :: tail ->
          if diff (hd, x) = Const 0 then Times [ hd; diff (Times tail, x) ]
          else if diff (hd, x) = Const 1 then Times (Const 1 :: Const 1 :: tail)
          else Times (Const 1 :: diff (hd, x) :: tail) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tail -> Sum [ diff (hd, x); diff (Sum tail, x) ] )
