type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diffTimesList (l : aexp list) (x : string) : aexp list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const c -> if c = 0 then [ Const 0 ] else hd :: diffTimesList tl x
      | Var v -> if v = x then diffTimesList tl x else [ Const 0 ]
      | Power (v, p) ->
          if v = x then [ Const p; Power (v, p - 1) ] @ diffTimesList tl x
          else hd :: diffTimesList tl x
      | Times l2 -> diffTimesList l2 x @ diffTimesList tl x )


let rec diffSumList (l : aexp list) (x : string) : aexp list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const c -> diffSumList tl x
      | Var v -> if v = x then Const 1 :: diffSumList tl x else diffSumList tl x
      | Power (v, p) ->
          if v = x then Times [ Const p; Power (v, p - 1) ] :: diffSumList tl x
          else diffSumList tl x
      | Times l2 -> Times (diffTimesList l2 x) :: diffSumList tl x
      | Sum l2 -> diffSumList l2 x @ diffSumList tl x )


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const c -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, p) -> Times [ Const p; Power (v, p - 1) ]
  | Times l -> Times (diffTimesList l x)
  | Sum l -> Sum (diffSumList l x)
