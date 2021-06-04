type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Const a -> Const 0
  | Var v -> Const 1
  | Power (v, a) ->
      if v = x then Times [ Const a; Power (v, a - 1) ] else Const 0
  | Times l ->
      let rec iter : aexp list -> aexp list -> aexp list -> aexp list =
       fun l1 l2 sum ->
        match l1 with
        | [] -> sum
        | hd :: tl ->
            iter tl (hd :: l2) (Times ((diff (hd, x) :: tl) @ l2) :: sum)
      in
      Sum (iter l [] [])
  | Sum l ->
      let rec iter l res =
        match l with [] -> res | hd :: tl -> iter tl (diff (hd, x) :: res)
      in
      Sum (iter l [])
