type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map f l x =
  match l with [] -> [] | hd :: tl -> f (hd, x) :: map f tl x


let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, n) ->
      if s = x then Times ([ Const n ] @ [ Power (s, n - 1) ]) else Const 0
  | Times [] -> Times []
  | [ Times hd; tl ] ->
      Sum
        ( [ Times ([ diff (hd, x) ] @ [ tl ]) ]
        @ [ Times ([ hd ] @ [ diff (tl, x) ]) ] )
  | Times hd :: tl ->
      Sum
        ( [ Times ([ diff (hd, x) ] @ tl) ]
        @ [ Times ([ hd ] @ [ diff (Times tl, x) ]) ] )
  | Sum lst -> Sum (map diff lst x)
