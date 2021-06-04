type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map (f : 'c * 'b -> 'a) (l : 'c list) (x : string) : 'a list =
  match l with [] -> [] | hd :: tl -> f (hd, x) :: map f tl x


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, n) ->
      if s = x then Times ([ Const n ] @ [ Power (s, n - 1) ]) else Const 0
  | Times [] -> Times []
  | [ Times __s61 ] -> diff (__s61, x)
  | [ Times hd; tl ] ->
      Sum
        ( [ Times ([ diff (hd, x) ] @ [ tl ]) ]
        @ [ Times ([ hd ] @ [ diff (tl, x) ]) ] )
  | Times hd :: tl ->
      Sum
        ( [ Times ([ diff (hd, x) ] @ tl) ]
        @ [ Times ([ hd ] @ [ diff (Times tl, x) ]) ] )
  | Sum lst -> Sum (map diff lst x)
