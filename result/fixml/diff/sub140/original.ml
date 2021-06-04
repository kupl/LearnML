type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
  | Times l ->
      let rec difft : aexp list -> aexp list =
       fun l ->
        match l with
        | [] -> []
        | hd :: tl ->
            if diff (hd, x) = Const 0 then hd :: difft tl
            else diff (hd, x) :: difft tl
      in
      Times (difft l)
  | Sum l ->
      let rec diffs : aexp list -> aexp list =
       fun l -> match l with [] -> [] | hd :: tl -> diff (hd, x) :: diffs tl
      in
      Sum (diffs l)
