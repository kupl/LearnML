type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x && b = 1 then Const 1
      else if a = x then Times [ Const b; Power (a, b - 1) ]
      else Const 0
  | Times a -> (
      match a with
      | [] -> Const 0
      | __s69 :: __s70 ->
          Sum
            [
              Times (diff (__s69, x) :: __s70);
              Times [ __s69; diff (Times __s70, x) ];
            ] )
  | Sum a ->
      let rec help_diff (e1 : aexp list) : aexp list =
        match e1 with [] -> [] | hd :: tl -> diff (hd, x) :: help_diff tl
      in
      Sum (help_diff a)
