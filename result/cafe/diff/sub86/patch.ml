type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const x -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, y) ->
      if x = var then Times [ Const y; Power (x, y - 1) ] else Const 0
  | Times x -> (
      match x with
      | [] -> Const 0
      | [ __s68 ] -> diff (__s68, var)
      | __s69 :: __s70 ->
          Sum
            [
              Times (diff (__s69, var) :: __s70);
              Times [ __s69; diff (Times __s70, var) ];
            ] )
  | Sum x ->
      let rec diff1 (l : aexp list) : aexp list =
        match l with [] -> [] | hd :: tl -> diff (hd, var) :: diff1 tl
      in
      Sum (diff1 x)
