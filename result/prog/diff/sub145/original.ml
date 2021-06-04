type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const c -> Const 0
  | Var v -> if v = x then Const 1 else Var v
  | Power (str, po) -> (
      match str with x -> Times [ Const po; Power (str, po - 1) ] )
  | Times l -> (
      match l with
      | [] -> raise Failure "wrong format"
      | hd :: tl ->
          let inner_hd : aexp =
            match tl with hd2 :: tl2 -> hd2 | [] -> Const 0
          in
          Sum
            [
              Times [ diff (hd, x); inner_hd ]; Times [ hd; diff (inner_hd, x) ];
            ] )
  | Sum l -> (
      match l with
      | [] -> raise Failure "wrong format"
      | hd :: tl ->
          let inner_hd : aexp =
            match tl with hd2 :: tl2 -> hd2 | [] -> Const 0
          in
          Sum [ diff (hd, x); diff (inner_hd, x) ] )
