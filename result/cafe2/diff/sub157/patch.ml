type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec search (lst : 'a list) a : bool =
  match lst with
  | [] -> true
  | hd :: tl -> if hd = a then false else search tl a


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var variable -> if variable = x then Const 1 else Const 0
  | Power (str, n) ->
      if str = x then Times [ Const n; Power (str, n - 1) ] else Const 0
  | Times lst -> (
      let rec f (l1 : aexp list) (l2 : aexp list) (a : int) : aexp =
        match l1 with
        | [] ->
            if a = 1 then Times (Const a :: l2)
            else Times (Const a :: Power (x, a - 1) :: l2)
        | hd :: tl -> (
            match hd with
            | Var x -> f tl (diff (hd, x) :: l2) (a + 1)
            | Const n -> f tl (hd :: l2) a
            | _ -> f tl (diff (hd, x) :: l2) a )
      in

      match lst with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
  | Sum lst ->
      let rec f (l1 : aexp list) (l2 : aexp list) : aexp =
        match l1 with [] -> Sum l2 | hd :: tl -> f tl (diff (hd, x) :: l2)
      in
      f lst []