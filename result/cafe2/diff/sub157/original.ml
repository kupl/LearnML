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
  | Var variable -> if variable = x then Const 1 else Var variable
  | Power (str, n) ->
      if n = 0 then Const 0
      else if n = 1 then Const 1
      else Times [ Const n; Power (str, n - 1) ]
  | Times lst ->
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
      f lst [] 0
  | Sum lst ->
      let rec f (l1 : aexp list) (l2 : aexp list) : aexp =
        match l1 with [] -> Sum l2 | hd :: tl -> f tl (diff (hd, x) :: l2)
      in
      f lst []