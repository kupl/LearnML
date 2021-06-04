type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  let rec t2 (vl : aexp list) (var : string) : aexp list =
    match vl with
    | [] -> []
    | hd :: tl -> [ Times ([ diff (hd, var) ] @ tl); Times (hd :: t2 tl var) ]
  in

  let rec t (el : aexp list) (c : int) (vl : aexp list) (var : string) : aexp =
    match el with
    | [] -> Times [ Const c; Sum (t2 vl var) ]
    | hd :: tl -> (
        match hd with
        | Const x -> t tl (c * x) vl var
        | a -> t tl c (vl @ [ a ]) var )
  in

  let rec s (el : aexp list) (l : aexp list) (var : string) : aexp =
    match el with [] -> Sum l | hd :: tl -> s tl (l @ [ diff (hd, var) ]) var
  in

  match exp with
  | Const x -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, y) ->
      if x = var then
        if y = 0 then Const 0
        else if y = 1 then diff (Var x, var)
        else if y = 2 then Times [ Const 2; Var x ]
        else Times [ Const y; Power (x, y - 1) ]
      else Const 0
  | Times l -> t l 1 [] var
  | Sum l -> s l [] var
