exception Error of string

type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (str : string)) : aexp =
  let rec diff_times (orihd : aexp) (li : aexp list) (s : string) (n : int) :
      aexp =
    match li with
    | hd :: tl ->
        if hd = orihd && n = 1 then Const 0
        else
          Sum
            (List.append
               [ Times (diff (hd, s) :: tl) ]
               [ diff_times orihd (List.append tl [ hd ]) s 1 ])
  in

  match aexp with
  | Const _ -> Const 0
  | Var s -> if s = str then Const 1 else Const 0
  | Power (s, i) ->
      if s = str then Times [ Const i; Power (s, i - 1) ] else Const 0
  | Times l ->
      if l = [] then raise Error "no times!" else diff_times (List.hd l) l str 0
  | Sum hd :: tl ->
      if tl = [] then diff (hd, str)
      else Sum (List.append [ diff (hd, str) ] [ diff (Sum tl, str) ])
