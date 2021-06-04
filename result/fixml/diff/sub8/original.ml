type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (alexp, str) ->
  let zeroTimes : aexp -> aexp =
   fun a ->
    match a with
    | Times hd :: tl ->
        if hd = Const 0 || List.mem (Const 0) tl then Const 0 else a
    | _ -> a
  in

  match alexp with
  | Const c -> Const 0
  | Var s -> if s != str then Const 0 else Const 1
  | Power (s, n) ->
      if s != str then Const 0
      else if n = 0 then Const 1
      else if n = 1 then Const n
      else if n = 2 then Times [ Const n; Var s ]
      else Times [ Const n; Power (s, n - 1) ]
  | Times hd :: tl ->
      if tl = [] then diff (hd, str)
      else
        let ht = zeroTimes (Times (diff (hd, str) :: tl)) in

        let st = zeroTimes (Times [ hd; diff (Times tl, str) ]) in
        if ht = Const 0 && st = Const 0 then Const 0
        else if ht = Const 0 then st
        else if st = Const 0 then ht
        else Sum [ ht; st ]
  | Times [] -> Const 0
  | Sum hd :: tl ->
      if tl = [] then diff (hd, str)
      else
        let h = diff (hd, str) in

        let t = diff (Sum tl, str) in
        if h = Const 0 then t else if t = Const 0 then h else Sum [ h; t ]
  | Sum [] -> Const 0
