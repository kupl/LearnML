type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec find_ch : aexp list * string -> bool =
 fun (l, x) ->
  match l with
  | [] -> false
  | hd :: tl -> (
      match hd with
      | Var str -> if str = x then true else find_ch (tl, x)
      | Power (str, _) -> if str = x then true else find_ch (tl, x)
      | _ -> find_ch (tl, x) )


let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ hd ] -> diff (hd, x)
      | hd :: tl -> Sum ([ diff (hd, x) ] @ [ diff (Sum tl, x) ]) )
  | Times l ->
      if find_ch (l, x) then
        match l with
        | [] -> Const 0
        | hd :: tl -> (
            match hd with
            | Const n -> Times ([ Const n ] @ [ diff (Times tl, x) ])
            | Var v ->
                if v = x then
                  match tl with
                  | [] -> diff (Var v, x)
                  | _ -> diff (Times (tl @ [ Var v ]), x)
                else Times ([ Var v ] @ [ diff (Times tl, x) ])
            | Power (v, n) ->
                if v = x then
                  match tl with
                  | [] -> diff (Power (v, n), x)
                  | _ -> diff (Times (tl @ [ Power (v, n) ]), x)
                else Times ([ Power (v, n) ] @ [ diff (Times tl, x) ])
            | _ -> Times l )
      else Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, n) ->
      if v = x then
        match n with
        | 1 -> Const 1
        | 2 -> Times ([ Const 2 ] @ [ Var v ])
        | _ -> Times ([ Const n ] @ [ Power (v, n - 1) ])
      else Const 0
  | Const n -> Const 0
