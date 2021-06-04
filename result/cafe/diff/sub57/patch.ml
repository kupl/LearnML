type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, p) ->
      if v = x then
        match p with
        | 1 -> Const 1
        | 2 -> Times [ Const p; Var v ]
        | _ -> Times [ Const p; Power (v, p - 1) ]
      else Const 0
  | Times l1 ->
      let rec iter (l : aexp list) : aexp list =
        match l with
        | [] -> []
        | h :: t ->
            let rec isConst (l : aexp list) : bool =
              match l with
              | [] -> true
              | h :: t -> ( match h with Const c -> isConst t | _ -> false )
            in
            [ Times [ diff (h, x); Times t ]; Times [ h; diff (Times t, x) ] ]
      in
      Sum (iter l1)
  | Sum l2 ->
      let rec iter (l : aexp list) : aexp list =
        match l with
        | [] -> []
        | h :: t -> (
            match h with
            | Const 0 -> iter t
            | Times [ Const 0 ] -> iter t
            | _ -> diff (h, x) :: iter t )
      in
      Sum (iter l2)
