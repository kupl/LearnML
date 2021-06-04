type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (str : string)) : aexp =
  let rec isThereStr (aexp : aexp) (str : string) : bool =
    match aexp with
    | Const i -> false
    | Var s -> s = str
    | Power (s, i) -> s = str
    | Times l -> (
        match l with
        | [] -> false
        | h :: t -> isThereStr h str || isThereStr (Times t) str )
    | Sum l -> (
        match l with
        | [] -> false
        | h :: t -> isThereStr h str || isThereStr (Sum t) str )
  in

  let rec mul (al : aexp list) (str : string) : aexp list =
    match al with
    | [] -> []
    | [ h ] -> if isThereStr h str then [ diff (h, str) ] else [ h ]
    | h :: t ->
        if isThereStr h str then List.append [ diff (h, str) ] (mul t str)
        else List.append [ h ] (mul t str)
  in

  match aexp with
  | Const i -> Const 0
  | Var s -> if s = str then Const 1 else Const 0
  | Power (s, i) ->
      if s = str then
        if i > 1 then Times [ Const i; Power (s, i - 1) ] else Const 1
      else Const 0
  | Times l -> if isThereStr aexp str then Times (mul l str) else Const 0
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ h ] -> diff (h, str)
      | h :: t -> Sum (List.append [ diff (h, str) ] [ diff (Sum t, str) ]) )
