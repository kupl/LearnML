(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
  let extract_var = fun f ->
    let rec append = fun x y ->
      match x with
      | [] -> y
      | hd :: rest -> hd :: (append rest y)
    in
    let rec extract_loop = fun f ->
      match f with
      | Var x -> [(x, false)]
      | Neg f' -> (extract_loop f')
      | And (x, y) -> append (extract_loop x) (extract_loop y)
      | Or (x, y) -> append (extract_loop x) (extract_loop y)
      | Imply (x, y) -> append (extract_loop x) (extract_loop y)
      | Iff (x, y) -> append (extract_loop x) (extract_loop y)
      | _ -> [("", false)]
    in
    let rec pack = fun vars acc ->
      let rec find = fun v l ->
        match l with
        | [] -> false
        | ((var, _) :: rest) ->
          if var = v then
            true
          else
            find v rest
      in
      match vars with
      | [] -> acc
      | (var, _) as hd :: rest ->
        if var != "" && find var acc = false then
          pack rest (hd :: acc)
        else
          pack rest acc
    in
    pack (extract_loop f) []
  in
  let rec lookup = fun x vars ->
    match vars with
    | [] -> raise (Failure ("Undefined variable: " ^ x))
    | ((var, value) :: rest) ->
      if var = x then
        value
      else
        lookup x rest
  in
  let rec eval = fun vars e ->
    match e with
    | True -> true
    | False -> false
    | Var x -> lookup x vars
    | Neg f -> not (eval vars f)
    | And (x, y) -> (eval vars x) && (eval vars y)
    | Or (x, y) -> (eval vars x) || (eval vars y)
    | Imply (x, y) ->
      (match ((eval vars x), (eval vars y)) with
      | (false, _) -> true
      | (true, true) -> true
      | (true, false) -> false)
    | Iff (x, y) -> eval vars (And(Imply (x, y), Imply (y, x)))
  in
  let rec next = fun l ->
    match l with
    | [] -> []
    | (var, value) :: rest ->
      if value = false then
          (var, true) :: rest
      else
          (var, false) :: next rest
  in
  let rec is_last = fun l ->
    match l with
    | [] -> true
    | (_, value) :: rest ->
      if value = false then
        is_last rest
      else
        false
  in
  let rec solve = fun vars f ->
    if eval vars f = true then
      true
    else
      let next_vars = next vars
      in
        if is_last next_vars then
          false
        else
          solve next_vars f
  in
  solve (extract_var f) f
