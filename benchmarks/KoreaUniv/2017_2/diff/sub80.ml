(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
let rec impl _e =
  match _e with
  | Const (_) -> Const (0)
  | Var (y) ->
  if x = y then Const (1)
  else Const (0)
  | Power (y, n) ->
  if x = y then
    if n = 0 then Const (0)
    else
      if n = 1 then Const (1)
      else
        if n = 2 then Times [Const (2); Var (y)]
        else Times ([Const (n); Power (y, (n - 1))])
  else Const (0)
  | Times (l) ->
  let tmp =
    match l with
    | [] -> Const (0)
    | hd::tl ->
    let res = impl hd in
    let diff_tl_part =
      if hd = (Const (1)) then impl (Times (tl))
      else Times ([hd; (impl (Times (tl)))]) in
    if res = (Const (0)) then diff_tl_part
    else
      if diff_tl_part = (Const (0)) then Times ((impl hd)::tl)
      else Sum ([(Times ((impl hd)::tl)); diff_tl_part]) in
  tmp
  | Sum (l) ->
  let tmp =
    match l with
    | [] -> Const (0)
    | hd::tl ->
    let res = impl hd in
    if res = (Const (0)) then impl (Sum (tl))
    else Sum ([(impl hd); (impl (Sum (tl)))]) in
  tmp in
impl e;;