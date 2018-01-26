(* problem 3 *)
type formula = 
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool = fun f ->
  let rec isinlist : string list -> string -> bool = fun strlist v ->
    (match strlist with
     | [] -> false
     | hd::tl -> if hd = v then true else isinlist tl v
    )
  in
  let rec isinlist2 : (string * bool) list -> string -> bool = fun table v ->
    (match table with
     | [] -> false
     | (str, b)::tl -> if str = v then b else isinlist2 tl v
    )
  in
  let rec find_vals : formula -> string list = fun form ->
    (match form with
    | True| False -> []
    | Var v -> [v]
    | Neg v -> find_vals v
    | And (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Or (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Imply (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Iff (v1, v2) -> (find_vals v1)@(find_vals v2)
    )
  in
  let rec edditlist : string list -> string list = fun strlist ->
    (match strlist with
     | [] -> []
     | hd::tl -> if isinlist tl hd then tl else hd::(edditlist tl)
    )
  in
  let rec calform : formula -> (string * bool) list -> bool = fun form table ->
    (match form with
     | True -> true
     | False -> false
     | Var v -> isinlist2 table v
     | Neg v -> not (calform v table)
     | And (v1, v2) -> (calform v1 table) && (calform v2 table)
     | Or (v1, v2) -> (calform v1 table) || (calform v2 table)
     | Imply (v1, v2) -> (not (calform v1 table)) || (calform v2 table)
     | Iff (v1, v2) -> let b1 = calform v1 table in let b2 = calform v2 table in (b1 && b2) || ((not b1) && (not b2))
    )
  in
  let rec calall : formula -> string list -> (string * bool) list -> bool = fun form strlist table ->
    (match strlist with
    | [] -> calform form table
    | hd::tl -> 
      if calall form tl ((hd, true)::table) then true
      else if calall form tl ((hd, false)::table) then true else false
    )
  in
  let vallist = edditlist (find_vals f) in
  calall f vallist []
