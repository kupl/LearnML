
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

type logical_env = (string * bool) list
let extend_logical_env (x, b) e = (x, b) :: e
let rec find_logical_env x e =
  match e with
  | [] -> raise (Failure "Runtime error!")
  | (y, b) :: tl -> if x = y then b else find_logical_env x tl

let rec extract_var : formula -> string list -> string list
= fun f v_list ->
  let rec append_dup l1 l2 =
    (match l1 with
     | [] -> l2
     | hd::tl -> if (List.mem hd l2) then append_dup tl l2
                 else append_dup tl (hd::l2))
  in
  match f with
  | True -> v_list
  | False -> v_list
  | Var (x) -> append_dup [x] v_list
  | Neg (f1) -> extract_var f1 v_list
  | And (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Or (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Imply (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Iff (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)

let rec sat_check_branch : string list -> (formula -> logical_env-> bool) -> formula -> logical_env -> bool
= fun vlist sat_fun f e->
  match vlist with
  | [] -> (sat_fun f e)
  | hd::tl -> (sat_check_branch tl sat_fun f (extend_logical_env (hd, true) e)) || (sat_check_branch tl sat_fun f (extend_logical_env (hd, false) e))


let rec sat_helper : formula -> logical_env -> bool
= fun f e ->
  match f with
  | True -> true
  | False -> false
  | Var (x) -> find_logical_env x e
  | Neg (f1) -> let b = sat_helper f1 e in (not b)
  | And (f1, f2) -> let b1 = sat_helper f1 e in
                    let b2 = sat_helper f2 e in
                    b1 && b2
  | Or (f1, f2) -> let b1 = sat_helper f1 e in
                   let b2 = sat_helper f2 e in
                   b1 || b2
  | Imply (f1, f2) -> sat_helper (Or (Neg (f1), f2)) e
  | Iff (f1, f2) -> sat_helper (And (Imply (f1, f2), Imply (f2, f1))) e

let sat : formula -> bool
= fun f ->
  let vlist = extract_var f [] in
  sat_check_branch vlist sat_helper f []

