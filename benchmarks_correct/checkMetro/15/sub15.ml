(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-3 *)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec isInGroup: var * var list -> bool = fun(n, l) ->
  match l with
  | [] -> false
  | hd::tl -> if(hd = n) then true else isInGroup(n, tl) 

let rec checkInGroup: lambda * var list -> bool = fun(m, l) ->
  match m with
  | V n -> isInGroup(n, l)
  | P (n, m1) -> checkInGroup(m1, n::l)
  | C (m1, m2) ->
      if(checkInGroup(m1, l)
      && checkInGroup(m2, l)) then true else false

let rec check: lambda -> bool = fun(m) ->
  checkInGroup(m, [])

(*
let _= 
  let print_bool x = print_endline (string_of_bool x) in 

  let a81 = check (P("a", V "a")) in 
  let a82 = check (P("a", P("a", V "a"))) in 
  let a83 = check (P("a", P("b", C(V "a", V "b")))) in 
  let a84 = check (P("a", C(V "a", P("b", V "a")))) in 
  let a85 = check (P("a", V "b")) in 
  let a86 = check (P("a", C(V "a", P("b", V "c")))) in 
  let a87 = check (P("a", P("b", C(V "a", V "c")))) in 

  print_bool(false = check ( V "a" )); 
  print_bool(true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a")))) )); 
  print_bool(false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V     "c")))) )); 
  print_bool(true = a81); 
  print_bool(true = a82); 
  print_bool(true = a83); 
  print_bool(true = a84); 
  print_bool(false = a85); 
  print_bool(false = a86); 
  print_bool(false = a87) 
;;
*)
