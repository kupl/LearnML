type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

(** check: lambda -> bool *)
let rec check met = (* is met consistent? *)
  aux [] met

  and aux varlist met = match met with (* does varlist embrace met? *)
  | V n -> contains(n, varlist) (* varlist contains n*)
  | P (n,m1) -> aux (n::varlist) m1 (*new varlist = n :: old, check m1*)
  | C (m1,m2)-> (aux varlist m1) && (aux varlist m2) (* varlist both embraces m1, m2*)

  and contains(n, varlist) = match varlist with (* varlist contains n*)
  | [] -> false
  | hd :: tl -> (
    if(n = hd) then true
    else contains(n, tl)
    )

(** Testcases *)
(**
let _ =
let myprint met = if (check met) then print_int 1 else print_int 0 in
(* true *)
myprint(P("a", V "a"));
myprint(P("a", P("a", V "a")));
myprint(P("a", P("b", C(V "a", V "b"))));
myprint(P("a", C(V "a", P("b", V "a"))));

(* false *)
myprint(P("a", V "b"));
myprint(P("a", C(V "a", P("b", V "c"))));
myprint(P("a", P("b", C(V "a", V "c"))))
;;
let _=
let print_bool x = print_endline (string_of_bool x) in

let a81 = check (P("a", V "a")) in
let a82 = check (P("a", P("a", V "a"))) in
let a83 = check (P("a", P("b", C(V "a", V "b")))) in
let a84 = check (P("a", C(V "a", P("b", V "a")))) in
let a85 = check (P("a", V "b")) in
let a86 = check (P("a", C(V "a", P("b", V "c")))) in
let a87 = check (P("a", P("b", C(V "a", V "c")))) in

print_bool(false = check ( V "a"));
print_bool(true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a"))))));
print_bool(false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c"))))));
print_bool(true = a81);
print_bool(true = a82);
print_bool(true = a83);
print_bool(true = a84);
print_bool(false = a85);
print_bool(false = a86);
print_bool(false = a87)
;;
*)
