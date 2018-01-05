type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

(** checkMetro: metro -> bool *)
let rec checkMetro met = (* is met consistent? *)
  aux [] met

  and aux namelist met = match met with (* does namelist embrace met? *)
  | STATION n -> contains(n, namelist) (* namelist contains n*)
  | AREA (n,m1) -> aux (n::namelist) m1 (*new namelist = n :: old, check m1*)
  | CONNECT (m1,m2)-> (aux namelist m1) && (aux namelist m2) (* namelist both embraces m1, m2*)

  and contains(n, namelist) = match namelist with (* namelist contains n*)
  | [] -> false
  | hd :: tl -> (
    if(n = hd) then true
    else contains(n, tl)
    )

(** Testcases *)
(**
let _ =
let myprint met = if (checkMetro met) then print_int 1 else print_int 0 in
(* true *)
myprint(AREA("a", STATION "a"));
myprint(AREA("a", AREA("a", STATION "a")));
myprint(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));
myprint(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));

(* false *)
myprint(AREA("a", STATION "b"));
myprint(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));
myprint(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
;;
let _=
let print_bool x = print_endline (string_of_bool x) in

let a81 = checkMetro (AREA("a", STATION "a")) in
let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in
let a85 = checkMetro (AREA("a", STATION "b")) in
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in

print_bool(false = checkMetro ( STATION "a"));
print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));
print_bool(true = a81);
print_bool(true = a82);
print_bool(true = a83);
print_bool(true = a84);
print_bool(false = a85);
print_bool(false = a86);
print_bool(false = a87)
;;
*)
