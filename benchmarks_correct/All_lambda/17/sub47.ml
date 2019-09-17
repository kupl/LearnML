type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let rec retvar (met:lambda) : string list =
match met with
| V str -> [str]
| P (str,lambda) -> List.filter (fun x->(x<>str)) (retvar (lambda))
| C (met1, met2) -> List.append (retvar (met1)) (retvar (met2))

let rec check (met:lambda) : bool = 
match met with
| V var -> false
| P(var, lambda) -> if((retvar(met))==[]) then true else false
| C(me1,me2) -> (check me1)&&(check me2)



(*
(*test new*)
let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
  print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, true == check(P("a", V "a"))); 
  test_case(2, true == check(P("a", P("a", V "a")))); 
  test_case(3, true == check(P("a", P("b", C(V "a", V "b"))))); 
  test_case(4, true == check(P("a", C(V "a", P("b", V "a"))))); 
  test_case(5, false == check(P("a", V "b"))); 
  test_case(6, false == check(P("a", C(V "a", P("b", V "c"))))); 
  test_case(7, false == check(P("a", P("b", C(V "a", V "c"))))); 
  test_case(8, true == check(C(P("a", V "a"), P("b", P("a", C(V "b", V "a")))))); 
  test_case(9, false == check(C(P("c", V "c"), P("b", P("a", C(V "b", V "c")))))); 
  test_case(10, false == check(V "a"))

(*test old*)
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
;;*)
