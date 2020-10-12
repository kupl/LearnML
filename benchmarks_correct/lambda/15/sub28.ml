(* hw2ex3.ml*)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec findName (n: var) (nl: var list) : bool = 
  match nl with
    | [] -> false
    | hd::tl -> if hd = n then true else (findName n tl)

let rec checkName (nl: var list) (m: lambda) : bool = 
  match m with
    | V n -> (findName n nl)
    | P (n,sub_m) -> (checkName (n::nl) sub_m)
    | C (sub_m1, sub_m2) -> (checkName nl sub_m1) && (checkName nl sub_m2)


let check (m: lambda) : bool = 
  checkName [] m



(* testcase


   let _ = check (P("a", V "a"))


   let _ = check (P("a", P("a", V "a")))






   let _ = check (P("a", P("b", C(V "a", V "b"))))
   let _ = check (P("a", C(V "a", P("b", V "a"))))


   let _ = check (P("a", V "b"))
   let _ = check (P("a", C(V "a", P("b", V "c"))))
   let _ = check (P("a", P("b", C(V "a", V "c"))))


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
