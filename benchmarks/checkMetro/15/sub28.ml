(* hw2ex3.ml*)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec findName (n: name) (nl: name list) : bool = 
  match nl with
    | [] -> false
    | hd::tl -> if hd = n then true else (findName n tl)

let rec checkName (nl: name list) (m: metro) : bool = 
  match m with
    | STATION n -> (findName n nl)
    | AREA (n,sub_m) -> (checkName (n::nl) sub_m)
    | CONNECT (sub_m1, sub_m2) -> (checkName nl sub_m1) && (checkName nl sub_m2)


let checkMetro (m: metro) : bool = 
  checkName [] m



(* testcase


   let _ = checkMetro (AREA("a", STATION "a"))


   let _ = checkMetro (AREA("a", AREA("a", STATION "a")))






   let _ = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
   let _ = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))


   let _ = checkMetro (AREA("a", STATION "b"))
   let _ = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
   let _ = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))


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
