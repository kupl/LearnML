(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-3 *)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec isInGroup: name * name list -> bool = fun(n, l) ->
  match l with
  | [] -> false
  | hd::tl -> if(hd = n) then true else isInGroup(n, tl) 

let rec checkMetroInGroup: metro * name list -> bool = fun(m, l) ->
  match m with
  | STATION n -> isInGroup(n, l)
  | AREA (n, m1) -> checkMetroInGroup(m1, n::l)
  | CONNECT (m1, m2) ->
      if(checkMetroInGroup(m1, l)
      && checkMetroInGroup(m2, l)) then true else false

let rec checkMetro: metro -> bool = fun(m) ->
  checkMetroInGroup(m, [])

(*
let _= 
  let print_bool x = print_endline (string_of_bool x) in 

  let a81 = checkMetro (AREA("a", STATION "a")) in 
  let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in 
  let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in 
  let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in 
  let a85 = checkMetro (AREA("a", STATION "b")) in 
  let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in 
  let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in 

  print_bool(false = checkMetro ( STATION "a" )); 
  print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))) )); 
  print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION     "c")))) )); 
  print_bool(true = a81); 
  print_bool(true = a82); 
  print_bool(true = a83); 
  print_bool(true = a84); 
  print_bool(false = a85); 
  print_bool(false = a86); 
  print_bool(false = a87) 
;;
*)
