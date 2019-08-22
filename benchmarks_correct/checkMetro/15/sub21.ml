type lambda = V of var | P of var * lambda | C of lambda * lambda
 and var = string 

let check (met:lambda) :bool =
  let rec isElementOfList (e:'a) (l:'a list) :bool = 
    match l with
	| [] -> false
	| h :: t -> if e = h then true else isElementOfList e t
  in
  let rec checkAreasTailing (met:lambda) (areas:var list) :bool =
    match met with
	| V n -> isElementOfList n areas
	| P (n, m) -> checkAreasTailing m (n :: areas)
	| C (m1, m2) -> (checkAreasTailing m1 areas) && (checkAreasTailing m2 areas)
  in
  checkAreasTailing met []

(* MISUNDERSTOOD Version
  let rec tailAreas (met:lambda) (tail:var list) :var list =
    match met with
	| V n -> []
	| P (n, m) -> n :: (tailAreas m)
	| C (m1, m2) -> (tailAreas m1) @ (tailAreas m2)
  in
  let rec tailStations (met:lambda) (tail:var list) :var list =
    match met with
    | V n -> n
    | P (n, m) -> tailStations m
    | C (m1, m2) -> (tailStations m1) @ (tailStations m2)
  in
  let rec isFstSublistOfSnd (fst:a' list) (snd:a' list) :bool =
    let rec isElementOfList (e:a') (l:a' list) :bool = 
	  match l with
	  | [] -> false
	  | h :: t -> if e = a then true else isElementOfList e t
	in
	match fst with
	| [] -> false
	| h :: t -> if isElementOfList h snd then true else isFstSublistOfSnd t snd
  in
  isFstSublistOfSnd (tailStations met) (tailAreas met)
*)
(* TESTCASE
let m1 = P("a", V "a")
let m2 = P("a", P("a", V "a"))
let m3 = P("a", P("b", C(V "a", V "b")))
let m4 = P("a", C(V "a", P("b", V "a")))
let m5 = P("a", V "b")
let m6 = P("a", C(V "a", P("b", V "c")))
let m7 = P("a", P("b", C(V "a", V "c")))

let ms = m1::m2::m3::m4::m5::m6::m7::[]

let f0 x = print_endline(string_of_bool (check x))

let _ = List.map f0 ms 
*)
