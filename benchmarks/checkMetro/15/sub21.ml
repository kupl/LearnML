type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
 and name = string 

let checkMetro (met:metro) :bool =
  let rec isElementOfList (e:'a) (l:'a list) :bool = 
    match l with
	| [] -> false
	| h :: t -> if e = h then true else isElementOfList e t
  in
  let rec checkAreasTailing (met:metro) (areas:name list) :bool =
    match met with
	| STATION n -> isElementOfList n areas
	| AREA (n, m) -> checkAreasTailing m (n :: areas)
	| CONNECT (m1, m2) -> (checkAreasTailing m1 areas) && (checkAreasTailing m2 areas)
  in
  checkAreasTailing met []

(* MISUNDERSTOOD Version
  let rec tailAreas (met:metro) (tail:name list) :name list =
    match met with
	| STATION n -> []
	| AREA (n, m) -> n :: (tailAreas m)
	| CONNECT (m1, m2) -> (tailAreas m1) @ (tailAreas m2)
  in
  let rec tailStations (met:metro) (tail:name list) :name list =
    match met with
    | STATION n -> n
    | AREA (n, m) -> tailStations m
    | CONNECT (m1, m2) -> (tailStations m1) @ (tailStations m2)
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
let m1 = AREA("a", STATION "a")
let m2 = AREA("a", AREA("a", STATION "a"))
let m3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let m4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let m5 = AREA("a", STATION "b")
let m6 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let m7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))

let ms = m1::m2::m3::m4::m5::m6::m7::[]

let f0 x = print_endline(string_of_bool (checkMetro x))

let _ = List.map f0 ms 
*)
