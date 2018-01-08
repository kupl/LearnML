(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#2-1 *)

exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

(*
dtree : has node level from top in desc
eg:
                               |					level 5
               |-------------------------------|			level 4
       |---------------|               |---------------|		level 3
   |-------|       |-------|       |-------|       |-------|   		level 2
 |---|   |---|   |---|   |---|   |---|   |---|   |---|   |---| 		level 1
|-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| |-| 	level 0
*)
type dtree = DLEAF of int | DNODE of dtree * dtree * int

(*
depth : return depth of tourna
eg: the depth of above tree is 5
*)
let rec depth t = match t with LEAF s -> 0
		| NODE (a, b) -> (max (depth a) (depth b)) + 1

(*
reconstruct : make dtree from tourna and depth of tourna
usage : reconstruct (t, depth t)
*)
let rec reconstruct (t, d) = match t with LEAF s -> DLEAF d
		| NODE (a, b) -> DNODE ( reconstruct (a, d-1), reconstruct (b, d-1), d)

(*
dot n : make string of '-' in differenct length regarding dtree level
space n : make string of ' ' in differenct length regarding dtree level
*)
let rec power (a, b) = if b == 0 then 1 else a * power (a, b-1)
let dot n = String.make (power(2, n) - 1) '-'
let space n = String.make (power(2, n) - 1) ' '

(*
leaf_struct : make long leaf like right side of below tree
       |       
   |-------|   
 |---|     |   
|-| |-|    |   
usage : leaf_struct (2, 2) // must have same input at two argument
*)
let leaf n = String.concat "|" [(space n); (space n)]
let rec leaf_struct (i, n) =
		if i == 0 then []
		else (leaf n) :: leaf_struct (i - 1, n)

(*
combine : merge two list of string
eg:
                                |
   |           |            |-------|
 |---|   +   |---|   ->   |---|   |---|  
|-| |-|     |-| |-|      |-| |-| |-| |-| 
usage : combine (list1, list2, depth, depth) // must have same input at last two argument
*)
let rec combine (l1, l2, i, n) = if i == 1 then
			(String.concat (String.concat (dot n) ["|"; "|"]) [(space (n-1)); (space (n-1))]) ::
			 [(String.concat "|" [(space n); (space n)])]
		else (String.concat " " [(List.hd l1); (List.hd l2)]) :: (combine (List.tl l1, List.tl l2, i - 1, n))

(*
ptree : construct list of string from dtree using leaf_struct and combine function
*)
let rec ptree t = match t with DLEAF n -> if n != 0 then raise (Error "reconstruct error : invalid dtree") else ["|"]
		| DNODE (DLEAF a, DLEAF b, n) -> if (a != n-1 || b != n-1) then raise (Error "reconstruct error : invalid dtree")
							else combine (leaf_struct (a, a), leaf_struct (b, b), n, n)
		| DNODE (DNODE (x, y, z), DLEAF b, n) -> if (z != n-1 || b != n-1) then raise (Error "reconstruct error : invalid dtree")
							else combine (ptree (DNODE (x, y, z)), leaf_struct (b, b), n, n)
		| DNODE (DLEAF a, DNODE (x, y, z), n) -> if (a != n-1 || z != n-1) then raise (Error "reconstruct error : invalid dtree")
							else combine (leaf_struct (a, a), ptree (DNODE (x, y, z)), n, n)
		| DNODE (DNODE (a, b, c), DNODE (x, y, z), n) -> if (c != n-1 || z != n-1) then raise (Error "reconstruct error : invalid dtree")
							else combine (ptree (DNODE (a, b, c)), ptree (DNODE (x, y, z)), n, n)

let pptree t = List.iter (fun x -> print_string x; print_newline()) (List.rev (ptree (reconstruct (t, depth t))))

(*
(* test code *)

let t1 = LEAF Korea
let f0 = t1
let f1 = NODE (f0, f0)
let f21 = NODE (f1, f1)
let f22 = NODE (f1, f0)
let f23 = NODE (f0, f1)
let f301 = NODE (f21, f0)
let f302 = NODE (f21, f1)
let f303 = NODE (f21, f21)
let f304 = NODE (f21, f22)
let f305 = NODE (f21, f23)
let f306 = NODE (f22, f0)
let f307 = NODE (f22, f1)
let f308 = NODE (f22, f21)
let f309 = NODE (f22, f22)
let f310 = NODE (f22, f23)
let f311 = NODE (f23, f0)
let f312 = NODE (f23, f1)
let f313 = NODE (f23, f21)
let f314 = NODE (f23, f22)
let f315 = NODE (f23, f23)
let f316 = NODE (f0, f21)
let f317 = NODE (f0, f22)
let f318 = NODE (f0, f23)
let f319 = NODE (f1, f21)
let f320 = NODE (f1, f22)
let f321 = NODE (f1, f23)
let f4 = NODE (f303, f303)
let f5 = NODE (f4, f4)
let f6 = NODE (f5, f5)

let _ =
pptree f0;
pptree f1;
pptree f21;
pptree f22;
pptree f23;
pptree f301;
pptree f302;
pptree f303;
pptree f304;
pptree f305;
pptree f306;
pptree f307;
pptree f308;
pptree f309;
pptree f310;
pptree f311;
pptree f312;
pptree f313;
pptree f314;
pptree f315;
pptree f316;
pptree f317;
pptree f318;
pptree f319;
pptree f320;
pptree f321;
pptree f4;
pptree f5;
pptree f6
*)
