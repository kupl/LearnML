(* 2004-11951 Noh, Soon Hyun *)
(* not finished *)

(* belows are from homework #1 *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let team_to_string t =
	match t with
	| Korea -> "Korea"			| France -> "France"
	| Usa -> "Usa"				| Brazil -> "Brazil"
	| Japan -> "Japan"			| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"	| Poland -> "Poland"	
	| Italy -> "Italy"			| Germany -> "Germany"
	| Sweden -> "Sweden"		| England -> "England"
	| Croatia -> "Croatia"		| Argentina -> "Argentina"
	| Portugal -> "Portugal"
	
let rec parenize tourna =
	match tourna with
	| NODE (x1, x2) -> "(" ^ parenize x1 ^ " " ^ parenize x2 ^ ")"
	| LEAF y -> (team_to_string y)

(* homework 2 starts from here *)
(* max is a basic ftn that returns maximum value *)
let max a b =
	if a<b then b
	else a
(* power operator for integer *)
let rec power a b = (* a^b *)
	if b=0 then 1
	else a * (power a (b-1))
(* printDuplicate return target string n times *)
let rec printDuplicate element n =
	if n=0 then ""
	else element ^ (printDuplicate element (n-1))

(* chkDepth returns tree depth *)
let rec chkDepth tourna =
	match tourna with
	| LEAF _ -> 0
	| NODE (t1, t2) -> (1 + (max (chkDepth t1) (chkDepth t2)))

(* return string value of a node or a leaf for 1 depth *)
let printNode tourna depth =
	match (tourna, depth) with
	(* bottom case *)
	| (_, 0) -> ""
	| (NODE (t1, t2), _)
		-> (printDuplicate " " ((power 2 (depth-1))-1)) ^ "|" ^
		   (printDuplicate "-" ((power 2 depth)-1)) ^ "|" ^
		   (printDuplicate " " (power 2 (depth-1)))
	| (LEAF t,  _)
		-> (printDuplicate " " ((power 2 depth)-1)) ^ "|" ^
		   (printDuplicate " " (power 2 depth))
	

(* subpptree: tourna -> integer -> integer -> tourna list*)
let rec subpptree currentList nextList depth =
	match (currentList, depth) with
	| (_, 0) -> ""
	| ([], d) -> "\n" ^ (subpptree nextList [] (d-1))
	| ((NODE (t1, t2), _)::l, d)
		-> (printNode (NODE (t1, t2)) d) ^ 
		   (subpptree l (List.append nextList [(t1, (depth-1)); (t2, (depth-1))]) d) 

	| (((LEAF t), own)::l, d)
		-> (printNode (LEAF t) own) ^
		   (subpptree l (List.append nextList [(LEAF t), own]) d)

let pptree tourna =
	print_string ((printDuplicate " " 
								 ((power 2 (chkDepth tourna))-1)) ^
				 "|\n");
	print_string (subpptree [tourna, (chkDepth tourna)] [] (chkDepth tourna))
(*

let a = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (LEAF A, LEAF B))
let b = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B))
let c = NODE(LEAF A, NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B)))

let _ = pptree a
let _ = pptree b
let _ = pptree c
*)
