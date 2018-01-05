(* Name: Yoon Jae Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 3: Priority Queue *)

(* 1. Provided declarations / definitions *)
type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
	| EMPTY -> -1 
	| NODE(r,_,_,_) -> r 
(**)

(* shake: Returns a new node with value x whose sub-heaps are lh and rh *)
let shake (x,lh,rh) = 
	if (rank lh) >= (rank rh) 
	then NODE(rank rh+1, x, lh, rh) 
	else NODE(rank lh+1, x, rh, lh) 
(**)

(* Merges h1 and h2 into a single leftist heap *)
let rec merge : heap * heap -> heap = fun (h1,h2) ->
	match (h1, h2) with
	| (_, EMPTY) -> h1
	| (EMPTY, _) -> h2
	| (NODE(rank1,v1,lh1,rh1),NODE(rank2,v2,lh2,rh2)) -> (
		if v1 >= v2 then shake(v2, lh2, merge(rh2, h1))
		else shake(v1, lh1, merge(rh1, h2))
	)
(**)

(* Returns the value of the root of h *)
let findMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,_,_) -> x 
(**)

(* Inserts a value x into h *)
let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

(* Deletes the root of h *)
let deleteMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh) 
(**)

(* 3. Test code *)
(*
(*
		1
	2		4
3
*)
let testHeap1 = NODE(
	1, (* rank *)
	1, (* value *)
	NODE(
		0,
		2,
		NODE(
			0,
			3,
			EMPTY,
			EMPTY
		),
		EMPTY
	),
	NODE(
		0,
		4,
		EMPTY,
		EMPTY
	)
)
(*
			5
	6				5
				6
*)
let testHeap2 = NODE(
	1,
	5,
	NODE(
		0,
		6,
		EMPTY,
		EMPTY
	),
	NODE(
		0,
		5,
		NODE(
			0,
			6,
			EMPTY,
			EMPTY
		),
		EMPTY
	)
)
(*
		1
	2		3
*)
let testHeap3 = NODE(
	0,
	1,
	NODE(0,2,EMPTY,EMPTY),
	NODE(0,3,EMPTY,EMPTY)
)
(*
	4
*)
let testHeap4 = NODE(0,4,EMPTY,EMPTY)
(*
	5
*)
let testHeap5 = NODE(0,5,EMPTY,EMPTY)

let rec stringListToString : string list -> string = fun l ->
	match l with
	| [] -> ""
	| h::[] -> h
	| h::t -> (h ^ stringListToString(t))
(**)
	
let rec heapToString = fun h ->
	match h with
	| EMPTY -> "EMPTY"
	| NODE(rank,value,left,right) -> (
		let string_list = [
			"NODE(";
			string_of_int(rank);
			",";
			string_of_int(value);
			",";
			heapToString(left);
			",";
			heapToString(right);
			")"
		] in
		stringListToString(string_list)
	)
(**)

let testRunner = fun (heap1, heap2, result) ->
	print_endline "===========================================";
	let heap1_string = heapToString(heap1) in
	let heap2_string = heapToString(heap2) in
	let merged_heap = merge(heap1,heap2) in
	let merged_heap_string = heapToString(merged_heap) in
		print_endline ("                                   " ^ result);
		print_endline "Heap 1:";
		print_endline heap1_string;
		print_endline "-----------------";
		print_endline "Heap 2:";
		print_endline heap2_string;
		print_endline "-----------------";
		print_endline "Merged heap:";
		print_endline merged_heap_string
(**)

let test =
	let heap1 = testHeap1 in
	let heap2 = testHeap2 in
	let result = "h1 + h2: Good" in
	testRunner(heap1,heap2, result)
(**)
let test =
	let heap1 = testHeap4 in
	let heap2 = testHeap5 in
	let result = "h4 + h5: Good" in
	testRunner(heap1,heap2, result)
(**)
let test =
	let heap1 = testHeap3 in
	let heap2 = EMPTY in
	let result = "h3 + E: Good" in
	testRunner(heap1,heap2, result)
(**)
let test = 
	print_endline "============================================";
	let hp1 = EMPTY in
	let hp2 = insert(1,insert(3,insert(2,insert(2,insert(1,hp1))))) in
	let hp3 = insert(-1,insert(0,hp1)) in
	let	hp23 = merge(hp2,hp3) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1
	);
	
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);
	let hp23 = deleteMin(hp23) in
	let min1 = findMin(hp23) in (
		Printf.printf "Min: %d\n" min1;
	);	
(**)
*)