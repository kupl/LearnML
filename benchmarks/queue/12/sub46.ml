(* Name: Yoon Jae Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 4: Queue = 2 stacks *)

(* 1. Provided declarations / definitions *)
module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

(* 2. My code *)
module IntListQ = struct
	type element = int list
	type queue = element list * element list (* (l,r) *)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ : queue * element -> queue = fun ((l,r), elem) -> (
		(elem::l,r)
	)
	let deQ : queue -> element * queue = fun (l,r) -> (
		match (l,r) with
		| ([],[]) -> (* Error: attempting to deQ from an empty queue *)
			raise EMPTY_Q
		| (_,[]) -> (
			(* Need to reverse L and make that R, before deQ-ing *)
			let new_r = List.rev l in
			let new_r_hd = List.hd new_r in
			let new_r_tl = List.tl new_r in
				(new_r_hd, ([], new_r_tl))
		)
		| (_,rh::rt) -> (rh, (l,rt))
	)
end

(* 3. Test code *)
(*
module ValidIntListQ = (IntListQ: Queue)

let rec intListToStringHelper : int list -> string = fun l ->
	match l with
	| [] -> ""
	| h::[] -> string_of_int(h)
	| h::t -> (string_of_int(h) ^ ";" ^ intListToStringHelper(t))
(**)
let rec intListToString : int list -> string = fun l ->
	("[" ^ intListToStringHelper(l) ^ "]")
(**)
let testRunner (title,expected,actual) =
	print_endline "=========================================";
	(if expected = actual then
		print_endline ("                            Good: " ^ title)
	else
		print_endline ("                            Bad: " ^ title));
	let expected_string = intListToString(expected) in
	let actual_string = intListToString(actual) in
	Printf.printf "Expected: %s\n-------------\nActual  : %s\n" expected_string actual_string
(**)

let test =
	let q0 = IntListQ.emptyQ in
	let q1 = IntListQ.enQ(q0,[1]) in
	let (x1,q2) = IntListQ.deQ(q1) in
		testRunner("test1",[1], x1);
	let q3 = IntListQ.enQ(q2,[1;2;3]) in
	let q4 = IntListQ.enQ(q3,[4;5;6]) in
	let q5 = IntListQ.enQ(q4,[]) in
	let (x2,q6) = IntListQ.deQ(q5) in
		testRunner("test2",[1;2;3], x2);
	let (x3,q7) = IntListQ.deQ(q6) in
		testRunner("test3",[4;5;6], x3);
	let (x4,q8) = IntListQ.deQ(q7) in
		testRunner("test4",[], x4);
	(try
		print_endline "=========================================";
		let (_,_) = IntListQ.deQ(q8) in
			print_endline ("                             Bad: test5 (Error expected)")
	with
		| _ -> (
			print_endline ("                            Good: test5 (Error caught)")
		)
	);
	let q9 = IntListQ.enQ(q8, [10;10;10]) in
	let q10 = IntListQ.enQ(q9, [-10;-10;-10]) in
	let (x5,q11) = IntListQ.deQ(q10) in
		testRunner("test5",[10;10;10], x5);
	let (x6,q12) = IntListQ.deQ(q11) in
		testRunner("test6",[-10;-10;-10], x6);		
(**)
*)