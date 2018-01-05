(* 2004-11951 Noh, Soon Hyun *)

module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end
	
module IntListQ : Queue with type element = int list =
struct
	type element = int list
	type queue = (int list list) * (int list list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (queue, element) =
		match queue with
		| (stack1, stack2) -> (element::stack1, stack2)
	let deQ queue =
		match queue with
		| (stack1, []) 
			-> (match (List.rev stack1) with
				| [] -> raise EMPTY_Q
				| c::revstack2 ->	(c, ([], revstack2)))
		| (stack1, c::stack2)
			-> (c, (stack1, stack2))
end


(* Test procedure 
let print1st l =
	match l with
	| [] -> print_string "empty\n"
	| c::l -> print_int c; print_char '\n'
let printS st =
	match st with
	| b::c -> print1st b
	| _ -> print_string "stackempth\n"

let q0 = IntListQ.emptyQ
let q1 = IntListQ.enQ(q0, [0])
let q2 = IntListQ.enQ(q1, [2])
let q3 = IntListQ.enQ(q2, [0])
let (x1,q4) = IntListQ.deQ q3
let q5 = IntListQ.enQ(q4, [-1])
let (x2,q6) = IntListQ.deQ q5
let (x3,q7) = IntListQ.deQ q6
let (x4,q8) = IntListQ.deQ q7

let _ = print1st x1; print1st x2; print1st x3; print1st x4
*)
