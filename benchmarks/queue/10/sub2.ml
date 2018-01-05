(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#2-4 *)

module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ =
	struct
		type element = int list
		type queue = ILQ of element list * element list
		exception EMPTY_Q
		let emptyQ = ILQ ([], [])
		let enQ (q, e) = match q with ILQ (l1, l2) -> ILQ (e::l1, l2)
		let deQ q = match q with ILQ (l1, []) -> (List.hd (List.rev l1), ILQ ([], List.tl (List.rev l1)))
			| ILQ (l1, h::t) -> (h, ILQ (l1, t))
	end

(*
(* test code *)

let myQ = IntListQ.emptyQ;;
let myQ = IntListQ.enQ(myQ, [1]);;
let myQ = IntListQ.enQ(myQ, [2]);;
let myQ = IntListQ.enQ(myQ, [3]);;
let myQ = IntListQ.enQ(myQ, [4]);;
let myQ = IntListQ.enQ(myQ, [5]);;
let (x, myQ) = IntListQ.deQ(myQ);;
let myQ = IntListQ.enQ(myQ, [6]);;
let myQ = IntListQ.enQ(myQ, [7]);;
let myQ = IntListQ.enQ(myQ, [8]);;
let myQ = IntListQ.enQ(myQ, [9]);;
let myQ = IntListQ.enQ(myQ, [10]);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let myQ = IntListQ.enQ(myQ, [11]);;
let myQ = IntListQ.enQ(myQ, [12]);;
let myQ = IntListQ.enQ(myQ, [13]);;
let myQ = IntListQ.enQ(myQ, [14]);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ);;
let (x, myQ) = IntListQ.deQ(myQ)

*)
