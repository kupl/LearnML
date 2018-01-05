(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-6.*)

module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end
;;

module IntListQ : Queue with type element = int list =
struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let rec enQ(q, e) = match q with
		| l, r -> e :: l, r
	let rec deQ(q) = match q with
		| [], [] -> raise EMPTY_Q
		| l, [] -> deQ ([], List.rev l)
		| l, hd :: rstack -> hd, (l, rstack)
end
;;

(* 
let myQ = IntListQ.emptyQ;;
let yourQ = IntListQ.enQ(myQ,[1]);;
let hisQ = IntListQ.enQ(yourQ,[2]);;
let herQ = IntListQ.enQ(hisQ,[3]);;
let (x, restQ) = IntListQ.deQ herQ;;
let (x, restQ2) = IntListQ.deQ restQ;;
let (x, restQ3) = IntListQ.deQ restQ2;;
let (x, restQ4) = IntListQ.deQ restQ3;;
let (x, restQ5) = IntListQ.deQ restQ4;;
*)