
(* EXERCISE 6 *)
module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ =
struct
	type element = int list
	type queue = (element list) * (element list)
	exception EMPTY_Q 
	let emptyQ = ([], [])
	let enQ = fun (queue_in , elm_in) -> (elm_in :: (fst queue_in), (snd queue_in))
	let deQ = fun queue_in -> 
(*fun queue_in ->
		if length(snd queue_in) > 0 then (hd (snd queue_in), (fst queue_in , tl (snd queue_in)))
		else if length(fst queue_in) > 0 then (hd (rev (fst queue_in)), ([],tl( rev (fst queue_in))))
		else raise (EMPTY_Q "Cant dequeue from empty queue")
*)  
		let hd : element list -> element = fun lin -> 
			match lin with 
			| [] -> raise (Failure "Empty list")
			| h :: t -> h
		in
		let tl : element list -> element list = fun lin -> 
			match lin with
			| [] -> raise (Failure "Empty list")
			| h :: t -> t
		in
		let rec snoc : element -> element list -> element list = fun elmin lin ->
			match lin with
			| [] -> elmin :: []
			| h :: t -> h :: (snoc elmin t)
		in
		let rec rev : element list -> element list = fun lin ->
			match lin with
			| [] -> []
			| h :: t -> snoc h (rev t)
		in		
		match queue_in with
		| (leftstack, firstin :: rightstack) -> (firstin, (leftstack, rightstack))
		| (left :: leftstack, []) -> (hd (rev (left :: leftstack)) ,([],tl (rev (left :: leftstack))))
		| ([],[]) -> raise (EMPTY_Q)

end
(*
module ValidIntListQ = (IntListQ : Queue)
let myQ = IntListQ.emptyQ

let p : IntListQ.element = [1;2;3]

let yourQ = IntListQ.enQ(myQ, 1::2::[])

let (x, restQ) = IntListQ.deQ yourQ

let hisQ = IntListQ.enQ(myQ, [2;3])

let q1 = IntListQ.emptyQ 
let q2 = IntListQ.enQ(q1, [1]) 
let q3 = IntListQ.enQ(q2, [2;3]) 
let q4 = IntListQ.enQ(q3, [4;5;6]) 
let (l1, q5) = IntListQ.deQ q4 
let q6 = IntListQ.enQ(q5, [7;8;9;10]) 
let (l2, q7) = IntListQ.deQ q6 
let q8 = IntListQ.enQ(q7, [11;13;20;100]) 
let (l3, q9) = IntListQ.deQ q8 
let (l4, q10) = IntListQ.deQ q9 
let (l5, q11) = IntListQ.deQ q10 
let q12 = IntListQ.enQ(q11, [4;5;6;7]) 
let (l6, q13) = IntListQ.deQ q12 

let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case (1, ([1] = l1)); 
  test_case (2, ([2;3] = l2)); 
  test_case (3, ([4;5;6] = l3)); 
  test_case (4, ([7;8;9;10] = l4)); 
  test_case (5, ([11;13;20;100] = l5)); 
  test_case (6, ([4;5;6;7] = l6)); 
  test_case (7, q13 = IntListQ.emptyQ) 

let (x, y) = try IntListQ.deQ q13 with IntListQ.EMPTY_Q -> ([19682934], IntListQ.emptyQ) 
let _ = if(x = [19682934]) then print_endline ("Error Case : Pass") 
  else print_endline("Error Case : Failure")*)
