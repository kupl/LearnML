(*********************************
 ** PL::HW[02].Problem[05]      **
 **                             **
 ** Mod. Init: 2014-09-27 20:47 **
 ** Mod. Fin.: 2014-09-27 22:23 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

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
	type queue = (element list * element list)
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, e) = ([e]@(fst q), snd q)
	let rec deQ q = 
		match q with 
		| ([], []) -> raise EMPTY_Q
		| (front, []) -> deQ ([], List.rev front)
		| (front, e::back) -> (e, (front, back))
end

