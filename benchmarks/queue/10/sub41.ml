(* \\kof\FolderRedirection\comjoy91\¹ÙÅÁ È­¸é\Homework 2(4).ml *)

module type Queue =
sig	
	type element
	type queue

	exception EMPTY_Q

	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end






module IntListQ : Queue =
struct
	type element = int list
	type queue = (element list) * (element list)

	exception EMPTY_Q

	let emptyQ = ([], [])
	let enQ (que, ele) = 
		match que with
		(eleList1, eleList2) -> (ele::eleList1, eleList2)
 
	let rec deQ que =
		match que with
		(eleList1, []) -> if List.length eleList1 = 0 	then raise EMPTY_Q
								else deQ ([], (List.rev eleList1))
		| (eleList1, h::tailList) -> (h, (eleList1, tailList))
end ;;


