
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
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])   (* fst / snd *)
	let enQ = fun (x, y) ->
		([y] @ fst(x), snd(x))
	let deQ = fun (que) ->
	let getArrangedStack(q) = (* if no args at stack then reverse it *)
		if (List.length(snd(q)) == 0) then
			if (List.length(fst(q)) == 0) then raise EMPTY_Q else ([], List.rev(fst(q)))
		else q
	in let res = getArrangedStack(que)
	in ( List.nth (snd(res)) 0, (fst(res), (List.tl(snd(res))) ) )
end

(* test *)
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])
