(* 2011-10915 / 생명과학부/ 신지민 / Homework 2-6 *)

module type Queue =
               sig
	       type element
	       type queue
	       exception EMPTY_Q
	       val emptyQ: queue
	       val enQ: queue * element -> queue val deQ: queue -> element * queue
	       end

 module IntListQ =
               struct
	       type element = int list 
	       type queue = element list * element list 
	       exception EMPTY_Q
	       let emptyQ : queue = ([],[])

	       let enQ : queue * element -> queue = function ((l,r),e) -> ((e::l,r))

	       let deQ : queue -> element * queue = function ((l,r)) ->
	       		if(r=[]) then begin
				if(l=[]) then raise EMPTY_Q
				else begin 
					let r = List.rev l in
					let l = [] in
					(List.hd r,(l,List.tl r))
				     end
				     end
			
			else (List.hd r,(l,List.tl r))
	       end

(*
module ValidIntListQ = (IntListQ : Queue)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(restQ, [2]) 
let (x,rQ) = IntListQ.deQ restQ
let (x,eQ) = IntListQ.deQ rQ*)
