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
	  let emptyQ = ([],[])
	  let enQ: queue * element -> queue =
		fun (myQ, item) ->
		match myQ with 
		| (a,b) -> (item::a, b)
	  let deQ: queue -> element * queue =
		fun myQ ->
		(*if myQ==emptyQ then raise EMP*)
		match myQ with
		| ([],[])-> raise EMPTY_Q
		| (a,[]) ->
			let foo:element list= List.rev a in
			let tmp2:element list =( if(List.length a ==1) then [] else List.tl foo) in
			let tmp:element=List.hd foo in
			(tmp, ([],tmp2) )
		| (a,b) ->
			let tmp2:element list=( if List.length b==1 then [] else List.tl b) in
			let tmp:element=List.hd b in
			(tmp, (a,tmp2) ) 
	end

(*for test whether my IntListQ is well defined*)
(*module ValidIntListQ = (IntListQ : Queue) *)

