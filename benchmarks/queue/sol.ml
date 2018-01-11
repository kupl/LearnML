(*Queue*)
(*http://ropas.snu.ac.kr/~kwang/4190.310/09/hw2.pdf 6번*)
module type Queue = 
	sig 
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ : Queue =
struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	
  let enQ : queue * element -> queue 
	= fun ((l,r),e) -> (e::l,r)
  
  let rec list_rev l = 
    match l with
    |[] -> []
    |hd::tl ->(list_rev tl)@[hd]
	
  let rec deQ : queue -> element * queue
	= fun q ->
		match q with
		|([],[]) -> raise EMPTY_Q
		|(l,h::r) -> (h,(l,r))
		|(l,[]) -> deQ  ([],list_rev l)
end
