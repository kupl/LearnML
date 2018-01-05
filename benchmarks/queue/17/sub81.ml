module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

let rec append: 'a list * 'a list -> 'a list = fun(list1,list2) ->
        match list1 with
                 [] -> list2
                |hd::tail -> hd::append(tail,list2)




let rec reverse_list: 'a list -> 'a list = fun(list_)
-> match list_ with
         [] -> []
        |hd::tail -> append(reverse_list(tail),[hd])


module IntListQ =
struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ : queue = ([],[])
	let enQ: queue * element -> queue = fun(q_, elm_) -> 
		(elm_::fst q_, snd q_)
	let deQ: queue -> element * queue = fun(q_)
	  -> match q_ with
		(a,b) -> (
			   match b with [] -> 
					 ( match a with [] -> raise EMPTY_Q
					      |hd::tl ->
					    ( match reverse_list(a) with hd::tail ->
						 ( hd,([],tail) )
						| [] -> raise EMPTY_Q
					    )
					 )
					|hd::tail -> (hd,(a,tail))
			 )


end
