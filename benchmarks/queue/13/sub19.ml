module type Queue=
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ:queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ=
	struct
		type element = int list
		type queue=((int list) list) * ((int list) list)
		exception EMPTY_Q
		let emptyQ=([],[])
		let enQ (q, ele)=match q with
		|(alist, blist)->((ele::alist), blist)
		
		let rec deQ q=match q with
		|(alist, blist)->if alist=[] && blist=[] then raise EMPTY_Q
						else if blist=[] then deQ ([],List.rev alist)
						else (match blist with
						    	|(hd::tl)->(hd, (alist,tl))
								)
	end
