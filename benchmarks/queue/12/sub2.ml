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
type queue = element list*element list
exception EMPTY_Q
let emptyQ :queue =([],[])  
let enQ:queue * element -> queue=fun(q,e)->
  match q with
  ([],[])->(e::[],[])
  |(l,r)->((e::l),r)
let deQ: queue -> element * queue=fun q ->
  match q with
  ([],[])->raise EMPTY_Q
  |(l,[])->
	let r= List.rev l in
    (List.hd r, ([],List.tl r))
  |(l,hd::tl)->(hd,(l,tl))
end
