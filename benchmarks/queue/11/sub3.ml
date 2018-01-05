module type Queue =
sig
type element
type queue
exception EMPTY_Q
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list = 
struct 
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ((l,r),e) = (e::l,r)		
	let rec deQ(q) = 
		match q with
		| ([],[])	-> raise EMPTY_Q
		| (l,[])	-> deQ([],List.rev l)
		| (l,e::r)	-> (e,(l,r))

end 

(*
let myQ = IntListQ.emptyQ
let myQ = IntListQ.enQ(myQ, [1])
let myQ = IntListQ.enQ(myQ, [2])
let myQ = IntListQ.enQ(myQ, [3])
let myQ = IntListQ.enQ(myQ, [4])
let myQ = IntListQ.enQ(myQ, [5])
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let myQ = IntListQ.enQ(myQ, [21])
let myQ = IntListQ.enQ(myQ, [22])
let myQ = IntListQ.enQ(myQ, [23])
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
let (x,myQ) = IntListQ.deQ myQ
*)