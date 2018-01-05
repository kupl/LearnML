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
		let emptyQ = ( [], [] )
		let enQ ( q, elem ) = match q with ( enq, deq ) -> ( elem::enq, deq )
		let rec deQ ( q : queue ) = 
			match q with
				| ( enq, de::deq ) -> ( de, ( enq, deq ) )
				| ( t::enq, [] ) -> deQ ( ( [], ( List.rev ( t::enq ) ) ) )
				| ( [], [] ) -> raise EMPTY_Q
	end

let rec print_intList t = match t with a::b -> print_int a; print_string " "; (print_intList b ) | [] -> print_string " ";;
let rec print_intListList t = match t with a::b -> print_intList a; print_string " "; (print_intListList b ) | [] -> print_string " ";;

let print_dqres p = match p with ( em, restQ ) -> print_intList em;;

let rec print_queue ( ( p : IntListQ.queue ), ( c : int ) ) = 
	let isEmpty ( q : IntListQ.queue ) = ( q = IntListQ.emptyQ ) in
	if isEmpty p then print_string " end\n"
	else match ( IntListQ.deQ( p ) ) with ( a, b ) -> print_string " ";print_intList a;( print_queue  (b, c + 1 ) );print_int c;;

let myQ = IntListQ.emptyQ;;
let yourQ = IntListQ.enQ( IntListQ.enQ( IntListQ.enQ(myQ, [1]), [3] ), [2] );;
let hisQ = IntListQ.enQ(myQ, [2]);;
let (x, restQ) = ( IntListQ.deQ ( yourQ ) );;

print_queue (myQ, 0);;
print_queue (yourQ, 0);;
print_queue (hisQ, 0);;
print_intList (x);;
print_queue (restQ, 0);;
