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

let enQ pair =
match pair with
|((lst1, lst2), elem) -> ([elem]@lst1, lst2)

let deQ queue =
match queue with
|([],[]) -> raise EMPTY_Q
|(lst1, []) -> ((List.hd (List.rev lst1)), ([],(List.tl (List.rev lst1))))
|(lst, hd::tl) -> (hd, (lst, tl))

end
