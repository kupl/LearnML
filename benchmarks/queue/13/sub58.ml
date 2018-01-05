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

let emptyQ =([],[])


let enQ (que, ele) =
( ele::(fst que) , snd que)
let rec deQ que = 
match que with
| ([], []) -> raise EMPTY_Q
| (lStack, []) -> deQ([], List.rev lStack)
| (lStack, rStack) -> (List.hd rStack, (lStack, List.tl rStack))
end