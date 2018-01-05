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
type queue = (int list list * int list list)
exception EMPTY_Q
let emptyQ = [], []
let enQ (que, e) =
let front, back = que in
let tmpque = (e::front, back) in
tmpque
let rec deQ que = 
match que with
(front, e :: back) ->
let retque = (front, back) in
(e, retque)
|([], []) ->
raise EMPTY_Q
|(front, []) ->
let revque = ([], List.rev front) in
deQ revque
end