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
                let enQ : queue * element -> queue = fun ((l,r),e) ->
                        (e::l,r)
                let deQ : queue -> element * queue = fun (l,r) ->
                        if r=[] then
                                if l=[] then raise EMPTY_Q
                                else let r=List.rev l in
                                        List.hd r,([],List.tl r)
			else List.hd r,(l,List.tl r)
        end
(*
let deq = fun q e -> let (r,t) = IntListQ.deQ q in
  let _ = assert (r=e) in
  t
let enq = fun q e -> IntListQ.enQ (q,e)

let q = (IntListQ.emptyQ)
let q = enq q [1]
let q = enq q [2]
let q = deq q [1]
let q = enq q [1;2]
let q = deq q [2]
let q = deq q [1;2]

let q = enq q [1]
let q = enq q [2]
  let q = deq q [1]
let q = enq q [3]
  let q = deq q [2]
let q = enq q [4]
let q = enq q [5]
let q = enq q [6]
  let q = deq q [3]
  let q = deq q [4]
  let q = deq q [5]
let q = enq q [7]
  let q = deq q [6]
let q = enq q [8]
let q = enq q [9]
let q = enq q [10]
  let q = deq q [7]
  let q = deq q [8]
  let q = deq q [9]
  let q = deq q [10]

let q = deq q [-1] (* empty error *)*)
