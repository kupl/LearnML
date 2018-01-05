module type Queue = sig
  type element
  type queue 
  exception EMPTY_Q
  val emptyQ: queue 
  val enQ: queue * element -> queue 
  val deQ: queue -> element * queue 
end

module IntListQ = struct
  type element = int list
  type queue = Q of element * element
  exception EMPTY_Q
  let emptyQ = Q([], [])
  let enQ = function
    | (Q(l, r), d) -> Q(d@l, r)
  let rec deQ = function
    | Q([], []) -> raise EMPTY_Q
    | Q(l, r::right) -> (r, Q(l, right))
    | Q(l, []) -> deQ (Q([], List.rev l))
end

(* TEST CASE *)
(*
let q1 = IntListQ.emptyQ;;
let q2 = IntListQ.enQ(q1, [1]);;
let (x, q3) = IntListQ.deQ q2;;
let q4 = IntListQ.enQ(q1, [2]);;
let q5 = IntListQ.enQ(q4, [3]);;
let q6 = IntListQ.enQ(q5, [4]);;
let q7 = IntListQ.enQ(q6, [5]);;
let q8 = IntListQ.enQ(q7, [6]);;
let q9 = IntListQ.enQ(q8, [7]);;
let q10 = IntListQ.enQ(q9, [8]);;
let (x, q11) = IntListQ.deQ q10;;
let (x, q12) = IntListQ.deQ q11;;
let (x, q13) = IntListQ.deQ q12;;
let (x, q14) = IntListQ.deQ q13;;
let q19 = IntListQ.enQ(q14, [8]);;
let (x, q15) = IntListQ.deQ q14;;
let (x, q16) = IntListQ.deQ q15;;
let (x, q17) = IntListQ.deQ q16;;
let (x, q18) = IntListQ.deQ q17;;
*)
