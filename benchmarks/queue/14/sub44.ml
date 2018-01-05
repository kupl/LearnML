
(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q5: Queue *)

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
  type queue = (element list * element list)
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ arg =
    let (q, e) = arg in
    let (a, b) = q in
    if (List.length a > List.length b)
    then (e :: [], List.append b (List.rev a))
    else (e :: a, b)

  let rec deQ = function
    | ([], []) -> raise EMPTY_Q
          | (x :: xs as a, []) -> deQ ([], List.rev a)
          | (a, x :: xs) -> (x, (a, xs))

end

(* Test case *)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])


