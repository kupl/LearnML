module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

exception EMPTY_Q

module IntListQ = struct
  type element = int list
  type queue = (int list) list * (int list) list (* list of "integer lists" *)
    (* enqueue in left , dequeue from right, if right empty, reverse left as right, then dequeue*)
  exception EMPTY_Q
  let emptyQ : queue = ([],[])
  let enQ (q , elem) : queue = match q with
    | (left, right) -> (elem :: left, right)
  let rec deQ q : element * queue = match q with
    | (left, right) -> (match right with
      | [] -> (match left with
        | [] -> raise EMPTY_Q
        | _ -> deQ ([], List.rev left)
        )
      | hd :: tl -> (hd, (left, tl))
      )
end

(**
module ValidIntListQ = (IntListQ : Queue)
*)
(** Testcases *)
(**
let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6])
let (e1, q3) = IntListQ.deQ q2    ;;
*)
