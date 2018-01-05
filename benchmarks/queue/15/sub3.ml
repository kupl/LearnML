(*
    PL 2-6
    2008-11609 박성원
*)

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
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ = function (q, elem) ->
    match q with
    | (l1, l2) -> (elem :: l1, l2)
  let rec deQ = function q ->
    let rec revList l result =
      match l with
      | [] -> result
      | v :: l -> revList l (v :: result)
    in
    match q with
    | ([], []) -> raise EMPTY_Q
    | (l1, []) -> deQ ([], revList l1 [])
    | (l1, l :: l2) -> (l, (l1, l2))
end
