(* Homework 2 - Exercise 6
 * 2011-10492 Jaeyeong Yang *)
module IntListQ =
  struct
    type element = int list
    type queue = element list * element list

    exception EMPTY_Q

    let emptyQ: queue = ([], [])

    let enQ: queue * element -> queue = fun (q, e) ->
      match q with
      | (l, r) -> (e :: l, r)

    let deQ: queue -> element * queue = fun q ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | (l, []) ->
        let r = List.rev l in
        (match r with
         | [] -> raise EMPTY_Q
         | h :: t -> (h, ([], t)))
      | ([], h :: t) -> 
        (h, ([], t))
      | (l, h :: t) ->
        (h, ([], List.append t (List.rev l)))
  end

let myQ = IntListQ.emptyQ

let yourQ = IntListQ.enQ(myQ, [1])

let (x, restQ) = IntListQ.deQ yourQ

let hisQ = IntListQ.enQ(myQ, [2])
