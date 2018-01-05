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
  type queue = ((element list) * (element list))
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ : queue * element -> queue = fun (x, y) ->
   match x with
   | (x1, x2) -> ((y::x1), x2)
  let rec deQ = fun x ->
   match x with
   | (x1, x2_h::x2_t) -> (x2_h, (x1, x2_t))
   | ([], []) -> raise EMPTY_Q
   | (x1, []) ->
    let rec reverse : int list list -> (int list list -> int list list) = fun x ->
 	 match x with
	 | [] -> (fun y -> y)
	 | x_h::x_t -> (fun y -> ((reverse x_t) (x_h::y))) in
    deQ ([], (reverse x1) [])
end