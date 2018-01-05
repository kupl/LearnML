module type Queue =
 sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
 end

module IntListQ =
 struct
  type element = int list
  type queue = int list list * int list list
  exception EMPTY_Q
  let emptyQ = ([],[])

  let enQ ((l_lst, r_lst), elem) = (elem :: l_lst, r_lst)

  let rec deQ (l_lst,r_lst) =
   match r_lst with
   [] -> (let next_r_lst = List.rev l_lst in
	  match next_r_lst with
	  [] -> raise EMPTY_Q
	  |_ -> (deQ ([], next_r_lst)))
   |h::t -> (let nextQ = (l_lst, t) in
	     (h, nextQ))
 end
