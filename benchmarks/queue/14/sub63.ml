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
  type queue = (element list)*(element list)
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ (myQ,ele) = match myQ with
  |(l1,l2) -> (ele::l1,l2)
  let rec deQ myQ = match myQ with
  |([],[]) -> raise (EMPTY_Q)
  |(l,[]) -> deQ ([],List.rev l)
  |(l1,x::l2) -> (x,([],List.append l2 (List.rev l1)))
 end


