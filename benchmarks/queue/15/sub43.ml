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
 let enQ((l1,l2),e) = ((e::l1),l2)
 let rec deQ(l1,l2) = 
  if l2!=[] then ((List.hd l2),(l1,(List.tl l2)))
  else if l1!=[] then deQ([],(List.rev l1))
  else raise EMPTY_Q
end