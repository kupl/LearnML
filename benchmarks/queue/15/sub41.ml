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

let rec printlist = fun l ->
 match l with
 | [] -> ()
 | e::l -> print_int(e); print_string " "; printlist(l)

let rec printQ = fun (l1,l2) -> 
 match l2 with
 | e::l -> printlist(List.hd l2); print_string "\n"; printQ(l1,(List.tl l2))
 | [] -> if l1==[] then ()
 else printQ([],(List.rev l1))