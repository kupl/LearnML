module type Queue =
 sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
 end
;;

module IntListQ: Queue with type element = int list = 
 struct
  type element = int list
  type queue =  element list * element list
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ input =
   match input with
   |((lq,rq), inp) -> (inp::lq,rq)
   
  let deQ q = 
   let (a,b) = q in
   let newq = ([],b@(List.rev a)) in
   match newq with
   |([],hd::tl) -> (hd,([],tl))
   |([],[]) -> raise EMPTY_Q
   |_ -> raise EMPTY_Q
 end
;;
