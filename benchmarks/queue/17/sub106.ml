(*real code start*)
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
  type queue = int list list * int list list
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ = fun (q1, e1) -> 
   match q1 with
   |(x,y) -> (e1::x,y)
  let deQ = fun (q1) ->
   match q1 with
   |([],[]) -> raise EMPTY_Q
   |(x,[]) -> let xrev = List.rev x in
     (List.hd(xrev) ,([], List.tl(xrev)))
   |(x,hd::tl) -> (hd, (x, tl))
end
(*real code end*)
