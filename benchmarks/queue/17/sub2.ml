(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-14
  Homework-# : 2-6
  Excercise-Name : Queue = 2 Stacks
*)
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
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ : queue = ([],[])
  let enQ (((lSt, rSt), ele):(queue*element)) : queue = (
    let newLSt = ele::lSt in
    (newLSt, rSt)
  )
  let deQ ((lSt,rSt):queue) : (element*queue) = (
    match (lSt,rSt) with
    | (lSt,[]) -> (
      let revLSt = List.rev lSt in
      match revLSt with 
       | [] -> raise EMPTY_Q
       | hd::tl -> (hd, ([],tl))
    )
    | (lSt, hd::tl) -> (hd, (lSt, tl))
  )
end

(*
module ValidIntListQ = (IntListQ : Queue)
*)
