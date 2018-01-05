(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 6
 *)


module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end;;



module IntListQ =
struct
  type element = int list
  type queue = int list * int list
  exception EMPTY_Q
  let emptyQ = ([],[])
  let rec enQ = (fun ((lq,rq),e) -> 
		   match e with
		       [] -> (lq,rq)
		     | hd::tl -> (enQ ((hd::lq,rq),tl))
		)
  let rec deQ = (fun (lq,rq) ->
		   match rq with
		       [] -> 
			 if lq=[] 
			 then raise EMPTY_Q
			 else deQ (rq,List.rev lq)
		     | hd::tl -> ([hd],(lq,tl))
		)
end;;

(*
let myQ = IntListQ.emptyQ;;
let yourQ = IntListQ.enQ(myQ, [1]);;
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ,[2]);;
open IntListQ;;
let right(a,b) =b;;
deQ(right(deQ(enQ(myQ,[1;2;3;4]))));;
deQ(right(deQ(right(deQ(enQ(myQ,[1;2;3;4]))))));;
deQ(right(deQ(right(deQ(enQ(myQ,[1;2]))))));;
deQ(enQ(right(deQ(right(deQ(right(deQ(enQ(myQ,[1;2;3;4]))))))),[9;8]));;
deQ(right(deQ(enQ(right(deQ(right(deQ(right(deQ(enQ(myQ,[1;2;3;4]))))))),[9;8]))));;

enQ(myQ,[1;2;3]);;
enQ(enQ(myQ,[1;2;3]),[4;5]);;
*)
