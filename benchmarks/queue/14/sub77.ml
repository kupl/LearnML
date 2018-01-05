(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 30, 2014
 *)

(* Exercise 5 : Queue = 2 Stacks *)
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
  let emptyQ = ([], [])
  let enQ : queue * element -> queue = fun (q,elem) ->
   	let l, r = q in
   	(elem::l, r)
  let deQ : queue -> element * queue = function
   	| l, hd::r   -> hd, (l, r)
   	| l::tl, []  -> (match (List.rev (l::tl)) with hd::tl -> (hd, ([], tl)) | _ -> raise EMPTY_Q)
   	| [], []     -> raise EMPTY_Q
end
