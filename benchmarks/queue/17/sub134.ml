(*
 * 2017 - 09 - 22
 * PL Homework 2-6
 * Joonmo Yang
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
    type queue = element * element
    exception EMPTY_Q
    let emptyQ = [], []
    let enQ =
      fun (q, elem) ->
      ( match q with
        | l, r -> elem::l, r
      )
    let rec deQ =
      fun q ->
      ( match q with
        | [], [] -> raise EMPTY_Q
        | l, r -> if (List.length l) <= (List.length r) then (List.hd r, (l, List.tl r))
                  else deQ ((List.rev (List.tl (List.rev l))), (List.rev ((List.hd (List.rev l))::(List.rev r))))
      )
  end

(* test cases
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])

let q1 = IntListQ.emptyQ 
let q2 = IntListQ.enQ(q1, [1]) 
let q3 = IntListQ.enQ(q2, [2;3]) 
let q4 = IntListQ.enQ(q3, [4;5;6]) 
let (l1, q5) = IntListQ.deQ q4 
let q6 = IntListQ.enQ(q5, [7;8;9;10]) 
let (l2, q7) = IntListQ.deQ q6 
let q8 = IntListQ.enQ(q7, [11;13;20;100]) 
let (l3, q9) = IntListQ.deQ q8 
let (l4, q10) = IntListQ.deQ q9 
let (l5, q11) = IntListQ.deQ q10 
let q12 = IntListQ.enQ(q11, [4;5;6;7]) 
let (l6, q13) = IntListQ.deQ q12 

let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case (1, ([1] = l1)); 
  test_case (2, ([2;3] = l2)); 
  test_case (3, ([4;5;6] = l3)); 
  test_case (4, ([7;8;9;10] = l4)); 
  test_case (5, ([11;13;20;100] = l5)); 
  test_case (6, ([4;5;6;7] = l6)); 
  test_case (7, q13 = IntListQ.emptyQ) 

let (x, y) = try IntListQ.deQ q13 with IntListQ.EMPTY_Q -> ([19682934], IntListQ.emptyQ) 
let _ = if(x = [19682934]) then print_endline ("Error Case : Pass") 
  else print_endline("Error Case : Failure")
*)
