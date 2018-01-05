(*
module type Queue =
 sig
  type element
  type queue 
  exception EMPTY Q 
  val emptyQ: queue	
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
 end
*)
let rec reverse l =
  match l with
   [] -> []
   | hd :: tl -> (reverse tl) @ [hd]


module IntListQ =
 struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ (q, e) =
  match q with
  (q1, q2) -> (e::q1, q2)

  let deQ q =
  match q with
  ([], []) -> raise EMPTY_Q
  |(q1, []) ->(List.hd(reverse q1), ([], List.tl(reverse q1)))
  |(q1, h::q2) -> (h, (q1, q2)) 
 end
(*
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
