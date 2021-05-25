(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)
exception Error of string

let rec iter (n, f) =
  let fun_composit f1 f2 =
    fun x -> f1 (f2 x)
  in

  let identity x =
    x
  in

    if n < 0 then
      raise (Error "# of iter is less than 0")
    else if n = 0 then 
      identity
    else
      fun_composit f (iter ((n-1), f))

(*
;;
iter ((-1), (fun n -> n * n)) 1234;;    (* Exception *)
iter (0, (fun n -> n * n)) 1234;;       (* 1234 *)
iter (2, (fun n -> n * n)) 2;;          (* 16 *)
iter (100, ((+) 2)) 0;;                 (* 200 *)
iter (10000, ((+) 2)) 0;;               (* 20000 *)
iter (2, List.tl) [1;2;3;4];;           (* [3; 4] *)
*)
