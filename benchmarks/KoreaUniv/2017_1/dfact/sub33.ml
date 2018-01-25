(* problem 5*)

let rec dfact : int -> int
= fun n -> 
  if n = 1 then 1
  else if n = 2 then 2
  else n * dfact (n-2) ;; 