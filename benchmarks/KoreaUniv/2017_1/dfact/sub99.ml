(* problem 5*)

let rec dfact : int -> int
= fun n -> if n < 0 then raise(Failure "Negative") else match (n-1)/2 with
|0 -> n
|_ -> n * dfact (n-2)