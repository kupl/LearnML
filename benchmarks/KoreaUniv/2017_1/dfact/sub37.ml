(* problem 5*)

let rec dfact : int -> int
= fun n ->
   if n < 0 then raise (Failure "n has to be non-negative") (*exlcude the case when n is negative*)
   else
      match n with
	0 -> 1
	|1 -> 1 (*by the mathematical definition, 0!! or 1!! is 1*)
	|_ -> n * dfact(n - 2);; (*when n is larger than 1, call the function "dfact" recursively, following the rule of getting the value of double factorial, til n becomes 0 or 1*)