(* ex 3 *)
let rec iter = fun (n, f) ->
  if (n > 0) then
    fun x -> f (iter(n-1, f) x)
  else fun x -> x

(* ex 3 test *)
(* let _ = print_endline(string_of_int (iter(5, fun x -> 2*x) 10)) *)
