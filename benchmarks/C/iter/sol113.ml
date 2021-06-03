(* 2010-11753 snucse Taekmin Kim *)
(* HW 1-3 *)

let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun(n, fn) ->
  if n == 0 then
    fun x -> x
  else
    fun x -> iter(n - 1, fn) (fn x)

(*
let _ = print_endline(string_of_int(iter(3, fun x -> 2 * x) 1))
let _ = print_endline(string_of_int(iter(3, fun x -> 2 + x) 0))
*)
