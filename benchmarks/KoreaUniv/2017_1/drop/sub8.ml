(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
[]->[]
|_::t ->if n=1 then t else drop t (n-1);;
