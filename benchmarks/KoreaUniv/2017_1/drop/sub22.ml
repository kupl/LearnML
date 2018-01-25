(* problem 6*)

let rec drop : 'a list -> int -> 'a list
 = fun l n ->
 if n = 0 then l
 else if (List.length l = 0) then l
 else (drop(match l with a::b -> b)(n-1));;