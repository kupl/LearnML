let rec subtest : int * int -> bool
= fun (n1, n2) ->
	if n2*n2 > n1 then true
	else if n1 - n1/n2*n2 = 0 then false
	else subtest(n1, n2+1)

(* Problem 3 *)
let rec prime : int -> bool
(* = fun n -> let i = ref 1 -> let status = ref true ->
while !i < n and n % !i != 0 do
	i := !i + 1;
done -> match i with
| !n -> true
| _ -> false
*)
= fun n -> subtest(n, 2)
