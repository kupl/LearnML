(*zipper*)
(*http://ropas.snu.ac.kr/~kwang/4190.310/09/hw1.pdf 3번*)
let rec zipper : int list * int list -> int list
= fun (l1, l2) ->
	match l1 with
	|[] -> l2
	|hd::tl -> hd::(zipper (l2, tl))