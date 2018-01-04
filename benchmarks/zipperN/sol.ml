(*zipperN*)
(*http://ropas.snu.ac.kr/~kwang/4190.310/09/hw1.pdf 4번*)
let rec zipperN : int list list -> int list
= fun l ->
	match l with
	|[] -> []
	|h_l::tl -> 
		begin match h_l with
		|[] -> zipperN tl
		|h::t -> h::(zipperN (tl@[t]))
		end