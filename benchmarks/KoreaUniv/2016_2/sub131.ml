(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
		[] -> failwith "Empty list"
		|head::tail -> 
			(* 하나만 남은 경우 출력 *)
			if (List.length lst = 1) then head 
			else
				(* 리스트의 제일 처음값이 두번째보다 크면 두번째를 뺀 리스트를 다시 재귀돌림 *)
				if(head >= List.nth tail 0) then max( head::(List.tl tail) )

				(* 리스트의 제일 처음값이 두번째보다 작으면 첫번째를 뺀 리스트를 다시 재귀돌림 *)
				else max tail


let rec min : int list -> int
= fun lst ->  (* TODO *)
	match lst with
			[] -> failwith "Empty list"
			|head::tail -> 
				(* 하나만 남은 경우 출력 *)
				if (List.length lst = 1) then head 
				else
					(* 리스트의 제일 처음값이 두번째보다 작으면 두번째를 뺀 리스트를 다시 재귀돌림 *)
					if(head <= List.nth tail 0) then min( head::(List.tl tail) )

					(* 리스트의 제일 처음값이 두번째보다 크면 첫번째를 뺀 리스트를 다시 재귀돌림 *)
					else min tail


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = (* TODO *)
	match lst with
	|[] -> []
	|head::tail -> if pred head then head::(filter pred tail)
				   else filter pred tail



(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  (* TODO *)
	f (f a)
	




(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree


(*     
(*테스트용*)     
let t2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
let t3 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty) ), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
*)

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
	match tree with
	|Empty -> false
	|Node(mid, left , right) -> 

	if ( n=mid ) then true 
	else ( mem n left || mem n right )






(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

(* 
(*테스트용*)
 let one = SUCC ZERO
 let two = SUCC (SUCC ZERO)
 let three = SUCC (SUCC (SUCC ZERO))
*)

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)

	match n1 with
	|ZERO -> n2
	|SUCC( subn1 ) -> SUCC( natadd subn1 n2 ) (*subn1은 n1에서 1을뺀값*)







let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	if(n1=ZERO || n2=ZERO) then ZERO
	else
	
	(*내부에 변수 3개짜리 재귀함수를 만들어서 더하는 값을 일정하게 유지시킨 채 n2값을 불려나간다*)
	let rec innerLoop : nat -> nat -> nat -> nat
	= fun n1 n2 maintain ->
			
			match n1 with
			|SUCC ZERO -> n2
			|SUCC( sub_count ) -> innerLoop sub_count (natadd maintain n2) maintain	

	in innerLoop n1 n2 n2




(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 



(*테스트용*)
let one = Num 1
let two = Num 2
let three = Num 3
let four = Num 4



let rec eval : formula -> bool
= fun f -> (* TODO *)

	match f with
	|True -> true
	|False -> false

	|Not( inNot ) -> if (eval inNot) then false else true
	
	|AndAlso(and1, and2) -> if (eval and1) && (eval and2) then true else false 
	
	|OrElse(or1, or2) -> if (eval or1) || (eval or2) then true else false
	
	|Imply(im1, im2) -> 
		(	match (eval im1) with
			|true -> if (eval im2) then true else false
			|false -> true 
		)

	|Equal(exp1, exp2) ->

		(*두가지의 exp를 받아서 같은지 다른지를 확인시켜주는 재귀함수*)
		let rec expcheck : exp -> int
		= fun e ->

			match e with
			|Num( inNum ) -> inNum
			|Plus(p1, p2) -> ( (expcheck p1) + (expcheck p2) )
			|Minus(m1, m2) -> ( (expcheck m1) - (expcheck m2) )

		in if (expcheck exp1)=(expcheck exp2) then true else false


