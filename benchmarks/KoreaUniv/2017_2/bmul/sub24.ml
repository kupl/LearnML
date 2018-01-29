(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec dig_mul : bin -> digit -> bin
= fun b d ->
	if d = ZERO then [ZERO]
	else (
		match b with
		| hd::tl -> (
			if hd = ZERO then ZERO::(dig_mul tl d)
			else ONE::(dig_mul tl d)
		)
		| [] -> []
	)
;;

let rec dig_mul_all : bin -> bin -> int -> bin list
= fun b1 b2 zero_tl->
	match b2 with
	| hd::tl -> (
		let mul_result = ref (dig_mul b1 hd) in
		if !mul_result != [] then (
			for i = 1 to zero_tl do
				mul_result := (!mul_result)@[ZERO]
			done;
			(!mul_result)::(dig_mul_all b1 tl (zero_tl - 1))
		)
		else (dig_mul_all b1 tl (zero_tl - 1))
	)
	| [] -> []
;;

let dig_sum : (digit * digit * digit) -> (digit * digit)
= fun (d1, d2, remain) ->
	let count = ref 0 in
	if d1 = ONE then count := !count + 1;
	if d2 = ONE then count := !count + 1;
	if remain = ONE then count := !count + 1;
	if !count = 0 then (ZERO, ZERO)
	else if !count = 1 then (ONE, ZERO)
	else if !count = 2 then (ZERO, ONE)
	else (ONE, ONE)
;;

let rec dig_sum_list : bin -> bin -> digit -> bin
= fun b1 b2 remain ->
	match b1 with
	| hd1::tl1 -> (
		match b2 with
		| hd2::tl2 -> (
			let (result, new_remain) = dig_sum (hd1, hd2, remain) in
			result::(dig_sum_list tl1 tl2 new_remain)
		)
		| [] -> (
			let (result, new_remain) = dig_sum (hd1, ZERO, remain) in
			result::(dig_sum_list tl1 b2 new_remain)
		)
	)
	| [] -> (
		match b2 with
		| hd2::tl2 -> (
			let (result, new_remain) = dig_sum (ZERO, hd2, remain) in
			result::(dig_sum_list b1 tl2 new_remain)
		)
		| [] -> (
			if remain = ONE then [ONE]
			else []
		)
	)
;;

let rec dig_sum_list_rec : bin -> bin list -> bin
= fun b l ->
	match l with
	| hd::tl -> (
		let sum = dig_sum_list b hd ZERO in
		dig_sum_list_rec sum tl
	)
	| [] -> b
;;

let rec rev_list : bin list -> bin list
= fun l ->
	match l with
	| hd::tl -> (List.rev hd)::(rev_list tl)
	| [] -> []
;;

let rec erase_zero_head : bin -> bin
= fun b ->
	match b with
	| [ZERO] -> b
	| hd::tl -> if hd = ZERO then erase_zero_head tl else b
	| [] -> []
;;

let bmul : bin -> bin -> bin
= fun b1 b2 ->
	if b1 = [] || b2 = [] then raise (Failure "Empty list error")
	else
	(
		let b_list = dig_mul_all b1 b2 ((List.length b2)-1) in
		let rev_b_list = rev_list b_list in
		match rev_b_list with
		| hd::tl -> erase_zero_head (List.rev (dig_sum_list_rec hd tl))
		| [] -> raise (Failure "Empty list error")
	)
;;