(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->  
				(*길이계산v*)
				let rec bin_length : bin -> int
				= fun b -> match b with
				| [] -> 0
				| hd::tl -> 1+ (bin_length tl) in

				(*거듭제곱v*)
				let rec expt : int -> int -> int
				= fun b n -> match n with
							| 0 -> 1
							| _ -> match n mod 2 with
								|1 -> b* (expt b ((n-1)/2))*(expt b ((n-1)/2)) 
								|_ -> (expt b (n/2))*(expt b (n/2)) in

				(*십진수로v*)
				let rec btod : bin -> int -> int
				= fun b_bin l -> match b_bin with
								| [] -> 0
								| hd::tl -> match hd with
											| ZERO -> (btod tl (l-1)) 
											| ONE -> ((expt 2 (l-1)) + (btod tl (l-1)) ) in

				(*이진수로*)
				let rec dtob : int -> bin ->bin
				= fun dec result -> match dec with
									| 0 -> result
									|_ -> match dec mod 2 with
											| 0 -> (dtob (dec/2) (ZERO::result))
											| 1 -> (dtob (dec/2) (ONE::result)) in
				(*진짜 계산*)
				(dtob ( (btod b1 ((bin_length b1))) * (btod b2 ((bin_length b2))) ) []);;