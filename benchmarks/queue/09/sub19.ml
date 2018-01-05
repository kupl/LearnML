(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW2_Exercise6.ml *)

(*Exercise 6*)

module IntListQ=
	struct
		type element = int list
		type queue = (element list) * (element list)
		exception EMPTY_Q
		let emptyQ =
			([],[])
		let enQ ((q:queue), (e:element)) =
			match q with
				 (left, right) -> (e::left, right) (*e is also list od int *)
		
		let deQ (qe:queue) =
			let help (q:queue)=
			 match q with
			   (left, right) -> (
					     match right with
					      [] ->  ([],(List.rev left))
					     |_ -> q
					     )
		   in
			    match (help qe) with
			    (left, a::right) -> (a,(left, right))
			    |_-> raise EMPTY_Q
		end
;;

