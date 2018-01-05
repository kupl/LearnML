
let rec merge (list0, list1) =
	match (list0, list1) with
	|	([], l) | (l, []) -> l
	|	(e1::l0, e2::l1) ->
		if e1 > e2 then e1::merge(l0, list1)
		else e2::merge(list0, l1)
	;;

(* exercise test
let test_list0 = [5;4;3;2;1;-1];;
let test_list1 = [4;3;1;0];;

let print_list plist =
	let print_elem elem = Printf.printf "elem : %d\n" elem in
	List.iter print_elem plist;;

print_list( merge (test_list0, test_list1) );;
exercise *)
