let rec merge : int list * int list -> int list = fun (l1,l2) ->
	if List.length(l1)==0 then l2
	else if List.length(l2)==0 then l1
	else if List.hd(l1)>List.hd(l2) then List.hd(l1)::merge(List.tl(l1),l2)
	else List.hd(l2)::merge(l1,List.tl(l2))

(*
let l1 = [9;7;6;4;1]
let l2 = [11;10;8;5;3;2]
let l=merge(l1,l2)
let _ = print_endline(string_of_bool (l=[11;10;9;8;7;6;5;4;3;2;1]))


let l1 = [4]
let l2 = [5]
let l=merge(l1,l2)
let _ = print_endline(string_of_bool (l=[5;4]))

let l1 = []
let l2 = []
let l=merge(l1,l2)
let _ = print_endline(string_of_bool (l=[]))
*)
