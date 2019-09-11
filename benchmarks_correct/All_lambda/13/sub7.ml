type lambda = V of var
		 | P of var * lambda
		 | C of lambda * lambda
and var = string

let rec isinlst mtr lst =
	match (mtr,lst) with
	|(_, []) -> false
	|(_, _) -> if mtr=(List.hd lst) then true else isinlst mtr (List.tl lst)


let rec check1 mtr lst=
	match mtr with 
		|V st -> isinlst st lst
		|P (var, mt)-> check1 mt (var::lst)
		|C (m1, m2)-> check1 m1 lst && check1 m2 lst

let rec check mtr = check1 mtr []

(*
let t1 = check(P("a", V "a"))
 let t2=check(P("a", P("a", V "a")))
 let t3=check(P("a", P("b", C(V "a", V "b"))))
 let t4=check(P("a", C(V "a", P("b", V "a"))))
 let t5=check(P("a", V "b"))
 let t6=check(P("a", C(V "a", P("b", V "c"))))
 let t7=check(P("a", P("b", C(V "a", V "c"))))

*)
