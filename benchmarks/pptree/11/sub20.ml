type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let pptree tour =

	let rec check(t, n) =
                match t with
                        | LEAF a -> n
                        | NODE(a, b) -> (max (check(a, (n+1))) (check(b, (n+1))))
	in

	let rec exp(n, res) =
		if (n=0) then res
		else exp((n-1), 2*res)
	in
	let makespace n =
		let rec space m =
			if (m=0) then ""
			else (" " ^ space(m-1))
		in
		space(exp(n,1) - 1)
	in
	let makeline n =
		let rec line m =
			if (m=0) then ""
			else ("-" ^ line(m-1))
		in
		line(exp(n,1) - 1)
	in

	let rec makestring(t, l, n, m, str,x,c) =
		if (m = 1) then (match t with
					| LEAF a -> makespace(l-x) ^ "|"
					| NODE (a, b) -> (makespace(l-n) ^ "|" ^ makeline(l-n+1) ^ "|"))
		else (match t with
				| LEAF a -> makestring(t, l, n, (m-1), "", (if(c=0) then m else x-1),1) ^ makespace(l-x)
				| NODE (a, b) -> (match (a, b) with
							| (NODE(s,t),NODE(p,q)) -> (makestring(a, l, n, (m-1), "",x,0) ^ makespace(l-n) ^ " " ^ makestring(b, l, n, (m-1), "",x,0))
							| (NODE(s,t), LEAF p) -> (makestring(a, l, n, (m-1), "",x,0) ^ makespace(l-n+1) ^ makespace(m-2) ^ makestring(b, l, n, (m-1), "",(if (c=0) then (n-1) else x),1))
							| (LEAF p, NODE(s,t)) -> (makestring(a, l, n, (m-1), "",(if (c=0) then (n-1) else x),1) ^ " " ^ makespace(l-n+1) ^ makespace(m-2) ^ makestring(b, l, n, (m-1), "",x,0))
							| (LEAF p, LEAF q) -> (makestring(a,l,n,(m-1),"",(if (c=0) then (n-1) else x),1) ^ makespace(l-n+1) ^ makespace(m-2) ^ makestring(b,l,n,(m-1),"",(if (c=0) then (n-1) else x),1))))
	in
	
	let rec print(t, n, m) =
		if (m=0) then (makespace(n) ^ "|")
		else (print(t, n, (m - 1)) ^ "\n" ^ makestring(t,n,m,m,"",m,0))
	in
	print_string(print(tour, check(tour,0), check(tour,0)))

