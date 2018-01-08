type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna


let rec get_depth tr = 
	match tr with
	LEAF n -> 0
	| NODE (a, b) -> (max (get_depth a) (get_depth b))+1





let rec get_value tr f n width = 
	if f = 0 then
	(*	(match tr with
		LEAF n ->[0;0]
		| NODE(t1, t2) -> [1;1])*) [1;1]
	else if n <= (width/2) then
		(match tr with
		LEAF n -> [0;0]
		| NODE(t1, t2) -> get_value t1 (f-1) n (width/2))
	else
		(match tr with
			LEAF n -> [0;0]
			|NODE(t1, t2) -> get_value t2 (f-1) (n-(width/2)) (width/2))

let rec make_floor tr floor n length = 
	if n > length
		then []
	else
		List.append (get_value tr floor n length)  (make_floor tr floor (n+1) length)



let rec make_list tr floor level length = 
	if floor > level 
		then []
	else
		(make_floor tr floor 1 length)::(make_list tr (floor+1) level (length*2))

let rec tree_to_list tr = 
	make_list tr 1 (get_depth tr) 1

	
(*
let print_floor lt nth space = 
	match lt with
	[] -> ()
	| h::t ->
	| h1::h2::t ->
		if h1 = 1
			then (print " " (2*space-1); print "|" 1; print "-" (4*space-1); print"|" 1; print " " (2*space-1))
*)


let rec print str n = 
	if n <= 0
		then ()
	else
		(print_string str; print str (n-1))
	
let rec cut lt st fin = 
	cut2 lt st fin 1
	and cut2 lt st fin n = 
		if n < st then
			cut2 lt st fin (n+1)
		else if n > fin then
			[]
		else
			(List.nth lt (n-1))::(cut2 lt st fin (n+1))


		


let rec is_zero lt = 
	match lt with
	[] -> true
	|h::t -> if h = 0 then is_zero t else false

let rec exp a b = 
	if b = 0 then
		1
	else if b = 1 then
		a
	else
		a * (exp a (b-1))

let rec space_calculator n space = 
	if n > space then
		0
	else
		n + (space_calculator (n*2) space)
let rec print_tree lt space width start = 
	if width = 1 then
		((if start = 1 then (print " " (space-1);) else());
		if lt = [1;1] then
			(print "|" 1; print "-" (2*space-1); print "|" 1; print " " (2*space-1))
		else
			(print " " (space_calculator (space) (width*space));
			print "|" 1;
			print " " ((space_calculator (space) (space*width))+(2*space-1))))
	else
		(if is_zero lt then
			((if start = 1 then (print " " (space-1);) else());
			(print " " (space_calculator (space) (width*space));
			print "|" 1;
			print " " ((space_calculator (space) (space*width))+(2*space-1))))
		else
			(print_tree (cut lt 1 width) space (width/2) start;
			print_tree (cut lt (width+1) (width*2)) space (width/2) 0))



let rec pptree2 lt floor depth = 
		if floor > depth then
			()
		else
			(print_tree (List.nth lt (floor-1)) (exp 2 (depth-floor)) (exp 2 (floor-1)) 1 ;
			print "\n" 1;
			pptree2 lt (floor+1) depth )
	

let pptree tr = 
	print " " ((exp 2 (get_depth tr))-1);print "|" 1;print "\n" 1;
	pptree2 (tree_to_list tr) 1 (get_depth tr) 

	





let a = LEAF Korea 
let b = LEAF Japan 
let c = NODE (a, b) 
let d = NODE (c, a) 
let e = NODE (b, c) 
let f = NODE (c, c) 
let g = NODE (d, a) 
let h = NODE (a, d) 
let i = NODE (a, g) 
let j = NODE (h, d) 
let k = NODE (i, j) 
let l = NODE (c, c) 
let m = NODE (l, l) 
let n = NODE (m, m) 
let o = NODE (n, n) 
let p = NODE (o, o) 


let t = (NODE (LEAF Korea, LEAF Korea))
let t2 = NODE (t, LEAF Korea)
let t3 = NODE (t2, LEAF Korea)
let t4 = NODE (t3, LEAF Korea)
let t5 = NODE (t4, LEAF Korea)

let q = (NODE (LEAF Korea, LEAF Korea))
let q2 = NODE (q, q)
let q3 = NODE (q2, q2)
let q3 = NODE (q2, q2)
let q4 = NODE (q3, q3)
let q5 = NODE (q4, q4)


let r =  NODE (LEAF Korea, LEAF Korea)
let r2 = NODE (LEAF Korea, r)
let r3 = NODE (LEAF Korea, r2)
let r4 = NODE (LEAF Korea, r3)
let r5 = NODE (LEAF Korea, r4)


let l = tree_to_list r
let l2 = tree_to_list r2
let l3 = tree_to_list r3
