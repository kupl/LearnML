type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna
exception OutofScreen
exception EncodingError
let pptree tree =  
	let rec count t =
		match t with
			NODE(t1, t2)->if(count t1)>(count t2) then (count t1)+1 else (count t2)+1
			|LEAF(t) -> 1 
		in
	let rec makescreen x y =
		let rec makerow y =
			if y=0 then [] else 0::(makerow (y-1)) 
			in
		if x=0 then [] else (makerow y)::(makescreen (x-1) y)
		in
	let rec set s x y n =
		let rec setrow row y n= 
			match row with h::t -> if y = 0 then n::t 
								   else h::(setrow t (y-1) n)
						   |[]->raise OutofScreen
			in
		match s with h::t -> if x=0 then (setrow h y n)::t
									else h::(set t (x-1) y n)
					 |[]->raise OutofScreen
		in
	let rec pow n = if n=0 then 1 else 2*(pow (n-1)) in
	let screen = makescreen (count tree) ((pow (count tree))+1) in
	let width = (pow (count tree))+1 in
	let depth = (count tree)-1 in
	let rec setnode s t loc d =
		match t with
		NODE(t1, t2)->setnode (setnode (set (set s d loc 1) (d+1) loc 2) t1 (loc - (width/(pow (d+2)))) (d+1)) t2 (loc + (width/(pow (d+2)))) (d+1)
		|LEAF(t1)->if d = depth then (set s d loc 1) else (setnode (set s d loc 1) t loc (d+1))
		in
	let rec coloring row b =
		match row with
			h::t->if b then
				(if h=1 then h::(coloring t false) else 2::(coloring t true))
				else if h=2 then h::(coloring t true)
				else h::(coloring t b)
			|[]->[]
		in
	let colorboth row = (coloring (List.rev (coloring (List.rev row) false)) false) in
	let rec color s =
		match s with h::t->(colorboth h)::(color t)
					 |[]->[]
		in
	let mytree = color (setnode screen tree (width/2) 0) in
	let rec print t = 
		let rec printrow row =
			match row with h::t -> ((if h=0 then print_string " "
								   else if h=1 then print_string "|"
								   else print_string "-"
								   );(printrow t))
						  |[]->print_string "\n"
			in
		match t with h::t->((printrow h);(print t))
					|[]->print_string""
			in
	print mytree
								   
	
	


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
