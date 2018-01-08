type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

exception STRANGE of string

type prt = SINGLE of int | DOUBLE of int * int

let pptree tre = 
	let rec level t =
		match t with
			| LEAF(_) -> (1,1)
			| NODE(a,b) -> 
				let (c,d) = (max (level a) (level b)) in
				(c+1,2*d)
	in
	let (lv,len) = (level tre) in
	let str = String.make (lv*len*2+1) ' ' in
	let rec newlines n =
		if n>0 then (str.[n*len*2-1] <- '\n'; (newlines (n-1)))
	in

	newlines lv;

	let rec draw t x y =
		let _,hor = (level t) in

		let rec drawdash x1 x2 =
			str.[x1] <- '-';
			if x1!=x2 then drawdash (x1+1) x2
		in
		let rec drawvert x y =
			str.[x+y*len*2] <- '|';
			if y+1<lv then drawvert x (y+1)
		in
		str.[x+(y-1)*len*2] <- '|';
		match t with
			| LEAF(_) -> drawvert x (y-1)
			| NODE(a,b) ->
				str.[x+y*len*2-(hor/2)] <- '|';
				str.[x+y*len*2+(hor/2)] <- '|';
				drawdash (x+y*len*2-(hor/2)+1) (x+y*len*2+(hor/2)-1);
				draw a (x-(hor/2)) (y+1); draw b (x+(hor/2)) (y+1)
	in

	draw tre (len-1) 1;
	print_string str