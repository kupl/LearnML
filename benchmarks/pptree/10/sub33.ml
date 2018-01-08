type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

exception Error of string
exception Errorgeth of string * int

let first p = match p with
	(a, b, c) -> a
let second p = match p with
	(a, b, c) -> b
let third p = match p with
	(a, b, c) -> c
	
let rec geth n = if n = 1 then 0
		else if n <= 0 then raise (Errorgeth ("geth", n))
		else 2*(geth (n-1))+1

let rec getheight tn = match tn with
	LEAF t -> 1
	| NODE (t1, t2) -> let ht1 = (getheight t1) in
			let ht2 = (getheight t2) in
			if (ht1 > ht2) then 1+ht1
			else 1+ht2
let rec coordinate l height currenth= match l with
			[] -> (height-currenth,(geth height),"|")
			| (h::t) -> (let before = (second (coordinate t height (currenth+1))) in
					match h with 0 ->((height-currenth),before - (((geth (currenth+1))/2)+1), "|")
							| 1 -> ((height-currenth),before + (((geth (currenth+1))/2)+1),"|")
							| _ ->  raise (Error "coordinate"))

let rec leaflist a b c h= if (a > h) then []
			else if (a = h) then [(a, b, c)]
			else (a, b, c) :: (leaflist (a+1) b c h)

let rec getlist tn lst h ch = match tn with
				NODE (t1, t2) -> (coordinate lst h ch) :: ((getlist t1 (0::lst) h (ch-1)) @ (getlist t2 (1::lst) h (ch-1)))
				| LEAF t -> (match (coordinate lst h ch) with
						(a, b, c) -> (leaflist a b c (h-1)))

let forfilter ch x = if (first x) = ch then true
			else false

let rec getx the lst = match the with
		(a, b) -> if (b < 0) then raise (Error "getx")
			else if (List.mem (a, b, "|") lst) then b
			else (getx (a, b-1) lst)
let rec dashs a b c = if b>c then []
			else if b=c then [(a, b, "-")]
			else (a, b, "-") :: (dashs a (b+1) c)

let getsub lst x = match x with
		(a, b, c) -> let d = (b-(getx (a+1, b) lst)) in
				if (d = 0) then []
				else (dashs (a+1) (b-d+1) (b+d-1))

let rec getlist2 lst h ch = if (ch > (h - 2)) then []
		else (let fchlst = (List.filter (forfilter ch) lst) in
			(List.flatten (List.map (getsub lst) fchlst)) @ (getlist2 lst h (ch+1)))

let rec space l r = if (not ((fst l)=(fst r))) then raise (Error "space")
			else if (snd l) > (snd r) then []
			else if (snd l) = (snd r) then
			match l with
			(a, b) -> [(a, b, " ")]
			else match l with
			(a, b) -> ((a, b, " ")::(space (a, (b+1)) r))

let forfilter2 lst x = match x with
			(a, b, c) -> if (List.mem (a, b, "|") lst) || (List.mem (a, b, "-") lst) then false
			else true

let rec getlist3 lst h ch = (let last = (second (List.hd (List.rev (List.sort compare lst)))) in
				if (ch > h-1) then []
				else (List.filter (forfilter2 lst) (space (ch, 0) (ch,last))) @ (getlist3 lst h (ch+1)))
			
let rec get tn = (let height = (getheight tn) in
			let l1 = (List.sort compare (getlist tn [] height height)) in
			let l2 = (List.sort compare (getlist2 l1 height 0)) in
			let l3 = (List.sort compare (l1@l2)) in
			let l4 = (List.sort compare ((getlist3 l3 height 0) @ l3)) in
			l4)

let rec printget lst = match lst with
			[] -> ()
			| (a, b, c) :: t -> (let last = (second (List.hd (List.rev lst))) in
						if (b = last) then ((print_string c);(print_string "\n");(printget t))
						else ((print_string c);(printget t)))

let pptree tn = (printget (get tn))
