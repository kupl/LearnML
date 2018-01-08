
(* 2008-11720 Á¶°Ü¸® *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
        | Poland | Portugal | Italy | Germany | Sweden | England
        | Croatia | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

type tourna2 = A of int
			| L
			| N of tourna2 * tourna2

exception Error


let pptree tour =

let iftwo a =
	let loga2=(log (float a))/.(log 2.0)
	in 
	if (loga2=(floor loga2)) then true else false
in

(* return reversed tlist with float N=0, L=1, A=2^n *)
let rec makeTlist tqueue tlist height r c = 
	if (height=r) then tlist else
	(match tqueue with
	[] -> tlist
	| h::t -> 
		if ((iftwo c)&&(iftwo (c+1))) then
   		(match h with 
		 A i -> (makeTlist (t@[A (i*2); A (i*2)]) ([i*2; i*2]::tlist) height (r+1) (c+1))
		 | L -> (makeTlist (t@[A 2; A 2]) ([2; 2]::tlist) height (r+1) (c+1))
		 | N (t1, t2) -> 
		 	(match t1 with 
			 	A _ -> raise Error
		   		| L -> 
					(match t2 with 
					 A _ -> raise Error
					| L -> (makeTlist (t@[t1; t2]) ([1;-1;1;-2]::tlist) height (r+1) (c+1))
					| N (_, _) -> (makeTlist (t@[t1; t2]) ([0;-1;1;-2]::tlist) height (r+1) (c+1)))
				| N (_, _) ->
			   		(match t2 with
					 A _ -> raise Error
				    | L -> (makeTlist (t@[t1; t2]) ([1;-1;0;-2]::tlist) height (r+1) (c+1))
				    | N (_, _) -> (makeTlist (t@[t1; t2]) ([0;-1;0;-2]::tlist) height (r+1) (c+1)))
			))
		else if (iftwo c) then
		(match h with
		 A i -> (makeTlist (t@[A (i*2); A (i*2)]) ([i*2; i*2]::tlist) height r (c+1))
		 | L -> (makeTlist (t@[A 2; A 2]) ([2; 2]::tlist) height r (c+1))
		 | N (t1, t2) ->
			(match t1 with
			 	A _ -> raise Error
			    | L ->
					(match t2 with
					 A _ -> raise Error
					| L -> (makeTlist (t@[t1; t2]) ([1;-1;1;-2]::tlist) height r (c+1))
					| N (_, _) -> (makeTlist (t@[t1; t2]) ([0;-1;1;-2]::tlist) height r (c+1)))
				| N (_, _) ->
				    (match t2 with
					 A _-> raise Error
				    | L -> (makeTlist (t@[t1; t2]) ([1;-1; 0;-2]::tlist) height r (c+1))
				    | N (_, _) -> (makeTlist (t@[t1; t2]) ([0;-1;0;-2]::tlist) height r (c+1)))
			))

		else if (iftwo (c+1)) then
		(match tlist with
		 [] -> raise Error
		| head::tail ->
		(match h with
         A i -> (makeTlist (t@[A (i*2); A (i*2)]) (([i*2; i*2]@head)::tail) height (r+1) (c+1))
	     | L -> (makeTlist (t@[A 2; A 2]) (([2; 2]@head)::tail) height (r+1) (c+1))
	     | N (t1, t2) ->
		     (match t1 with
			  	A _ -> raise Error
			    | L ->
				    (match t2 with
					 A _ -> raise Error
				    | L -> (makeTlist (t@[t1; t2]) (([1;-1; 1;-2]@head)::tail) height (r+1) (c+1))
					| N (_, _) -> (makeTlist (t@[t1; t2]) (([0;-1;1;-2]@head)::tail) height (r+1) (c+1)))
				| N (_, _) ->
				    (match t2 with
					 	A _ -> raise Error
					   | L -> (makeTlist (t@[t1; t2]) (([1;-1; 0;-2]@head)::tail) height (r+1) (c+1))
					   | N (_, _) -> (makeTlist (t@[t1; t2]) (([0;-1; 0;-2]@head)::tail) height (r+1) (c+1)))
			)))

		else 
		(match tlist with
		 [] -> raise Error
		 | head::tail ->
		(match h with
		 A i -> (makeTlist (t@[A (i*2); A (i*2)]) (([i*2; i*2]@head)::tail) height r (c+1))
		 | L -> (makeTlist (t@[A 2; A 2]) (([2; 2]@head)::tail) height r(c+1))
	     | N (t1, t2) ->
		     (match t1 with
			  	A _ -> raise Error
			     | L ->
				    (match t2 with
					 A _ -> raise Error
				    | L -> (makeTlist (t@[t1; t2]) (([1;-1; 1;-2]@head)::tail) height r (c+1))
				    | N (_, _) -> (makeTlist (t@[t1; t2]) (([0;-1; 1;-2]@head)::tail) height r (c+1)))
				 | N (_, _) ->
				    (match t2 with
					 A _ -> raise Error
				   | L -> (makeTlist (t@[t1; t2]) (([1;-1; 0;-2]@head)::tail) height r (c+1))
				   | N (_, _) -> (makeTlist (t@[t1; t2]) (([0;-1; 0;-2]@head)::tail) height r (c+1)))
			 )))
		)
in

let rec calTheight tour =
	match tour with
	LEAF a -> 1
	| NODE (t1, t2) -> (max (calTheight t1) (calTheight t2))+1
in

let makeReverse doublelist = List.map List.rev (List.rev doublelist)
in

let rec maketour2 tour =
	match tour with
	LEAF _ -> L 
	| NODE (t1, t2) -> N (maketour2 t1, maketour2 t2)
in

let rec tlN l n =
	if (n<=0) then l
	else (tlN (List.tl l) (n-1))
in
let rec mkstr fir str n =
	if (n<=0.) then str
	else (mkstr fir (fir^str) (n-.1.))
in
let rec mkstrL l h r =
	match l with
	[] -> ""
	| head::tail -> 
		(if (head=(-2)) then ((mkstr " " "" ((2.**(float (h-r+1)))-.1.))^(mkstrL tail h r))
		 else if (head=(-1)) then ((mkstr "-" "" ((2.**(float (h-r+1)))-.1.))^(mkstrL tail h r))
		 else if (head=0 || head=1) then ("|"^(mkstrL tail h r))
		 else ((mkstr " " "" (2.**(float (h-r))*.(((float head)/.4.)+.((float head)-.1.)))^"|"^(mkstr " " "" (2.**(float (h-r))*.((float head)/.4.)+.((float head)-.1.)))^(mkstr " " "" (2.**(float (h-r+1))-.1.))^(mkstrL (tlN l head) h r))))
in

let rec printTlist height r tlist =
	if (r>height) then (print_string "")
	else if (r=1) then ((print_endline ((mkstr " " "" (2.**(float (height-r))))^"|")); (printTlist height (r+1) tlist))
	else match tlist with
			[] -> raise Error
			| head::tail -> (print_endline ((mkstr " " "" (2.**(float (height-r))))^(mkstrL head height r))); (printTlist height (r+1) tail)
in

(printTlist (calTheight tour) 1 (makeReverse (makeTlist [(maketour2 tour)] [] (calTheight tour) 1 1)))
