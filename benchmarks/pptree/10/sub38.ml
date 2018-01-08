type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna
;;

type analyzedtree 	= ALeaf of level * width  (* 여기서 width는 리프의 두께를 의미한다. *)	
					| ANode of level * tourna (* 여기서 레벨은 노드의 높이를 의미한다. *)
					| NewLine
		and level = int
		and width = int
;;

let rec print_n_char n c =
	if n <= 0 then ()
	else (print_char c;print_n_char (n-1) c)

let rec mypower n r =	(* 0승 이상의 크기만 받는다. *)
	if r <= 0 then 1
	else n * (mypower n (r-1))

module type ELEMENT =
	sig
		type t
	end

module TournaType : ELEMENT =
	struct
		type t = analyzedtree
	end

module EQueue =
	functor ( Element : ELEMENT ) ->
	struct 
		type t = Element.t
		type q = t list
		exception EMPTYQUEUE
		let empty = [] 
		let is_empty qu = 
			match qu with
				[] -> true
				| _ -> false 
		let pop qu =
			match qu with
				[] -> raise EMPTYQUEUE
				| x::y -> y 
		let push qu tt =
			qu @ [tt] 
		let clear qu =
			[] 
		let create () =
			[] 
		let top qu =
			match qu with
				[] -> raise EMPTYQUEUE
				| x::y -> x 
	end
module TonaQueue = EQueue(TournaType)
;;
let rec treeDepth tona =
	match tona with
		LEAF a -> 1
		| NODE (a,b) -> (max (treeDepth a) (treeDepth b))+1
;;
let printoneleaf antree = 
	match antree with
		ALeaf (l,w) -> 
			if l <= 0 
				then ()
			else
				(print_n_char ((mypower 2 (w-1))-1) ' ';print_char '|';print_n_char (mypower 2 (w-1)) ' ')
		| ANode (l,t) -> 
			if l <= 0 then ()
			else
				(match t with
					LEAF _ -> (print_n_char ((mypower 2 l)-1) ' ';print_char '|';print_n_char (mypower 2 l) ' ')
					| _ -> (print_n_char ((mypower 2 (l-1))-1) ' ';print_char '|';print_n_char ((mypower 2 l)-1) '-';print_char '|';print_n_char (mypower 2 (l-1)) ' '))
		| _ -> ()
;;

let rec goprint tonaque =
	if TonaQueue.is_empty tonaque
		then ()
	else
		let top = TonaQueue.top tonaque in
		let next = TonaQueue.pop tonaque in
		(printoneleaf top;
		 (match top with
		  	ALeaf (l,w) ->  if l <= 0 
								then ()
							else
								goprint (TonaQueue.push next (ALeaf (l-1,w)))
			| ANode (l,t) -> 
				if l <= 0 
					then ()
				else
					(match t with
					 	LEAF x -> goprint (TonaQueue.push next (ALeaf (l-1,l+1)))
						| NODE (x1,x2) -> 
							let nextque = TonaQueue.push next (ANode (l-1,x1)) in
							let nextque2 = TonaQueue.push nextque (ANode (l-1,x2)) in
							goprint nextque2)
			| NewLine -> (print_string "\n";goprint (TonaQueue.push next NewLine))
		 )
		)
;;					

let pptree tona =
	let equ = TonaQueue.create () in
	let dd = TonaQueue.push equ (ANode ((treeDepth tona)-1, tona)) in
	(printoneleaf (ALeaf ((treeDepth tona),(treeDepth tona)));
	 print_string "\n";
	 goprint (TonaQueue.push dd NewLine))
;;
