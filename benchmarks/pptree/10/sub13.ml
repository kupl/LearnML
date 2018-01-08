
exception Error of string
(*Problem 4.*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
|Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna
let maketeamstr : team -> string =
	(function thetea ->
		(match thetea with
			Korea -> "Korea"
			|France -> "France"
			|Usa -> "Usa"
			|Brazil -> "Brazil"
			|Japan -> "Japan"
			|Nigeria -> "Nigeria"
			|Cameroon -> "Cameroon"
			|Poland -> "Poland"
			|Portugal -> "Portugal"
			|Italy -> "Italy"
			|Germany -> "Germany"
			|Sweden -> "Sweden"
			|England -> "England"
			|Croatia -> "Croatia"
let rec spacefirstcount : string -> int =
	(function mystr -> 
		if ((String.length mystr) = 0) then 0
		else if ((String.get mystr 0) != ' ') then 0
		else (spacefirstcount (String.sub mystr 1 ((String.length mystr)-1))) + 1)
let rec spacerightcount : string -> int =
	(function mystr -> 
		let thelen = (String.length mystr) in
			if (thelen = 0) then 0
			else if ((String.get mystr (thelen-1)) != ' ') then 0
			else (spacerightcount (String.sub mystr 0 ((String.length mystr)-1))) + 1)

let leftsub : string -> unit=
	(function a ->
		(let theco = (spacefirstcount a) in
			(String.fill a 0 theco '-')))
let rightsub : string -> unit =
	(function a ->
		(let theco = (spacerightcount a) in
			(String.fill a ((String.length a)  - theco) theco '-')))
let makemiddleline : string -> string =
	(function a ->
		let b = (String.copy a) in
			let firstone = (String.index a '|') in
				let lastone = (String.rindex a '|') in
					let midone = ((firstone + lastone) / 2) in
						begin
						(String.fill b 0 (String.length b) ' ');
						b.[midone] <- '|';
						b;
					end)
let isodd alpha =
	if ((alpha mod 2) = 1) then true
		else false
let odorev (a1, a2) =
	isodd ((spacefirstcount a2) + (spacerightcount a1))
let garodraw (b1,b2) odder =
	let (a1,a2) = ((String.copy b1),(String.copy b2)) in
	begin
		(rightsub a1);
		(leftsub a2);
	if odder then
		(a1 ^ a2)
	else
		(a1 ^ "-" ^ a2)
	end
let rec hap_list (a1,a2) odder =
	if odder then 
		(match (a1,a2) with 
			([],[]) -> []
			| ((h1::t1), (h2::t2)) -> ((h1^h2) :: (hap_list (t1,t2) odder))
			| _ -> raise (Error "What!"))
	else match (a1,a2) with
			([],[]) -> []
			| ((h1::t1), (h2::t2)) -> ((h1^" "^h2) :: (hap_list (t1,t2) odder))
			| _ -> raise (Error "what!")

let rec hap_tree : string list * string list -> string list =
	(function (ls1, ls2) ->
		if ((List.length ls1) > (List.length ls2)) then (hap_tree (ls1, [(List.hd ls2)] @ ls2))
		else if ((List.length ls1) < (List.length ls2)) then (hap_tree (([(List.hd ls1)] @ ls1) , ls2))
		else 
		let (a1,a2) = ((List.hd ls1), (List.hd ls2)) in
			let (b1,b2) = ((List.tl ls1),(List.tl ls2)) in
				let theo = (odorev (a1,a2)) in
					let garo = (garodraw (a1,a2) theo) in
						[(makemiddleline garo)] @ [(garo)] @ (hap_list (b1,b2) theo))
let rec bojo_tree : tourna -> string list =
	(function thetou ->
		(match thetou with
			LEAF a -> ["|"]
			| NODE (a1,a2) -> let b1 = (bojo_tree a1) in
						let b2 = (bojo_tree a2) in
							(hap_tree (b1,b2))))
let printwithnew : string -> unit =
	(function a ->
		begin 
			print_string a;
			print_newline ();
		end)
let pptree : tourna -> unit =
	(function thetou ->
		(List.iter printwithnew (bojo_tree thetou)))
