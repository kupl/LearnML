(* Exercise 1 *)
type team =
	Korea
	| France
	| Usa
	| Brazil
	| Japan
	| Nigeria
	| Cameroon
	| Poland
	| Portugal
	| Italy
	| Germany
	| Sweden
	| England
	| Croatia
	| Argentina

type tourna =
	LEAF of team
	| NODE of tourna * tourna

let rec getDepth tourna =
	match tourna with
		LEAF _ ->
			1
		| NODE (a, b) ->
			((max (getDepth a) (getDepth b)) + 1)

let pow a b =
	(int_of_float ((float_of_int a) ** (float_of_int b)))

let getSubstrlen nowdepth maxdepth =
	((pow 2 (maxdepth - nowdepth + 1)) - 1)

let getPadlen nowdepth maxdepth =
	((pow 2 (maxdepth - nowdepth)) - 1)

let getBranchlen nowdepth maxdepth =
	((pow 2 (maxdepth - nowdepth + 1)) - 1)

let rec fillcharwith str len =
	if (len = 0) then
		""
	else
		(str ^ (fillcharwith str (len - 1)))

let rec getsubtreestr tourna depth nowdepth maxdepth =
	if (depth = 1) then
		let padlen = (getPadlen depth maxdepth) in
		((fillcharwith " " padlen) ^ "|")
	else
		match tourna with
			LEAF _ ->
				let padlen = (getPadlen nowdepth maxdepth) in
				((fillcharwith " " padlen) ^ "|")
			| NODE (l, r) ->
				if ((depth - 1) = nowdepth) then
					let padlen = (getPadlen depth maxdepth) in
					let pad = (fillcharwith " " padlen) in
					let branchlen = (getBranchlen depth maxdepth) in
					let branch = (fillcharwith "-" branchlen) in
					(pad ^ "|" ^ branch ^ "|")
				else
					let substrlen = (getSubstrlen (nowdepth + 1) maxdepth) in
					let lsubtreestr = (getsubtreestr l depth (nowdepth + 1) maxdepth) in
					let pad = (fillcharwith " " (substrlen - (String.length lsubtreestr) + 1)) in
					let rsubtreestr = (getsubtreestr r depth (nowdepth + 1) maxdepth) in
					(lsubtreestr ^ pad ^ rsubtreestr)

let subpptree tourna depth maxdepth =
	(getsubtreestr tourna depth 1 maxdepth)

let rec iterpptree tourna n maxdepth =
	if (n = maxdepth) then
		((subpptree tourna n maxdepth) ^ "\n")
	else
		((subpptree tourna n maxdepth) ^ "\n" ^ (iterpptree tourna (n + 1) maxdepth))

let pptree tourna =
	(print_string (iterpptree tourna 1 (getDepth tourna)))
