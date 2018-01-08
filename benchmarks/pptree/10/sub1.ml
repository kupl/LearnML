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

let toString w = match w with
  Korea -> "Korea"
| France -> "France"
| Usa -> "Usa"
| Brazil -> "Brazil"
| Japan -> "Japan"
| Nigeria -> "Nigeria"
| Cameroon -> "Cameroon"
| Poland -> "Poland"
| Portugal -> "Portugal"
| Italy -> "Italy"
| Germany -> "Germany"
| Sweden -> "Sweden"
| England -> "England"
| Croatia -> "Croatia"
| Argentina -> "Argentina"
;;

let pparray : char array -> unit =
(
	function x ->
	(
		print_char(':');
		Array.iter print_char x
	)
);;

let ppmatrix : char array array -> unit =
(
	function x ->
	(
		for i = 0 to (Array.length x) - 1 do
			pparray x.(Array.length x - 1 - i);
			print_char('\n')
		done
	)
);;

let rec extendmatrix tree height =
(
	if height < Array.length tree
	then tree
	else
		if height == Array.length tree
		then tree
		else extendmatrix (Array.append tree [|tree.(Array.length tree - 1)|]) height
);;

let rec arrayleft x =
	if x.(0) == '|'
	then 0
	else 1 + arrayleft (Array.sub x 1 (Array.length x - 1) )
;;

let rec arrayright x =
	if x.(Array.length x - 1) == '|'
	then 0
	else 1 + arrayright (Array.sub x 0 (Array.length x - 1) )
;;

let treeleft tree =
		arrayleft tree.(Array.length tree - 1)
;;

let treeright tree =
		arrayright tree.(Array.length tree - 1)
;;

let bridgelength left right =
	if 0 == ((left + right) mod 2)
	then 1
	else 2
;;

let rec charlist c count =
if count < 1
then []
else
	if count == 1
	then [c]
	else [c] @ (charlist c (count-1))
;;

let bridge ll lr rl rr =
	Array.of_list ((charlist ' ' ll) @ ['|'] @ (charlist '-' (lr + (bridgelength lr rl) + rl)) @ ['|'] @ (charlist ' ' rr))
;;

let cap ll lr rl rr =
	Array.of_list ((charlist ' ' (ll + 1 + (lr+rl+1)/2)) @ ['|'] @ (charlist ' ' ((lr+rl+1)/2 + 1 + rr)))
;;

let rec concatarray a b intvl = 
		Array.of_list ( (Array.to_list a) @ (charlist ' ' intvl) @ (Array.to_list b) )
;;

let rec concatmatrix mat1 mat2 intvl =
	if 1 == Array.length mat1
	then [ concatarray mat1.(0) mat2.(0) intvl ]
	else [ concatarray mat1.(0) mat2.(0) intvl ] @ (concatmatrix (Array.sub mat1 1 (Array.length mat1 - 1) ) (Array.sub mat2 1 (Array.length mat2 - 1) ) intvl)
;;

let treeheight t = Array.length t;;

let cuttail t =
	List.rev (List.tl (List.rev t))
;;

let connect tree1 tree2 =
	cuttail ( concatmatrix ( extendmatrix tree1 ( max ( treeheight tree1 ) ( treeheight tree2 ) ) ) ( extendmatrix tree2 ( max ( treeheight tree1 ) ( treeheight tree2 ) ) ) ( bridgelength (treeright tree1) (treeleft tree2) ) )
;;

let connectedtree : char array array * char array array -> char array array =
(
	function (left, right) ->
	(
		Array.of_list ( (connect left right) @ ( [ bridge (treeleft left) (treeright left) (treeleft right) (treeright right) ] ) @ ( [ cap (treeleft left) (treeright left) (treeleft right) (treeright right) ] ) )
	)
);;

let rec _pptree : tourna -> char array array =
(
	function x ->
	(
		match x with
		  LEAF leafname -> [|[|'|'|]|]
		| NODE (left, right) -> connectedtree ((_pptree left), (_pptree right))
	)
);;

let pptree x = ppmatrix (_pptree x);;

(*
test variable:
pptree (NODE (NODE(LEAF Korea, LEAF France), NODE(NODE(LEAF England, LEAF Italy), LEAF Usa)));;
pptree (NODE(NODE (NODE(LEAF Korea, LEAF France), NODE(NODE(LEAF England, LEAF Italy), LEAF Usa)), NODE (NODE(LEAF Korea, LEAF France), NODE(LEAF Usa, NODE(LEAF England, LEAF Italy)))));;
*)