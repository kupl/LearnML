type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let pptree tour = 
	let rec getLevel subTree =
		match subTree with
		LEAF a -> 1
		|NODE (tour1, tour2) -> 
			(let p = getLevel tour1 in
			 let q = getLevel tour2 in
			 if (p < q) then (q + 1)
			 else (p + 1)
		        )
	in
	let seQnum num =
	let rec seQof2 n =
		if (n == 0) then 1
		else if (n == 1) then 2
		else (2 * (seQof2 ( n - 1)))
	in
	((seQof2 num) - 1)
	in

	let rec print_space number =
                (if (number == 0) then print_string ""
                else ((print_string " ");print_space (number - 1)))
        in
	
	let print_leaf num =
        ((print_space num);(print_string "|");(print_space num))
        in

	let isLeaf tree num =
		match tree with 
		LEAF b -> 0
		|_ -> num - 1
	in
 	
	let rec sameLevelPrint tree num totalLevel =
	
	let print_node number =
	let rec print_hyper number =
		(if (number == 0) then print_string ""
		 else if (number >= 1) then ((print_string "-");print_hyper (number - 1))
		 else (print_string ""))
	in
	((print_space (number/2));(print_string "|");(print_hyper number);(print_string "|");(print_space (number/2)) )
	in
	
	if (num == 0) then 
	(match tree with
	LEAF a -> print_leaf (seQnum (totalLevel -1))
	|NODE (a,b) -> print_node (seQnum (totalLevel -1))
	)
	else
	(match tree with
	LEAF a -> print_leaf (seQnum (totalLevel-num + 1))
	|NODE (a,b) -> 	(sameLevelPrint a (isLeaf a num) (totalLevel -1));(print_string " ");(sameLevelPrint b (isLeaf b num) (totalLevel -1))
	)
	in

	let rec subpptree tree num totalLevel =
	
	if (num == -1) then ((print_leaf (seQnum (totalLevel - 1)));(print_string "\n");(subpptree tree (num + 1) totalLevel))
	else if (num + 1 == totalLevel) then (print_string "")
	else ((sameLevelPrint tree num totalLevel );(print_string "\n");(subpptree tree (num + 1) totalLevel ))
	in

(subpptree tour (-1) (getLevel tour))
