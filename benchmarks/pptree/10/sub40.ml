exception Error of string;;

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;

type tourna = LEAF of team | NODE of tourna * tourna;;

let rec add_tourna l1 l2 = match l1 with
    [] -> []
  | (hd1::tl1) -> (match l2 with
			[] ->  raise(Error "Leaf node is not in bottom")
		      | (hd2::tl2) -> (if ((tl1 = []) && tl2 != []) then (hd1^" "^hd2) :: (add_tourna l1 tl2) 
				       else if ((tl1 != []) && tl2 = []) then (hd1^" "^hd2) :: (add_tourna tl1 l2) 
				       else (hd1^" "^hd2) :: (add_tourna tl1 tl2)
				      )
		  );;

let max a b = if a>b then a else b;;

let rec node_height t = match t with
    LEAF l -> 0
  | NODE(LEAF l1, LEAF l2) -> 0
  | NODE(t1, t2) -> (max (node_height t1) (node_height t2)) + 1;;

let rec intro_blank n = match n with 
    0 -> 0
  | 1 -> 1
  | _ -> (intro_blank (n-1)) + 2*(n-1);;

let rec bar_number n = match n with
    1 -> 3
  | _ -> (bar_number (n-1)) + 2*n;;

let rec pptree_base t = match t with
    LEAF l -> " | " :: []
  | NODE(LEAF l1, LEAF l2) -> "|-|" :: []
  | NODE(LEAF l, t2) -> if ((node_height t) = 1) then ((String.make (intro_blank (node_height t)) ' ')^"|"^(String.make ((bar_number (node_height t))) '-')^"|"^(String.make (intro_blank (node_height t)) ' ')) :: (add_tourna [" | "] (pptree_base t2))
    else ((String.make 1 ' ')^"|"^(String.make ((bar_number (node_height t))-2) '-')^"|"^(String.make (intro_blank (node_height t)) ' ')) :: (add_tourna [" | "] (pptree_base t2))
  | NODE(t1, LEAF l) -> if ((node_height t) = 1) then ((String.make (intro_blank (node_height t)) ' ')^"|"^(String.make ((bar_number (node_height t))) '-')^"|"^(String.make (intro_blank (node_height t)) ' ')) :: (add_tourna (pptree_base t1) [" | "] )
    else ((String.make (intro_blank (node_height t)) ' ')^"|"^(String.make ((bar_number (node_height t))-2) '-')^"|"^(String.make 1 ' ')) :: (add_tourna (pptree_base t1) [" | "] )
  | NODE(t1, t2) -> ((String.make (intro_blank (node_height t)) ' ')^"|"^(String.make (bar_number (node_height t)) '-')^"|"^(String.make (intro_blank (node_height t)) ' ')) :: (add_tourna (pptree_base t1) (pptree_base t2));;
		    
let rec print_tree l = match l with
    [] -> ()
  | hd :: tl -> let _ = (print_string hd) in
    let _ = print_newline () in
      (print_tree tl);;

let rec top_blank n = match n with 
    0 -> 1
  | 1 -> 3
  | _ -> (intro_blank (n-1)) + 2*n;;


let pptree t = let _ = (print_string (String.make (top_blank (node_height t)) ' ' )) in let _ = print_string "|" in let _ = print_newline () in (print_tree (pptree_base t));;




