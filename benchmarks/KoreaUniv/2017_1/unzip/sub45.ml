(* problem 7*) let unzip : ('a * 'b) list -> 'a list * 'b list = fin lst -> let lef (x,_) = x in let rig (_,x) = x in let newlst a b = (a,b) in let rec llst k = match k with | [] -> [] | hd :: tl -> (lef hd) :: (llst tl) in let rec rlst p = match p with | [] -> [] | hd :: tl -> (rig hd) :: (rlst tl) in newlst (llst lst) (rlst lst);;
