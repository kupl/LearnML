  (* problem 7*)

  let rec unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> match lst with | [] -> ([],[])
  |(x,y)::tl -> let lst1,lst2 = unzip tl in x::lst1,y::lst2;;
