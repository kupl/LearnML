  (*problem 7*)
  let rec unzip: ('a * 'b) list -> 'a list * 'b list
  = fun lst -> match lst with
  |[] -> ([],[])
  |(x,y) :: tl ->
  let (fst, sec) = unzip tl in 
  (x:: fst, y::sec)
