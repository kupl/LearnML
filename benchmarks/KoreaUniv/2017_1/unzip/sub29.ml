(* problem 7*)
  let unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> let rec flist : ('a * 'b) list -> 'a list 
  = fun flst -> match flst with
  |[] -> []
  | hd::tl -> (match hd with
      | (x,_) -> x::(flist tl)) in
  let rec slist : ('a * 'b) list -> 'b list
  = fun slst -> match slst with
  |[] -> []
  | hd::tl -> (match hd with
      | (_,x) -> x::(slist tl)) in
  (flist lst, slist lst);;