let rec chck (lst : 'a list) item : bool =
  match lst with
  | [] -> true
  | h :: t -> if h = item then false else chck t item


let rec uniq (lst : 'c list) : 'b list =
  match lst with [] -> [] | h :: t -> if chck t h then h :: uniq t else uniq t


let (_ : int list) = uniq [ 5; 6; 5; 4; 1; 54; 2; 5; 2; 2; 5; 2; 3; 4; 2; 1 ]
