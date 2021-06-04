let rec isUniq (lst : 'a list) m : bool =
  match lst with
  | [] -> true
  | hd :: tl -> if hd = m then false else isUniq tl m


let rec uniq (lst : 'b list) : 'c list =
  let rec unique (uniList : 'b list) (givenList : 'b list) : 'b list =
    match givenList with
    | [] -> uniList
    | hd :: tl ->
        if isUniq uniList hd then unique (hd :: uniList) tl
        else unique uniList tl
  in
  unique [] lst


let (_ : int list) = uniq [ 3; 2; 1; 3; 4 ]
