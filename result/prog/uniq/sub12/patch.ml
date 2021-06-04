let rec isUniq (lst : 'a list) m : bool =
  match lst with
  | [] -> true
  | hd :: tl -> if hd = m then false else isUniq tl m


let rec __s1 __s2 (__s3 : 'b list) : bool =
  match __s3 with
  | [] -> false
  | __s11 :: __s12 -> __s11 = __s2 || __s1 __s2 __s12


let rec __s6 (__s7 : 'c list) (__s8 : 'c list) : 'c list =
  match __s7 with
  | [] -> __s8
  | __s9 :: __s10 ->
      if __s1 __s9 __s8 then __s6 __s10 __s8 else __s6 __s10 (__s8 @ [ __s9 ])


let rec uniq (lst : 'd list) : 'e list =
  let rec unique (uniList : 'd list) (givenList : 'd list) : 'd list =
    match givenList with
    | [] -> uniList
    | hd :: tl ->
        if isUniq uniList hd then unique (hd :: uniList) tl
        else unique uniList tl
  in
  __s6 lst []


let (_ : int list) = uniq [ 3; 2; 1; 3; 4 ]
