let rec isUniq (lst : 'a list) m : bool =
  match lst with
  | [] -> true
  | hd :: tl -> if hd = m then false else isUniq tl m


let rec __s3 (__s4 : 'b list) __s5 : bool =
  match __s4 with
  | [] -> false
  | __s11 :: __s12 -> if __s11 = __s5 then true else __s3 __s12 __s5


let rec __s6 (__s7 : 'b list) (__s8 : 'b list) : 'b list =
  match __s7 with
  | [] -> __s8
  | __s9 :: __s10 ->
      if __s3 __s8 __s9 then __s6 __s10 __s8 else __s6 __s10 (__s8 @ [ __s9 ])


let rec uniq (lst : 'c list) : 'd list =
  let rec unique (uniList : 'c list) (givenList : 'c list) : 'c list =
    match givenList with
    | [] -> uniList
    | hd :: tl ->
        if isUniq uniList hd then unique (hd :: uniList) tl
        else unique uniList tl
  in
  __s6 lst []


let (_ : int list) = uniq [ 3; 2; 1; 3; 4 ]
