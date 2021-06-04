let rec __s1 (__s2 : 'a list) : 'a list =
  match __s2 with [] -> [] | __s20 :: __s21 -> __s1 __s21 @ [ __s20 ]


let uniq (lst : 'b list) : 'c list =
  let rec tail_uniq (a : 'b list) (lst : 'b list) : 'b list =
    match lst with
    | [] -> __s1 a
    | hd :: tl -> tail_uniq (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tail_uniq [] lst


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
