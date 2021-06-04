let rec __s1 (__s2 : 'a list) : 'a list =
  match __s2 with [] -> [] | __s20 :: __s21 -> __s1 __s21 @ [ __s20 ]


let uniq (lst : 'b list) : 'c list =
  let rec tu (a : 'b list) (l : 'b list) : 'b list =
    match l with
    | [] -> __s1 a
    | hd :: tl -> tu (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tu [] lst


let (_ : int list) = uniq [ 5; 4; 3; 5 ]
