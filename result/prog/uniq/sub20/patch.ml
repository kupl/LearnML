let rec __s3 (__s4, (__s5 : 'a list)) : 'a list =
  List.filter (fun __s8 -> __s8 != __s4) __s5


let rec uniq (lst : 'b list) : 'c list =
  let rec loop (l : 'b list) n : 'b list =
    match l with
    | [] -> l
    | hd :: tl -> if n = hd then loop tl n else hd :: loop tl n
  in

  match lst with [] -> lst | hd :: tl -> hd :: uniq (__s3 (hd, tl))


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
