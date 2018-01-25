(*6*)
  let drop : 'a list -> int -> 'a list
  =fun l n ->
    let rec length l=
      match l with 
      | [] -> 0
      | hd::tl -> 1+length tl
      in
    let rec remove l n=
    match l with
    |[] -> []
    |hd::tl-> if n=1 then tl else remove tl (n-1)
    in
  if length l<=0 then [] else remove l n;;