let uniq : 'a list -> 'a list
= fun lst -> 
  let remove_duplicates (type a) (l: a list) =
  let module S = Set.Make(struct type t = a let compare = compare end) in
  let rec remove acc seen_set = function
      | [] -> List.rev acc
      | a :: rest when S.mem a seen_set -> remove acc seen_set rest
      | a :: rest -> remove (a::acc) (S.add a seen_set) rest in
  remove [] S.empty l;;
  remove_duplicates [5;6;5;4];;
