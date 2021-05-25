let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> l2 @ remove l2 l2 l1
and remove src checker checkee = match (checker, checkee) with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> y :: remove src src ys
  | (x::xs, y::ys) -> if x = y then remove src src ys else remove src xs checkee;;
