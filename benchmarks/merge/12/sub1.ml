(* E:\Documents and Settings\Administrator\πŸ≈¡ »≠∏È\merge *)

let rec merge a b =
  if (List.length a) = 0 && (List.length b) = 0 then b
  else if (List.length b) = 0 then a
  else if (List.length a) = 0 then b
  else if (List.hd(a) < List.hd(b)) = true then [List.hd a]
    @ (merge (List.tl a) b) else [List.hd b] @ (merge a (List.tl b));;

