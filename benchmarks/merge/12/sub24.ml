let rec merge : int list * int list -> int list = fun (a, b) ->
  if List.length(a) > 0 && List.length(b) > 0 then
    if List.hd(a) > List.hd(b) then
      List.append [List.hd(a)] (merge(List.tl(a), b))
    else
      List.append [List.hd(b)] (merge(a, List.tl(b)))
  else if List.length(a) > 0 then
    a
  else if List.length(b) > 0 then
    b
  else
    []
;;
