let rec merge : int list*int list -> int list = fun (l1,l2) ->
 if l1==[] then l2
 else if l2==[] then l1
 else if (List.hd l1)>(List.hd l2) then (List.hd l1)::merge(List.tl l1,l2)
 else (List.hd l2)::merge(List.tl l2,l1)