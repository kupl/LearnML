(* HW1 Exercise 1 merge lists *)

let rec merge : int list * int list -> int list = fun (list1, list2) ->
  match list1 with
  | [] -> list2
  | head1 :: tail1 ->
    (match list2 with
    | [] -> list1
    | head2 :: tail2 ->
      if (head1 > head2) then head1 :: merge (tail1, list2)
      else head2 :: merge (list1, tail2)
    )
