let merge (list1, list2) = List.sort compare (list1 @ list2) |> List.rev
