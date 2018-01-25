let rec sigma f = function
    | [] -> 0
    | a :: b -> f a + sigma f b;;

