let rec filter pred lst = fold (fun a b -> if (pred a) then a::b else b) lst [];;
