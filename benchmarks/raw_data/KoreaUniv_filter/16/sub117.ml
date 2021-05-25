let rec filter pred lst = 
match lst with
[] -> []
|h::t -> if pred h = true then h::filter pred t else filter pred t;;
