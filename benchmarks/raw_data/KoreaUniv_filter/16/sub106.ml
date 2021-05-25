let rec filter pred lst =
fun x acc -> if pred x then x::acc else acc;;
