{
	fun hd -> (hd+1), [1;2;3] -> [2;3;4];
	fun x -> (x-1), [1;2;3] -> [0;1;2];
}
let rec f func l =
	match l with
	|[] -> []
	|hd::tl -> (func hd) :: (f func tl) ;; 