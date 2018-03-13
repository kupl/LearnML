let rec filter pred lst = 
begin
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl
end;; 
