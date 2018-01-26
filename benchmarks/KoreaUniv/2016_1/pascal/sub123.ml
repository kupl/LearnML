(* Problem 1 *) 
exception Dont_insert_minus
exception Dont_insert_y_which_bigger_than_x
let rec pascal (x, y) =
if x<0||y<0 then raise Dont_insert_minus
else if x<y then raise Dont_insert_y_which_bigger_than_x
else if x==y then 1 
else if y==0 then 1
else pascal (x-1,y-1) + pascal (x-1,y)
;;