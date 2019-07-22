let rec filter pred lst = (* TODO *)
match lst with
[] -> []
|hd::tl -> if ( pred hd ) then hd::( filter pred tl )
else ( filter pred tl )
