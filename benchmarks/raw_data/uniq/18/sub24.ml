let rec check = fun n lst ->
match lst with
|[] ->false
|hd:: tl -> (hd==n) ||(check n tl);;
let rec uniq : 'a list -> 'a list = fun lst -> let rec x lst abc= match lst with
|[]-> abc
|hd::tl -> if check hd abc then x tl abc
else x tl(abc@hd::[])in x lst [];;
uniq [5;6;4;5];;
