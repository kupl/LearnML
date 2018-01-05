type metro =
STATION of string
|AREA of string*metro
|CONNECT of metro*metro;;

let rec checkmetro mtr =
match mtr with
|STATION _ -> true
|AREA (n,STATION m) -> n=m
|AREA (_,rest) -> checkmetro rest
|CONNECT (a,b) -> (checkmetro a)&&(checkmetro b)

