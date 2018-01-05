type metro = STATION of name
            | AREA of name*metro
            | CONNECT of metro*metro
and name = string

let rec checkmetrolist ametro checklist =
  match ametro with
 | STATION (a) -> List.mem a checklist
 | AREA (a,b) -> checkmetrolist b (a::checklist)
 | CONNECT(a,b)-> (checkmetrolist a checklist) && (checkmetrolist b checklist)
;;

let checkMetro ametro = checkmetrolist ametro []
;;


