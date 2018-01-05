exception Error of string;;

let rec vocalthree s i = if (String.sub s 0 3) = "000" then ["영"] 
else match ((String.get s i), i) with
    ('0', 0) -> (List.rev (List.append (vocalthree s 1) []))
  | ('1', 0) -> (List.rev (List.append (vocalthree s 1) ["백"]))
  | ('2', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"이"]))
  | ('3', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"삼"]))
  | ('4', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"사"]))
  | ('5', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"오"]))
  | ('6', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"육"]))
  | ('7', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"칠"]))
  | ('8', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"팔"]))
  | ('9', 0) -> (List.rev (List.append (vocalthree s 1) ["백";"구"]))
  | ('0', 1) -> (List.append (vocalthree s 2) [])
  | ('1', 1) -> (List.append (vocalthree s 2) ["십"])
  | ('2', 1) -> (List.append (vocalthree s 2) ["십";"이"])
  | ('3', 1) -> (List.append (vocalthree s 2) ["십";"삼"])
  | ('4', 1) -> (List.append (vocalthree s 2) ["십";"사"])
  | ('5', 1) -> (List.append (vocalthree s 2) ["십";"오"])
  | ('6', 1) -> (List.append (vocalthree s 2) ["십";"육"])
  | ('7', 1) -> (List.append (vocalthree s 2) ["십";"칠"])
  | ('8', 1) -> (List.append (vocalthree s 2) ["십";"팔"])
  | ('9', 1) -> (List.append (vocalthree s 2) ["십";"구"])
  | ('0', 2) -> []
  | ('1', 2) -> ["일"]
  | ('2', 2) -> ["이"]
  | ('3', 2) -> ["삼"]
  | ('4', 2) -> ["사"]
  | ('5', 2) -> ["오"]
  | ('6', 2) -> ["육"]
  | ('7', 2) -> ["칠"]
  | ('8', 2) -> ["팔"]
  | ('9', 2) -> ["구"]
  | (_, _) -> raise(Error "incorrect input three");;

let rec vocalfourf s i = if (String.sub s 0 4) = "0000" then ["영"]
else match ((String.get s i), i) with
    ('0', 0) -> (List.rev (List.append (vocalfourf s 1) []))
  | ('1', 0) -> (List.rev (List.append (vocalfourf s 1) ["천"]))
  | ('2', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"이"]))
  | ('3', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"삼"]))
  | ('4', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"사"]))
  | ('5', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"오"]))
  | ('6', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"육"]))
  | ('7', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"칠"]))
  | ('8', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"팔"]))
  | ('9', 0) -> (List.rev (List.append (vocalfourf s 1) ["천";"구"]))
  | ('0', 1) -> (List.append (vocalfourf s 2) [])
  | ('1', 1) -> (List.append (vocalfourf s 2) ["백"])
  | ('2', 1) -> (List.append (vocalfourf s 2) ["백";"이"])
  | ('3', 1) -> (List.append (vocalfourf s 2) ["백";"삼"])
  | ('4', 1) -> (List.append (vocalfourf s 2) ["백";"사"])
  | ('5', 1) -> (List.append (vocalfourf s 2) ["백";"오"])
  | ('6', 1) -> (List.append (vocalfourf s 2) ["백";"육"])
  | ('7', 1) -> (List.append (vocalfourf s 2) ["백";"칠"])
  | ('8', 1) -> (List.append (vocalfourf s 2) ["백";"팔"])
  | ('9', 1) -> (List.append (vocalfourf s 2) ["백";"구"])
  | ('0', 2) -> (List.append (vocalfourf s 3) [])
  | ('1', 2) -> (List.append (vocalfourf s 3) ["십"])
  | ('2', 2) -> (List.append (vocalfourf s 3) ["십";"이"])
  | ('3', 2) -> (List.append (vocalfourf s 3) ["십";"삼"])
  | ('4', 2) -> (List.append (vocalfourf s 3) ["십";"사"])
  | ('5', 2) -> (List.append (vocalfourf s 3) ["십";"오"] )
  | ('6', 2) -> (List.append (vocalfourf s 3) ["십";"육"])
  | ('7', 2) -> (List.append (vocalfourf s 3) ["십";"칠"])
  | ('8', 2) -> (List.append (vocalfourf s 3) ["십";"팔"])
  | ('9', 2) -> (List.append (vocalfourf s 3) ["십"; "구"])
  | ('0', 3) -> []
  | ('1', 3) -> ["일"]
  | ('2', 3) -> ["이"]
  | ('3', 3) -> ["삼"]
  | ('4', 3) -> ["사"]
  | ('5', 3) -> ["오"]
  | ('6', 3) -> ["육"]
  | ('7', 3) -> ["칠"]
  | ('8', 3) -> ["팔"]
  | ('9', 3) -> ["구"]
  | (_, _) -> raise(Error "incorrect input four");;

let rec vocalfourb1 s i = if (String.sub s 3 4) = "0000" then ["영"] 
else match ((String.get s i), i) with
    ('0', 3) -> (List.rev (List.append (vocalfourb1 s 4) []))
  | ('1', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천"]))
  | ('2', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"이"]))
  | ('3', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"삼"]))
  | ('4', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"사"]))
  | ('5', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"오"]))
  | ('6', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"육"]))
  | ('7', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"칠"]))
  | ('8', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"팔"]))
  | ('9', 3) -> (List.rev (List.append (vocalfourb1 s 4) ["천";"구"]))
  | ('0', 4) -> (List.append (vocalfourb1 s 5) [])
  | ('1', 4) -> (List.append (vocalfourb1 s 5) ["백"])
  | ('2', 4) -> (List.append (vocalfourb1 s 5) ["백";"이"])
  | ('3', 4) -> (List.append (vocalfourb1 s 5) ["백";"삼"])
  | ('4', 4) -> (List.append (vocalfourb1 s 5) ["백";"사"])
  | ('5', 4) -> (List.append (vocalfourb1 s 5) ["백";"오"])
  | ('6', 4) -> (List.append (vocalfourb1 s 5) ["백";"육"])
  | ('7', 4) -> (List.append (vocalfourb1 s 5) ["백";"칠"])
  | ('8', 4) -> (List.append (vocalfourb1 s 5) ["백";"팔"])
  | ('9', 4) -> (List.append (vocalfourb1 s 5) ["백";"구"])
  | ('0', 5) -> (List.append (vocalfourb1 s 6) [])
  | ('1', 5) -> (List.append (vocalfourb1 s 6) ["십"])
  | ('2', 5) -> (List.append (vocalfourb1 s 6) ["십";"이"])
  | ('3', 5) -> (List.append (vocalfourb1 s 6) ["십";"삼"])
  | ('4', 5) -> (List.append (vocalfourb1 s 6) ["십";"사"])
  | ('5', 5) -> (List.append (vocalfourb1 s 6) ["십";"오"] )
  | ('6', 5) -> (List.append (vocalfourb1 s 6) ["십";"육"])
  | ('7', 5) -> (List.append (vocalfourb1 s 6) ["십";"칠"])
  | ('8', 5) -> (List.append (vocalfourb1 s 6) ["십";"팔"])
  | ('9', 5) -> (List.append (vocalfourb1 s 6) ["십"; "구"])
  | ('0', 6) -> []
  | ('1', 6) -> ["일"]
  | ('2', 6) -> ["이"]
  | ('3', 6) -> ["삼"]
  | ('4', 6) -> ["사"]
  | ('5', 6) -> ["오"]
  | ('6', 6) -> ["육"]
  | ('7', 6) -> ["칠"]
  | ('8', 6) -> ["팔"]
  | ('9', 6) -> ["구"]
  | (_, _) -> raise(Error "incorrect input four");;

let rec vocalfourb2 s i = if (String.sub s 4 4) = "0000" then ["영"] 
else match ((String.get s i), i) with
    ('0', 4) -> (List.rev (List.append (vocalfourb2 s 5) []))
  | ('1', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천"]))
  | ('2', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"이"]))
  | ('3', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"삼"]))
  | ('4', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"사"]))
  | ('5', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"오"]))
  | ('6', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"육"]))
  | ('7', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"칠"]))
  | ('8', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"팔"]))
  | ('9', 4) -> (List.rev (List.append (vocalfourb2 s 5) ["천";"구"]))
  | ('0', 5) -> (List.append (vocalfourb2 s 6) [])
  | ('1', 5) -> (List.append (vocalfourb2 s 6) ["백"])
  | ('2', 5) -> (List.append (vocalfourb2 s 6) ["백";"이"])
  | ('3', 5) -> (List.append (vocalfourb2 s 6) ["백";"삼"])
  | ('4', 5) -> (List.append (vocalfourb2 s 6) ["백";"사"])
  | ('5', 5) -> (List.append (vocalfourb2 s 6) ["백";"오"])
  | ('6', 5) -> (List.append (vocalfourb2 s 6) ["백";"육"])
  | ('7', 5) -> (List.append (vocalfourb2 s 6) ["백";"칠"])
  | ('8', 5) -> (List.append (vocalfourb2 s 6) ["백";"팔"])
  | ('9', 5) -> (List.append (vocalfourb2 s 6) ["백";"구"])
  | ('0', 6) -> (List.append (vocalfourb2 s 7) [])
  | ('1', 6) -> (List.append (vocalfourb2 s 7) ["십"])
  | ('2', 6) -> (List.append (vocalfourb2 s 7) ["십";"이"])
  | ('3', 6) -> (List.append (vocalfourb2 s 7) ["십";"삼"])
  | ('4', 6) -> (List.append (vocalfourb2 s 7) ["십";"사"])
  | ('5', 6) -> (List.append (vocalfourb2 s 7) ["십";"오"] )
  | ('6', 6) -> (List.append (vocalfourb2 s 7) ["십";"육"])
  | ('7', 6) -> (List.append (vocalfourb2 s 7) ["십";"칠"])
  | ('8', 6) -> (List.append (vocalfourb2 s 7) ["십";"팔"])
  | ('9', 6) -> (List.append (vocalfourb2 s 7) ["십"; "구"])
  | ('0', 7) -> []
  | ('1', 7) -> ["일"]
  | ('2', 7) -> ["이"]
  | ('3', 7) -> ["삼"]
  | ('4', 7) -> ["사"]
  | ('5', 7) -> ["오"]
  | ('6', 7) -> ["육"]
  | ('7', 7) -> ["칠"]
  | ('8', 7) -> ["팔"]
  | ('9', 7) -> ["구"]
  | (_, _) -> raise(Error "incorrect input four");;

let vocalize s = if (String.length s) = 7 then [(vocalthree s 0);(vocalfourb1 s 3)] 
else if (String.length s) = 8 then [(vocalfourf s 0);(vocalfourb2 s 4)] 
else raise(Error "incorrect number length");;
