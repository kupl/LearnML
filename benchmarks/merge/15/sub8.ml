(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 1: merge
 * Written by Dongho Kang 
 *)

let rec merge: int list * int list -> int list = fun (aList, bList) ->
    (* two descending order to merged descending order list *)
    
    (* base cases *)
    if List.length aList = 0 then bList 
    else if List.length bList = 0 then aList
    
    (* recursive step *)
    else begin
        let ah: int = List.hd aList in (* most significant elmt of aList *)
        let bh: int = List.hd bList in (* most significant elmt of bList *)

        if ah > bh then ah :: merge (List.tl aList, bList)
        else bh :: merge (aList, List.tl bList)
    end
;;

