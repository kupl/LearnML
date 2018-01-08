type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina | A | B

type tourna = LEAF of team
| NODE of tourna * tourna

let pptstring tourna= 
          match tourna with
          |LEAF _ -> "|"

          |(NODE (LEAF _, LEAF _)) -> "'\n | \n"^"|-|\n"

                (*3*)
          |(NODE (LEAF _, (NODE (LEAF _, LEAF _)))) -> "'\n  |\n"^"|---|\n"^"|  |-| \n"
          |(NODE ((NODE (LEAF _, LEAF _), LEAF _))) -> "'\n   |\n "^"|---|\n"^"|-|  |\n"
          |(NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))) -> "'\n   |\n "^"|---|\n"^"|-| |-|\n"


                (*4*)
          |(NODE (LEAF _ , (NODE (LEAF _, (NODE (LEAF _, LEAF _)))))) ->
                          "'\n  |\n"^"|---|\n"^"| |---|\n"^"| |  |-|\n"          
          |(NODE (LEAF _ , (NODE ((NODE (LEAF _, LEAF _)), LEAF _)))) ->
                          "'\n   |\n"^"|-----|\n"^"|   |---|\n"^"|  |-|  |"      
          |(NODE (LEAF _ , (NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n   |\n"^"|-----|\n"^"|   |---|\n"^"|   |  |-|\n"


          |(NODE ((NODE (LEAF _, (NODE (LEAF _, LEAF _)))), LEAF _)) -> 
                          "'\n     |\n"^"  |-----|\n"^"|---|   |\n"^"|  |-|  |\n"
          |(NODE ((NODE ((NODE (LEAF _, LEAF _), LEAF _))), LEAF _)) -> 
                          "'\n      |\n   "^"|-----|\n "^"|---|   |\n"^"|-|  |   |\n"
          |(NODE ((NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))), LEAF _)) -> 
                          "'\n      |\n   "^"|-----|\n "^"|---|   |\n"^"|-| |-|  |\n"


          |(NODE ((NODE (LEAF _, LEAF _)) , (NODE (LEAF _, (NODE (LEAF _, LEAF _)))))) ->
                          "'\n    |\n "^"|-----|\n"^"|-|  |---|\n"^"| |  |  |-|\n"          
          |(NODE ((NODE (LEAF _, LEAF _)) , (NODE ((NODE (LEAF _, LEAF _)), LEAF _)))) ->
                          "'\n    |\n "^"|-----|\n"^"|-|  |---|\n"^"| | |-|  |\n"      
          |(NODE ((NODE (LEAF _, LEAF _)) , (NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n    |\n "^"|-----|\n"^"|-|  |---|\n"^"| | |-| |-|\n"

          
          |(NODE ((NODE (LEAF _, (NODE (LEAF _, LEAF _)))), (NODE (LEAF _, (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n      |\n  "^"|-------|\n"^"|---|   |---|\n"^"|  |-|  |  |-|\n"
          |(NODE ((NODE (LEAF _, (NODE (LEAF _, LEAF _)))), (NODE ((NODE (LEAF _, LEAF _), LEAF _))))) -> 
                          "'\n      |\n  "^"|-------|\n"^"|---|   |---|\n"^"|  |-| |-|  |\n"
          |(NODE ((NODE (LEAF _, (NODE (LEAF _, LEAF _)))) ,
           (NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n      |\n  "^"|-------|\n"^"|---|   |---|\n"^"|  |-| |-| |-|\n"

          |(NODE ((NODE ((NODE (LEAF _, LEAF _), LEAF _))), (NODE (LEAF _, (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-|  |   |  |-|\n"
          |(NODE ((NODE ((NODE (LEAF _, LEAF _), LEAF _))), (NODE ((NODE (LEAF _, LEAF _), LEAF _))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-|  |  |-|  |\n"
          |(NODE ((NODE ((NODE (LEAF _, LEAF _), LEAF _))),
           (NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-|  |  |-| |-|\n"   

           
          |(NODE ((NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))),
           (NODE (LEAF _, (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-| |-|  |  |-|\n"    
          |(NODE ((NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))),
           (NODE ((NODE (LEAF _, LEAF _), LEAF _))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-| |-| |-|  |\n"
          |(NODE ((NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))),
           (NODE ((NODE (LEAF _, LEAF _)), (NODE (LEAF _, LEAF _)))))) -> 
                          "'\n       |\n   "^"|-------|\n "^"|---|   |---|\n"^"|-| |-| |-| |-|\n"




let rec floor tourna = 
        match tourna with
        |LEAF _ -> 1
        |(NODE(LEAF _, LEAF _)) -> 2
        |(NODE(LEAF _, t)) |(NODE(t, LEAF _)) -> 1 + (floor t)
        |(NODE(t1, t2)) -> 
                        if (floor t1) >= (floor t2) then 1 + (floor t1)
                        else (floor t2) + 1
                        






let rec pptree tourna = print_string (pptstring tourna)


        

        
