      *****************************************************************
      * Program name:    MYPROG                               
      * Original author: MYNAME                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 01/01/08 MYNAME  Created for COBOL class         
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  pizza.
       AUTHOR.Yves. 
   
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-GUEST        PIC 9(02).
       01  WS-NUM-PIZZA-PART   PIC 9(2)V9(1).  
       01  WS-MODULO           PIC 9(2)V9(1).
       01  WS-NUM-PIZZA-FULL   PIC 9(02).
       PROCEDURE DIVISION.
       
           DISPLAY "Combien d'invités avez vous ?"
           ACCEPT WS-NUM-GUEST.

           MULTIPLY WS-NUM-GUEST BY 1.1 GIVING WS-NUM-PIZZA-PART.

           COMPUTE WS-MODULO = FUNCTION MOD(WS-NUM-PIZZA-PART,1).
           IF WS-MODULO = 0
              CONTINUE
            ELSE
              SET WS-NUM-PIZZA-PART UP BY 1
           END-IF.
           MOVE WS-NUM-PIZZA-PART TO WS-NUM-PIZZA-FULL.

           DISPLAY "Commandez" 
           SPACE WS-NUM-PIZZA-FULL
           SPACE "pizzas".
