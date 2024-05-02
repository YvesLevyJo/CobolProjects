      *****************************************************************
      *    Ce programme détermine si une chaine de caractere est un
      *    isgramme ou pas                        
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  isodgrm.
       AUTHOR. Yves. 
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT OCCURS 6.
           03 WS-INPUT-1 PIC X(6) VALUE "abbbea".  
       01  WS-COUNT-A PIC 9(2).
       01  WS-ACTUAL-LETTER PIC X.
       01  WS-I PIC 9(2) VALUE 0.
       01  WS-WORD-LENGTH PIC 9(2).

       PROCEDURE DIVISION.       
           COMPUTE WS-WORD-LENGTH = LENGTH OF WS-INPUT.   
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-WORD-LENGTH
                 DISPLAY "le compteur est a" SPACE WS-I

                 MOVE WS-INPUT(WS-I) TO WS-ACTUAL-LETTER
                 INSPECT WS-INPUT(WS-I)
                 TALLYING WS-COUNT-A FOR ALL WS-ACTUAL-LETTER
                 DISPLAY "la lettre"
                 SPACE WS-INPUT(WS-I)
                 SPACE "est presente" 
                 SPACE WS-COUNT-A 
                 SPACE "fois"
            END-PERFORM.
      *    DISPLAY WS-INPUT-1 SPACE "est pas un isogramme"
           STOP RUN.

      *0001-EVAL-ACTUAL-LETTER.
      *    IF WS-ACTUAL-LETTER IS NOT EQUAL TO "-"
      *       PERFORM 0001-SHOW-RESULT
      *    END-IF.
       
      *0001-SHOW-RESULT.
      *    IF WS-COUNT-A > 1 THEN
      *    DISPLAY WS-INPUT SPACE "n'est pas un isogramme"
      *    STOP RUN.

