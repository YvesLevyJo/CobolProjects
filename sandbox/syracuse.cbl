       IDENTIFICATION DIVISION.
       PROGRAM-ID. syracuse.
       AUTHOR. Yves.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM      PIC 9(02).
       01  WS-COUNTER  PIC 9(02).     
       PROCEDURE DIVISION.
           
       1000-CHOOSE-NUMBER-START.
           DISPLAY "Veuillez choisir un nombre entier positif non nul"
           ACCEPT WS-NUM.

           IF WS-NUM <= 0
            DISPLAY "Erreur dans le choix"
            PERFORM 1000-CHOOSE-NUMBER-START
           END-IF.
       1000-CHOOSE-NUMBER-END.
           EXIT.

       2000-SYRACUS-PROCESS-START.
           PERFORM UNTIL WS-NUM =1
              IF FUNCTION MOD (WS-NUM,2) = 0
                 DIVIDE WS-NUM BY 2 GIVING WS-NUM
               ELSE
                 COMPUTE WS-NUM = (WS-NUM *3 ) + 1
              END-IF
              SET WS-COUNTER UP BY 1
                PERFORM 2000-SYRACUS-PROCESS-START
           END-PERFORM.
       2000-SYRACUS-PROCESS-END.
       DISPLAY "Conjecture de syracuse appliquées"
           SPACE WS-COUNTER SPACE "fois avant d'arriver à 1.".
       EXIT.
       