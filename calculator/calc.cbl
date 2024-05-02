      ******************************************************************
      *    Calculatrice basique ne repondant pas au brief
      *================================================================= 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calc.
       AUTHOR. Yves.
      ******************************************************************
       DATA DIVISION.
      *****************************************************************
       WORKING-STORAGE SECTION.
      *    variable necessaires aux calcules
       01  WS-NUM1           PIC S9(3).
       01  WS-NUM2           PIC S9(3).
       01  WS-RESULT         PIC S9(5)v99.
       01  WS-OPE            PIC X(3).

      *    Variables d'affichace
       01  WS-NUM1-ED        PIC -Z(3)9.99.
       01  WS-NUM2-ED        PIC Z(3)9.99.
       01  WS-RESULT-ED      PIC Z(8)9.99.
       01  WS-RESULT-ED-NEG  PIC -Z(5)9.99.

      *    variables de messages
       01  WS-LINE           PIC X(29).
       01  WS-TITLE          PIC X(29) VALUE 
       "Bienvenue sur la CALCULATRICE".


      *    variable de sortie
       01  WS-SORTIE   PIC X(1).
       01  WS-RESULT-C PIC X(1).

       PROCEDURE DIVISION.
           PERFORM 0000-EN-TETE
           PERFORM 0001-SAISIE-NUM1
           PERFORM UNTIL FUNCTION UPPER-CASE(WS-SORTIE) = 'n'
              PERFORM 0008-CONTINUE
              THRU    0008-CONTINUE-SUITE
           END-PERFORM.
           STOP RUN.

       0000-EN-TETE.
           MOVE ALL "*" TO WS-LINE.
           DISPLAY WS-LINE.
           DISPLAY WS-TITLE.
           DISPLAY WS-LINE.
       
       0001-SAISIE-NUM1.
           DISPLAY "Veuilleur saisir le premier nombre :" 
           SPACE NO ADVANCING
              ACCEPT WS-NUM1.
              GO TO 0002-SAISIE-OPE.

       
       0002-SAISIE-OPE.
           DISPLAY "Veuillez saisir l'opérateur" SPACE 
           SPACE NO ADVANCING
              ACCEPT WS-OPE.
              GO TO 0003-SAISIE-NUM2.
       
       0003-SAISIE-NUM2.
           DISPLAY "Veuillez saisir le second nombre"
              SPACE NO ADVANCING 
              ACCEPT WS-NUM2.
              GO TO 0004-EVALUATE-CALCUL.

       0004-EVALUATE-CALCUL.
           EVALUATE WS-OPE
              WHEN "+" ADD WS-NUM1 WS-NUM2 TO WS-RESULT
              WHEN "-" MOVE WS-NUM1 TO WS-RESULT 
                 SUBTRACT WS-NUM2 FROM WS-RESULT
              WHEN "*" MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
              WHEN "/" 
                 IF WS-NUM2 = 0 THEN
                       DISPLAY "**Division par zéro impossible***"
                       GO TO 0003-SAISIE-NUM2
                  END-IF
                 DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
              WHEN OTHER DISPLAY "***Opérateur inconnu***"
              GO TO 0002-SAISIE-OPE
           END-EVALUATE.
              GO TO 0005-SHOW-RESULT.
     
       0005-SHOW-RESULT.
           MOVE WS-NUM1 TO WS-NUM1-ED.
           MOVE WS-NUM2 TO WS-NUM2-ED.
           IF WS-RESULT IS POSITIVE THEN
              MOVE WS-RESULT TO WS-RESULT-ED
              DISPLAY WS-NUM1-ED 
              SPACE WS-OPE
              SPACE WS-NUM2-ED
              SPACE "=" WS-RESULT-ED
            END-IF.

            IF WS-RESULT IS NEGATIVE THEN
              MOVE WS-RESULT TO WS-RESULT-ED-NEG
           DISPLAY WS-NUM1-ED 
              SPACE WS-OPE
              SPACE WS-NUM2-ED
              SPACE "=" WS-RESULT-ED-NEG
            END-IF.

           
       
       0008-CONTINUE.
           DISPLAY "Souhaitez vous continuer ? (y/n)".
           ACCEPT WS-SORTIE.
           IF FUNCTION LOWER-CASE(WS-SORTIE)= "n" THEN
              STOP RUN
           END-IF.
           IF FUNCTION LOWER-CASE(WS-SORTIE) = "y" THEN
              GO TO 0008-CONTINUE-SUITE
           END-IF.

       0008-CONTINUE-SUITE.
           DISPLAY "Souhaitez conserver le résultat ? (y/n)"
           ACCEPT WS-RESULT-C
            IF FUNCTION LOWER-CASE(WS-RESULT-C)= "n" THEN
              INITIALIZE WS-RESULT
              INITIALIZE WS-NUM1
              INITIALIZE WS-OPE
              INITIALIZE WS-NUM2
              GO TO 0001-SAISIE-NUM1
            END-IF.

           IF FUNCTION LOWER-CASE(WS-RESULT-C) = "y" THEN
              SET WS-NUM1 TO WS-RESULT
              INITIALIZE WS-RESULT
              INITIALIZE WS-OPE
              INITIALIZE WS-NUM2
              MOVE WS-NUM1 TO WS-NUM1-ED
              DISPLAY "Nous repartons de" SPACE WS-NUM1-ED 
              GO TO 0002-SAISIE-OPE
           END-IF.
