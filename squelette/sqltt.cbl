      *****************************************************************
      *    Cette application genere un squlette d'application cobol     
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  sqltt.
       AUTHOR. Yves. 
      *****************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT OUTFILE ASSIGN TO 'prog.cbl'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-STATUS.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  OUTPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  F-STATUS               PIC X(02).
       01  PNT.
           03 BLANK-LINE          PIC X(80) VALUE ALL SPACES.
           03 COMMENT-LINE        PIC X(66) VALUE ALL '*'.
           03 MARGIN-5            PIC X(05) VALUE ALL SPACES.
           03 MARGIN-7            PIC X(07) VALUE ALL SPACES.
           03 MARGIN-11           PIC X(11) VALUE ALL SPACES.
           03 MARGIN-12           PIC X(12) VALUE ALL SPACES.

       01  APP-DATAS.
           03 WS-APP-NAME         PIC X(08).
           03 WS-APP-AUTHOR       PIC X(10).

       01  APP-OPTIONS.
           03 WS-READ-CHOICE         PIC X.
           03 WS-WRITE-CHOICE        PIC X.
           03 WS-SORT-CHOICE         PIC X.
           03 WS-MERGE-CHOICE        PIC X.
           03 WS-BDD-ACCES-CHOICE    PIC X.
           03 WS-BDD-COUNT-CHOICE    PIC X.
           03 WS-CURSOR-CHOICE       PIC X.
           03 WS-BDD-UPDT-CHOICE     PIC X.
           03 WS-ROUTINE-CHOICE      PIC X.
           03 WS-ROUTINE-CALL-CHOICE PIC X.
           03 WS-CB-TEMPLATE-CHOICE  PIC X.
           03 WS-CB-CHOICE           PIC X.
           03 WS-HELLOCBL-CHOICE     PIC X.
           03 WS-PARA-CHOICE         PIC X.

       01  FILE-OPTIONS.
           03 WS-
       SCREEN SECTION.
       01  
       01  MAIN-SCREEN FOREGROUND-COLOR IS 6.
           03 BLANK SCREEN.
           05 FILLER PIC X(76) 
           VALUE 'GENERATEUR D''OSSATURE D''APPLICATION' LINE 2 COL 5.
           
           05 FILLER PIC X(26) 
              VALUE "Entrez le nom du programme" LINE PLUS 2 COL 10.
           05 PIC X(08) USING WS-APP-NAME COL 40 REQUIRED.

           05 FILLER PIC X(25) 
              VALUE "Entrez le nom de l'auteur" LINE PLUS 1 COL 10.
           05 PIC X(10) USING WS-APP-AUTHOR COL 40 REQUIRED.

           05 FILLER PIC X(35) 
              VALUE "1. Lire un fichier sequentiel (Y/N)" 
              LINE PLUS 2 COL 10.
           05 PIC X USING WS-READ-CHOICE COL 55.

           05 FILLER PIC X(37) 
              VALUE "2. Ecrire un fichier sequentiel (Y/N)" 
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-WRITE-CHOICE COL 55.

           05 FILLER PIC X(25) 
              VALUE "3. Trier un fichier (Y/N)" LINE PLUS 1 COL 10.
           05 PIC X USING WS-SORT-CHOICE COL 55.
      
           05 FILLER PIC X(37) 
              VALUE "4. Fusionner plusieurs fichiers (Y/N)" 
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-MERGE-CHOICE COL 55.

      ******************************************************************
           05 FILLER PIC X(38) 
              VALUE "5. Acceder a une base de donnees (Y/N)" 
              LINE PLUS 2 COL 10.
           05 PIC X USING WS-BDD-ACCES-CHOICE COL 55.

           05 FILLER PIC X(39) 
              VALUE "6. Generer une requete SECLECT COUNT(*)"
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-BDD-COUNT-CHOICE COL 55.

           05 FILLER PIC X(27) 
              VALUE "7. Generer un curseur (Y/N)" 
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-CURSOR-CHOICE COL 55.

           05 FILLER PIC X(35) 
              VALUE "8. Generer une requete UPDATE (Y/N)" 
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-BDD-UPDT-CHOICE COL 55.

           05 FILLER PIC X(31) 
              VALUE "9. Creer une sous routine (Y/N)" 
              LINE PLUS 2 COL 10.
           05 PIC X USING WS-ROUTINE-CHOICE COL 45.

           05 FILLER PIC X(48) 
             VALUE "10. Integrer un appel type de sous routine (Y/N)"
             LINE PLUS 1 COL 10.
           05 PIC X USING WS-ROUTINE-CALL-CHOICE COL 59.

           05 FILLER PIC X(42) 
              VALUE "11. Preparer un template de Copybook (Y/N)"
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-CB-TEMPLATE-CHOICE COL 52.

           05 FILLER PIC X(30) 
              VALUE "12. Integrer un Copybook (Y/N)"
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-CB-CHOICE COL 45.

           05 FILLER PIC X(31) 
              VALUE "13. Ajouter un HelloCobol (Y/N)"
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-HELLOCBL-CHOICE COL 45.

           05 FILLER PIC X(34) 
              VALUE "14. Integrer des paragraphes (Y/N)"
              LINE PLUS 1 COL 10.
           05 PIC X USING WS-PARA-CHOICE COL 45.
      ******************************************************************
       01  ERROR-SCREEN FOREGROUND-COLOR IS 6.
           03 BLANK SCREEN.
           05 FILLER 
           VALUE 'SAISIE INCORRECTE' LINE 2 COL 5.

       PROCEDURE DIVISION.
       0000-MAIN-START.
           PERFORM 0001-INITIALIZE-START
              THRU 0001-INITIALIZE-END.
           
           OPEN OUTPUT OUTFILE.
           
           PERFORM 1000-WRITE-IDENTIFICATION-START
              THRU 1000-WRITE-IDENTIFICATION-END.

           IF WS-READ-CHOICE EQUAL "Y"
           OR  WS-WRITE-CHOICE EQUAL "Y" THEN
           PERFORM 2000-WRITE-ENVIRONMENT-START
              THRU 2000-WRITE-ENVIRONMENT-END
           END-IF.

           IF WS-READ-CHOICE EQUAL "Y"
           PERFORM 2001-WRITE-INFILE-START
              THRU 2001-WRITE-INFILE-END
           END-IF.

           IF WS-WRITE-CHOICE EQUAL "Y"
           PERFORM 2002-WRITE-OUTFILE-START
              THRU 2002-WRITE-OUTFILE-END
           END-IF.
                      
           IF WS-SORT-CHOICE EQUAL "Y"
           PERFORM 3003-WRITE-SORT-START
              THRU 3003-WRITE-SORT-END
           END-IF.

           PERFORM 3000-WRITE-DATA-START
              THRU 3000-WRITE-DATA-END.
           
           IF WS-READ-CHOICE EQUAL "Y"
               PERFORM 3001-FD-INFILE-START
                  THRU 3001-FD-INFILE-END
           END-IF.

           IF WS-WRITE-CHOICE EQUAL "Y"
              PERFORM 3002-FD-OUFILE-START
                 THRU 3002-FD-OUFILE-END
           END-IF.

           PERFORM 4000-WRITE-WS-START
              THRU 4000-WRITE-WS-END.

           IF WS-READ-CHOICE EQUAL "Y"
           PERFORM 2001-WS-INSTATUS-START
              THRU 2001-WS-INSTATUS-END
           END-IF.

           IF WS-WRITE-CHOICE EQUAL "Y"
           PERFORM 2002-WS-OUTSTATUS-START
              THRU 2002-WS-OUTSTATUS-END
           END-IF.

           PERFORM 5000-WRITE-PROC-START
              THRU 5000-WRITE-PROC-END.

           IF WS-MERGE-CHOICE EQUAL "Y"
              PERFORM 5001-WRITE-MERGE-START
                 THRU 5001-WRITE-MERGE-END
           END-IF.
      *     WS-BDD-ACCES-CHOICE
      *     WS-BDD-COUNT-CHOICE
      *     WS-CURSOR-CHOICE
      *     WS-BDD-UPDT-CHOICE
      *     WS-ROUTINE-CHOICE
      *     WS-ROUTINE-CALL-CHOICE
      *     WS-CB-TEMPLATE-CHOICE
      *     WS-CB-CHOICE
      *     WS-HELLOCBL-CHOICE
      *     WS-PARA-CHOICE

           CLOSE OUTFILE.


       0000-MAIN-END.
           STOP RUN.

       0001-INITIALIZE-START.

           ACCEPT MAIN-SCREEN.
          
       0001-INITIALIZE-END.
           EXIT.

       1000-WRITE-IDENTIFICATION-START.
           COPY 'identdiv.cpy'.
       1000-WRITE-IDENTIFICATION-END.
           EXIT.

       2000-WRITE-ENVIRONMENT-START.
           COPY 'envdiv.cpy'.
       2000-WRITE-ENVIRONMENT-END.
           EXIT.

       2001-WRITE-INFILE-START.
           COPY 'envread.cpy'.
       2001-WRITE-INFILE-END.
           EXIT.
       
       2001-WS-INSTATUS-START.
           COPY 'wsinstat.cpy'.
       2001-WS-INSTATUS-END.
           EXIT.

       2002-WRITE-OUTFILE-START.
           COPY 'envwrite.cpy'.
       2002-WRITE-OUTFILE-END.
           EXIT.
       
       2002-WS-OUTSTATUS-START.
           COPY 'wsoustat.cpy'.
       2002-WS-OUTSTATUS-END.
           EXIT.

       3000-WRITE-DATA-START.
           COPY 'datadiv.cpy'.
       3000-WRITE-DATA-END.
           EXIT.

       3001-FD-INFILE-START.
           COPY 'datainf.cpy'.
       3001-FD-INFILE-END.
           EXIT.

       3002-FD-OUFILE-START.
           COPY 'dataoutf.cpy'.
       3002-FD-OUFILE-END.
           EXIT.

       3003-WRITE-SORT-START.
           COPY 'datasort.cpy'.
       3003-WRITE-SORT-END.
           EXIT.

       4000-WRITE-WS-START.
           COPY 'wsection.cpy'.
       4000-WRITE-WS-END.
           EXIT.
       
       5000-WRITE-PROC-START.
           COPY 'procediv.cpy'
       5000-WRITE-PROC-END.
           EXIT.
       
       5001-WRITE-MERGE-START.
           COPY 'procmerge.cpy'
       5001-WRITE-MERGE-END.