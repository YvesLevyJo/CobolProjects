       IDENTiFICATION DIVISION.
       PROGRAM-ID. mini_sql.
       AUTHOR. Yves.
           
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-O-STATUS  PIC X(02).
       01  WS-IDX       PIC 99.
       01  WS-DASHLINE  PIC X(99).

       01  WS-AGE-MAX            PIC 99.   
       01  WS-AGE-MIN            PIC 99.   
       01  WS-COUNT              PIC 9(3).

       01  WS-NOM     PIC X(50).
       01  WS-PRENOM  PIC X(50).
       01  WS-MAIL    PIC X(50).
       01  WS-PHRASE  PIC X(50).
       
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME PIC X(20) VALUE "dgse".
       01  USERNAME PIC X(05) VALUE "cobol".
       01  PASSWORD PIC X(10) VALUE "cbl85".

       01  SQL-RESULTS.
           03 SQL-AGE-MAX   PIC 99.
           03 SQL-AGE-MIN   PIC 99.
           03 SQL-AGE-VALUE PIC 99.
           03 SQL-AGE-COUNT PIC 99.
       
       01  SQL-RESULTS-BELGIUM.
           03 SQL-NOM     PIC X(50).
           03 SQL-PRENOM  PIC X(50).
           03 SQL-MAIL    PIC X(50).
           03 SQL-PHRASE  PIC X(50).

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.


       PROCEDURE DIVISION.
       1000-MAIN-START.
           MOVE ALL '-' TO WS-DASHLINE.
           EXEC SQL
              CONNECT :USERNAME IDENTIFIED BY :PASSWORD USING :DBNAME
           END-EXEC.

           IF SQLCODE NOT = ZERO
              PERFORM 1001-HANDLE-ERROR-START
                 THRU 1001-HANDLE-ERROR-END
           END-IF.


           PERFORM 2000-SHOW-MAXAGE-START
              THRU 2000-SHOW-MAXAGE-END.

           PERFORM 2000-SHOW-MINAGE-START
              THRU 2000-SHOW-MINAGE-END.
           
           PERFORM 3000-SHOW-COUNT-START
              THRU 3000-SHOW-COUNT-END.

           PERFORM 2000-SHOW-RESULT-START
              THRU 2000-SHOW-RESULT-END.
          
           PERFORM 4000-SHOW-DETAILS-START
              THRU 4000-SHOW-DETAILS-END.
           
       1000-MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
      ******************************************************************
       1001-HANDLE-ERROR-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
              *> TO RESTART TRANSACTION, DO ROLLBACK.
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-HANDLE-ERROR-END.
           STOP RUN.
      ******************************************************************
       2000-SHOW-MAXAGE-START.
           EXEC SQL
              SELECT MAX(age)
              INTO :SQL-AGE-MAX FROM DATABANK
           END-EXEC.
       2000-SHOW-MAXAGE-END.

       2000-SHOW-MINAGE-START.
           EXEC SQL
              SELECT MIN(age)           
              INTO :SQL-AGE-MIN FROM DATABANK
           END-EXEC.

       2000-SHOW-MINAGE-END.
           EXIT.

       3000-SHOW-COUNT-START.
           EXEC SQL
                 DECLARE CRAGE CURSOR FOR
                 SELECT age, COUNT(*)
                 FROM DATABANK
                 GROUP BY age
                 ORDER BY age ASC
           END-EXEC.

           EXEC SQL OPEN CRAGE END-EXEC.
              DISPLAY WS-DASHLINE.
              DISPLAY 'LISTE DES AGES'
              DISPLAY WS-DASHLINE.
              PERFORM UNTIL SQLCODE = +100
                 EXEC SQL
                    FETCH CRAGE
                    INTO :SQL-AGE-VALUE, :SQL-AGE-COUNT
                 END-EXEC

                 IF SQLCODE = 0 THEN
                    DISPLAY 'Age :' SQL-AGE-VALUE," : " SQL-AGE-COUNT 
                  END-IF
              END-PERFORM.
           EXEC SQL CLOSE CRAGE END-EXEC.
            
       3000-SHOW-COUNT-END.
           EXIT.

       4000-SHOW-DETAILS-START.
           EXEC SQL
               DECLARE CRBELG CURSOR FOR
               SELECT last_name, first_name, 
               email, phrase
               FROM databank, phrase
               WHERE country = 'Belgium'
           END-EXEC.

           EXEC SQL
              OPEN CRBELG
           END-EXEC.

           DISPLAY WS-DASHLINE.
           DISPLAY 'Comptage des belges'.
           DISPLAY WS-DASHLINE.

           PERFORM UNTIL SQLCODE  = 100
               EXEC SQL
                  FETCH CRBELG
                  INTO :SQL-NOM, :SQL-PRENOM, :SQL-MAIL, :SQL-PHRASE
               END-EXEC

                 IF SQLCODE = 0 
                 
                 MOVE SQL-NOM TO WS-NOM
                 MOVE SQL-PRENOM TO WS-PRENOM
                 MOVE SQL-MAIL TO WS-MAIL
                 MOVE SQL-PHRASE TO WS-PHRASE

                 DISPLAY 'Nom :' SPACE WS-NOM               
                 DISPLAY 'Prenom :' SPACE WS-PRENOM
                 DISPLAY 'Mail :' SPACE WS-MAIL
                 DISPLAY 'Phrase :' SPACE WS-PHRASE
                 DISPLAY '------------'          
                 END-IF
           END-PERFORM.
           EXEC SQL
              CLOSE CRBELG
           END-EXEC.
       4000-SHOW-DETAILS-END.

       2000-SHOW-RESULT-START.
           DISPLAY WS-DASHLINE.
           DISPLAY 'AGE Max et Max'.
           DISPLAY WS-DASHLINE.
           MOVE SQL-AGE-MAX TO WS-AGE-MAX.
           MOVE SQL-AGE-Min TO WS-AGE-MIN.
           DISPLAY "AGE MAX :" SPACE WS-AGE-MAX.
           DISPLAY "AGE MIN :" SPACE WS-AGE-MIN.


       2000-SHOW-RESULT-END.
           EXIT.
