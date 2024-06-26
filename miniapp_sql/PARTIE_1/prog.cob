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
       
OCESQL*    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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

OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".


OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MAX(age) FROM DATABANK".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MIN(age) FROM DATABANK".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(066) VALUE "SELECT age, COUNT( * ) FROM DA"
OCESQL  &  "TABANK GROUP BY age ORDER BY age ASC".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(091) VALUE "SELECT last_name, first_name, "
OCESQL  &  "email, phrase FROM databank, phrase WHERE country = 'Belgi"
OCESQL  &  "um'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       1000-MAIN-START.
           MOVE ALL '-' TO WS-DASHLINE.
OCESQL*    EXEC SQL
OCESQL*       CONNECT :USERNAME IDENTIFIED BY :PASSWORD USING :DBNAME
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 5
OCESQL          BY REFERENCE PASSWORD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 20
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
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
OCESQL*          EXEC SQL
OCESQL*              ROLLBACK
OCESQL*          END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-HANDLE-ERROR-END.
           STOP RUN.
      ******************************************************************
       2000-SHOW-MAXAGE-START.
OCESQL*    EXEC SQL
OCESQL*       SELECT MAX(age)
OCESQL*       INTO :SQL-AGE-MAX FROM DATABANK
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-MAX
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
       2000-SHOW-MAXAGE-END.

       2000-SHOW-MINAGE-START.
OCESQL*    EXEC SQL
OCESQL*       SELECT MIN(age)           
OCESQL*       INTO :SQL-AGE-MIN FROM DATABANK
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-MIN
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

       2000-SHOW-MINAGE-END.
           EXIT.

       3000-SHOW-COUNT-START.
OCESQL*    EXEC SQL
OCESQL*          DECLARE CRAGE CURSOR FOR
OCESQL*          SELECT age, COUNT(*)
OCESQL*          FROM DATABANK
OCESQL*          GROUP BY age
OCESQL*          ORDER BY age ASC
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRAGE" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.

OCESQL*    EXEC SQL OPEN CRAGE END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRAGE" & x"00"
OCESQL     END-CALL.
              DISPLAY WS-DASHLINE.
              DISPLAY 'LISTE DES AGES'
              DISPLAY WS-DASHLINE.
              PERFORM UNTIL SQLCODE = +100
OCESQL*          EXEC SQL
OCESQL*             FETCH CRAGE
OCESQL*             INTO :SQL-AGE-VALUE, :SQL-AGE-COUNT
OCESQL*          END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-VALUE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-COUNT
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRAGE" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

                 IF SQLCODE = 0 THEN
                    DISPLAY 'Age :' SQL-AGE-VALUE," : " SQL-AGE-COUNT 
                  END-IF
              END-PERFORM.
OCESQL*    EXEC SQL CLOSE CRAGE END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRAGE" & x"00"
OCESQL     END-CALL
OCESQL    .
            
       3000-SHOW-COUNT-END.
           EXIT.

       4000-SHOW-DETAILS-START.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRBELG CURSOR FOR
OCESQL*        SELECT last_name, first_name, 
OCESQL*        email, phrase
OCESQL*        FROM databank, phrase
OCESQL*        WHERE country = 'Belgium'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRBELG" & x"00"
OCESQL          BY REFERENCE SQ0005
OCESQL     END-CALL.

OCESQL*    EXEC SQL
OCESQL*       OPEN CRBELG
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRBELG" & x"00"
OCESQL     END-CALL.

           DISPLAY WS-DASHLINE.
           DISPLAY 'Comptage des belges'.
           DISPLAY WS-DASHLINE.

           PERFORM UNTIL SQLCODE  = 100
OCESQL*        EXEC SQL
OCESQL*           FETCH CRBELG
OCESQL*           INTO :SQL-NOM, :SQL-PRENOM, :SQL-MAIL, :SQL-PHRASE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-MAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-PHRASE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRBELG" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

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
OCESQL*    EXEC SQL
OCESQL*       CLOSE CRBELG
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "prog_CRBELG" & x"00"
OCESQL     END-CALL
OCESQL    .
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
           EXIT.
           EXIT.
