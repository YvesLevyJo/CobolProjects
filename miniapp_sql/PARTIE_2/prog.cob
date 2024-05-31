      *****************************************************************
      *    Ce programme met a jour le code pays de certains clients
      *    avant de corriger les incohérence pays/code pays
      *    et de mettre en majuscule les cplonne pays et langue parlée          
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  mini_sql.
       AUTHOR. Yves.
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DASHLINE  PIC X(200).  

OCESQL*    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME PIC X(20) VALUE "dgse".
       01  USERNAME PIC X(05) VALUE "cobol".
       01  PASSWORD PIC X(10) VALUE "cbl85".
OCESQL*    EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*    EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".
      ******************************************************************
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(091) VALUE "UPDATE databank SET country_co"
OCESQL  &  "de = 'BE' WHERE AGE > 35 AND AGE < 45 AND country_code = '"
OCESQL  &  "FR'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(065) VALUE "UPDATE databank SET country = "
OCESQL  &  "'Belgium' WHERE country_code = 'BE'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(068) VALUE "UPDATE databank SET spoken = U"
OCESQL  &  "PPER(spoken), country = UPPER(country)".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       1000-MAIN-START.
           MOVE ALL '-' TO WS-DASHLINE
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

              PERFORM 2000-UPDATE-CLT-START
                 THRU 2000-UPDATE-CLT-END.

              PERFORM 2001-CORRECT-DATA-START
                 THRU 2001-CORRECT-DATA-END.

              PERFORM 2001-UPDATE-MAJ-START
                 THRU 2001-UPDATE-MAJ-END.

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
       2000-UPDATE-CLT-START.
OCESQL*    EXEC SQL
OCESQL*       UPDATE databank
OCESQL*       SET country_code = 'BE'
OCESQL*       WHERE AGE > 35 
OCESQL*       AND AGE < 45
OCESQL*       AND country_code = 'FR'
OCESQL*    END-EXEC.   
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.
           IF SQLCODE = 0 THEN
              DISPLAY '> MAJ des country_code efféctuée.'
           ELSE
              PERFORM 1001-HANDLE-ERROR-START
                 THRU 1001-HANDLE-ERROR-END
           END-IF.
       2000-UPDATE-CLT-END.
       EXIT.

       2001-CORRECT-DATA-START.
OCESQL*     EXEC SQL
OCESQL*        UPDATE databank
OCESQL*        SET country = 'Belgium'
OCESQL*        WHERE country_code = 'BE'
OCESQL*     END-EXEC. 
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
            IF SQLCODE = 0 THEN
               DISPLAY '> CORRECTION des incohérences efféctuée.'
            ELSE
               PERFORM 1001-HANDLE-ERROR-START
                  THRU 1001-HANDLE-ERROR-END
            END-IF.
       2001-CORRECT-DATA-END.
       EXIT.

       2001-UPDATE-MAJ-START.
OCESQL*    EXEC SQL
OCESQL*      UPDATE databank
OCESQL*      SET spoken = UPPER(spoken),
OCESQL*      country = UPPER(country)
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
           IF SQLCODE = 0 THEN
             DISPLAY '> MISE en MAJUSCULE efféctuée.'
           ELSE
             PERFORM 1001-HANDLE-ERROR-START
                THRU 1001-HANDLE-ERROR-END
           END-IF.
       2001-UPDATE-MAJ-END.
       EXIT.
       EXIT.
       EXIT.
