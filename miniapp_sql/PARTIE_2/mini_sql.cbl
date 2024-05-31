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

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME PIC X(20) VALUE "dgse".
       01  USERNAME PIC X(05) VALUE "cobol".
       01  PASSWORD PIC X(10) VALUE "cbl85".
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.
      ******************************************************************
       PROCEDURE DIVISION.
       1000-MAIN-START.
           MOVE ALL '-' TO WS-DASHLINE
           EXEC SQL 
              CONNECT :USERNAME IDENTIFIED BY :PASSWORD USING :DBNAME
           END-EXEC.

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
       2000-UPDATE-CLT-START.
           EXEC SQL
              UPDATE databank
              SET country_code = 'BE'
              WHERE AGE > 35 
              AND AGE < 45
              AND country_code = 'FR'
           END-EXEC.   
           IF SQLCODE = 0 THEN
              DISPLAY '> MAJ des country_code efféctuée.'
           ELSE
              PERFORM 1001-HANDLE-ERROR-START
                 THRU 1001-HANDLE-ERROR-END
           END-IF.
       2000-UPDATE-CLT-END.
       EXIT.

       2001-CORRECT-DATA-START.
            EXEC SQL
               UPDATE databank
               SET country = 'Belgium'
               WHERE country_code = 'BE'
            END-EXEC. 
            IF SQLCODE = 0 THEN
               DISPLAY '> CORRECTION des incohérences efféctuée.'
            ELSE
               PERFORM 1001-HANDLE-ERROR-START
                  THRU 1001-HANDLE-ERROR-END
            END-IF.
       2001-CORRECT-DATA-END.
       EXIT.

       2001-UPDATE-MAJ-START.
           EXEC SQL
             UPDATE databank
             SET spoken = UPPER(spoken),
             country = UPPER(country)
           END-EXEC.
           IF SQLCODE = 0 THEN
             DISPLAY '> MISE en MAJUSCULE efféctuée.'
           ELSE
             PERFORM 1001-HANDLE-ERROR-START
                THRU 1001-HANDLE-ERROR-END
           END-IF.
       2001-UPDATE-MAJ-END.
       EXIT.
