           INITIALIZE OUTPUT-RECORD.           
           MOVE BLANK-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-11, "SELECT OUTFILE ASSIGN TO 'prog.cbl'"
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-11, "ORGANIZATION IS LINE SEQUENTIAL"
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           
           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-11, "FILE STATUS IS OUTF-STATUS."
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           