           INITIALIZE OUTPUT-RECORD.           
           MOVE BLANK-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-7, "FD  OUTPUT-P1."
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-7, "01  OUTFILE PIC X(200)"
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           