           INITIALIZE OUTPUT-RECORD.           
           MOVE BLANK-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           INITIALIZE OUTPUT-RECORD.
           STRING MARGIN-7, "01  OUTF-STATUS PIC X(02)."
           INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           