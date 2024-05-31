       IDENTIFICATION DIVISION.
       PROGRAM-ID. cpy.
       AUTHOR. Yves..
      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
           ASSIGN TO 
           'COBOL_FICHIER_MODELE_POUR_CLAUSE_COPY_Exercice.txt'
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL 
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT
           ASSIGN TO 'GROUPE-1.cpy'
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL 
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************
       DATA DIVISION.
        FILE SECTION.
           FD F-INPUT.
           01 REC-F-INPUT  PIC X(214).
           FD F-OUTPUT.
           01 REC-F-OUTPUT PIC X(80).
      ******************************************************************
        WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 INPUT-OK  VALUE '00'.
           88 INPUT-EOF  VALUE '10'.
       01  FS-OUTPUT PIC X(02).
           88 OUTPUT-OK  VALUE '00'.
           88 OUTPUT-EOF  VALUE '10'.
       01  ARRAY.
           03 ARRAY-TAF OCCURS 1 TO 99 TIMES 
                   DEPENDING ON WS-CONT
                   INDEXED BY WS-INDEX.
            05 FILLER   PIC X(13).

       01  WS-CONT  PIC 9(01) VALUE 1.
       01  VAR      PIC 9(02) VALUE 1.
       01  WS-SPACE-NUM PIC 9(02).

       01  WS-SAMPLE    PIC X(13).
       01  ws-SAMPLE-COUNT PIC 9(02).
       01  WS-POINTER      PIC 9(03) VALUE 1.
       01  WS-FULL-RECORD-LENGTH PIC 9(03).
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-START.
           PERFORM 1000-READ-IN-START
              THRU 1000-READ-IN-END.
       0000-MAIN-END.
           STOP RUN.

       1000-READ-IN-START.
           OPEN INPUT F-INPUT.
           OPEN OUTPUT F-OUTPUT.
           READ F-INPUT.
           INSPECT REC-F-INPUT 
            TALLYING WS-FULL-RECORD-LENGTH 
            FOR CHARACTERS.
           DISPLAY 'WS-FULL-RECORD-LENGTH' SPACE WS-FULL-RECORD-LENGTH.
           

           PERFORM 1001-COUNT-LABEL-START
              THRU 1001-COUNT-LABEL-END.
           

           MOVE '       01 GROUPE.' TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT.
              
           STRING '           03 FILLER PIC (', 
           ws-SAMPLE-COUNT,') VALUE' SPACE, "'",WS-SAMPLE,"'." 
           INTO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

       1000-READ-IN-END.
           CLOSE F-INPUT.
           CLOSE F-OUTPUT.

       1001-COUNT-LABEL-START.
           UNSTRING REC-F-INPUT DELIMITED BY SPACE INTO WS-SAMPLE.
           INSPECT WS-SAMPLE tallying WS-SAMPLE-COUNT FOR characters.
       1001-COUNT-LABEL-END.
           EXIT.

