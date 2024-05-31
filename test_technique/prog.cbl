      *****************************************************************
      * Program name:    prog                               
      * Original author: Yves
      *                                           
      * --------- ------------  --------------------------------------- 
      * 31/05/2024 MYNAME  Created for COBOL class                              
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  prog.
       AUTHOR. Yves. 
      *****************************************************************
       ENVIRONMENT DIVISION. 
 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT INFILE ASSIGN  TO "datassur.dat"
           ORGANIZATION IS LINE SEQUENTIAL 
           FILE STATUS IS INF-STATUS.

           SELECT OUTFILE ASSIGN TO "outfile.txt"
           ORGANIZATION IS LINE SEQUENTIAL 
           FILE STATUS IS OUT-STATUS.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
       FD  INFILE.
       01  IN-DESC.
           03 IN-IDENTIFIER PIC X(8).
           03 FILLER        PIC X(1).
           03 IN-LABEL      PIC X(14).
           03 FILLER        PIC X(1).
           03 IN-CONTRACT   PIC X(14).
           03 FILLER        PIC X(1).
           03 IN-COMPANY    PIC X(41).
           03 FILLER        PIC X(1).
           03 IN-STATUS     PIC X(8).
           03 FILLER        PIC X(1).
           03 IN-DATE       PIC X(8). 
           03 FILLER        PIC X(1).
           03 IN-NUMBER     PIC X(8).
           03 FILLER        PIC X(1).
           03 IN-AMOUNTH    PIC X(9).
           03 FILLER        PIC X(1).
           03 IN-CURRENCY   PIC X(3).

       FD  OUTFILE.
       01  OUT-DESC PIC X(200).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  INF-STATUS    PIC X(02).
           88 INF-EOF  VALUE '10'.
           88 INF-OK   VALUE '00'.

       01  OUT-STATUS    PIC X(02).
           88 OUT-EOF  VALUE '10'.
           88 OUT-OK   VALUE '00'.
       
       01  DISPLAY-PNT.
           03 PNT-TITLE    PIC X(19)  VALUE "Rapport de synthese".
           03 PNT-IDENTITY PIC X(16)  VALUE "Yves MBULU-NTOTO".
           03 PNT-LINE     PIC X(200) VALUE ALL "*".

       01  US-DATE.
           03 DATE-YEAR    PIC X(04).
           03 DATE-MOUNTH  PIC X(02).
           03 DATE-DAY     PIC X(02).

       01  FR-DATE.
           03 DATE-DAY     PIC X(02).
           03 FILLER       PIC X(01) VALUE "/".
           03 DATE-MOUNTH  PIC X(02).
           03 FILLER       PIC X(01) VALUE "/".
           03 DATE-YEAR    PIC X(04).

       01  FINAL-REPORT.
           03 IN-IDENTIFIER PIC X(8).
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-LABEL      PIC X(14).
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-CONTRACT   PIC X(14).
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-COMPANY    PIC X(41).
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-STATUS     PIC X(8).
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-DATE       PIC X(8). 
           03 FILLER        PIC X(1) VALUE '|'.
           03 IN-AMOUNTH    PIC X(9).
           03 FILLER        PIC X(1).
           03 IN-CURRENCY   PIC X(2).
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-START.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE.
           
           PERFORM 2000-HEADER-START
              THRU 2000-HEADER-END.

           PERFORM 1000-READ-START
              THRU 1000-READ-END.

       0000-MAIN-END. 
           CLOSE INFILE.
           CLOSE OUTFILE.
           STOP RUN.


       1000-READ-START.

           PERFORM UNTIL INF-EOF
              READ INFILE INTO IN-DESC
              MOVE CORR IN-DESC TO FINAL-REPORT
              MOVE FINAL-REPORT TO OUT-DESC
              WRITE OUT-DESC IN OUTFILE
           END-PERFORM.
       1000-READ-END.
           EXIT.

       2000-HEADER-START.
           MOVE FUNCTION CURRENT-DATE TO US-DATE.
           MOVE CORR US-DATE TO FR-DATE.

           INITIALIZE OUT-DESC.
           MOVE PNT-TITLE TO OUT-DESC.
           WRITE OUT-DESC IN OUTFILE.
           
           INITIALIZE OUT-DESC.
           MOVE PNT-IDENTITY TO OUT-DESC.
           WRITE OUT-DESC IN OUTFILE.
           
           INITIALIZE OUT-DESC.
           MOVE FR-DATE TO OUT-DESC.
           WRITE OUT-DESC IN OUTFILE.

           INITIALIZE OUT-DESC.
           MOVE PNT-LINE TO OUT-DESC.
           WRITE OUT-DESC IN OUTFILE.
       2000-HEADER-END.
           EXIT.

       3000-BODY-START.

       3000-BODY-END.
