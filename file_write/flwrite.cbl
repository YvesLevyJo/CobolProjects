      *****************************************************************       
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  flwrite.
       AUTHOR. Yves. 

      *****************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT INFILE ASSIGN  TO 'assurances.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.

           SELECT OUTFILE ASSIGN TO 'rapprot.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-O-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE
           RECORD CONTAINS 126 CHARACTERS
           RECORDING MODE IS F.

       01  E-INFILE PIC X(126).
       01  INFILE-DATA REDEFINES E-INFILE.
           05 INFILE-DATA-ID          PIC X(8).              
           05 FILLER                  PIC X.
           05 INFILE-DATA-REF         PIC X(14).       
           05 FILLER                  PIC X.
           05 INFILE-DATA-CONTRACT    PIC X(14).            
           05 FILLER                  PIC X.
           05 INFILE-DATA-ENTREPISE   PIC X(41).             
           05 FILLER                  PIC X.
           05 INFILE-DATA-STATUS      PIC X(8).          
           05 FILLER                  PIC X.
           05 INFILE-DATA-CODE1       PIC X(8).         
           05 FILLER                  PIC X.
           05 INFILE-DATA-CODE2       PIC X(8).                   
           05 FILLER                  PIC X.
           05 FILE-DATA-AMOUNTH       PIC 9(9).
           05 FILLER                  PIC X.
           05 FILE-DATA-CURRENCY      PIC X(3).
        
        FD OUTFILE
           RECORD CONTAINS 126 CHARACTERS
           RECORDING MODE IS F.         
       01  E-OUTFILE.
           05 OUTFILE-DATA-ID          PIC X(8).              
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-REF         PIC X(14).       
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-CONTRACT    PIC X(14).            
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-ENTREPISE   PIC X(41).             
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-STATUS      PIC X(8).          
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-CODE1       PIC X(8).         
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-CODE2       PIC X(8).                   
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-AMOUNTH       PIC 9(9).
           05 FILLER                  PIC X.
           05 OUTFILE-DATA-CURRENCY      PIC X(3).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WS-STATUS                  PIC X(2).
           88 WS-STATUS-EOF         VALUE '10'.
       01  WS-O-STATUS                PIC X(2).
           88 WS-O-STATUS-OK        VALUE '00'.
           88 WS-O-STATUS-EOF       VALUE '10'.

       01  Ws-NB                         PIC 99.

       PROCEDURE DIVISION.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE.
           PERFORM UNTIL WS-STATUS-EOF
              ADD 1 TO WS-NB
              READ INFILE
              IF (WS-NB =3) OR (WS-NB=7) THEN
                 DISPLAY INFILE-DATA
              END-IF
              WRITE E-OUTFILE FROM INFILE-DATA
           END-PERFORM.
           CLOSE INFILE.
           CLOSE OUTFILE.
           STOP RUN.
           
