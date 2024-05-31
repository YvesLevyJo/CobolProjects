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
           SELECT INFILE-P1 ASSIGN  TO 'assurances-part1.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-IN-1.

           SELECT INFILE-P2 ASSIGN  TO 'assurances-part2.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-IN-2.

           SELECT OUT-REP ASSIGN TO 'report.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OP.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE-P1.
       01  INSU-REC-P1             PIC X(123).

       FD  INFILE-P2.
       01  INSU-REC-P2             PIC X(123).

       FD  OUT-REP.
       01  INSU-REC-REPORT         PIC X(200).
      *****************************************************************
       WORKING-STORAGE SECTION.
      *    DEFINITION DES FILE STATUS
       01  FS-IN-1                  PIC X(2).
           88 IN-1-EOF         VALUE '10'.       
       01  FS-IN-2                  PIC X(2).
           88 IN-2-EOF         VALUE '10'.
       01  FS-OP                PIC X(2).
           88 WS-O-STATUS-OK        VALUE '00'.
           88 WS-O-STATUS-EOF       VALUE '10'.

      *    DEFINITION DE LA DATE
       01  WS-US-CURRENT-DATE.
           03 WS-CURRENT-YEAR   PIC 9(04).
           03 WS-CURRENT-MONTH  PIC 9(02).
           03 WS-CURRENT-DAY    PIC 9(02).

       01  WS-FR-CURRENT-DATE.
           03 WS-CURRENT-DAY    PIC 9(02).
           03 WS-FILLER         PIC X(01) VALUE "/".
           03 WS-CURRENT-MONTH  PIC 9(02).
           03 WS-FILLER         PIC X(01) VALUE "/".
           03 WS-CURRENT-YEAR   PIC 9(04).
      
      *    DISPLAY
       01  WS-INLINE-TEXT     PIC X(123).
       01  WS-STAR-LINE       PIC x(123).
       01  WS-DASH-LINE       PIC X(123).
      *    VARIABLE POUR BOUCLER SUR LE TABLEAU
       01  WS-INDEX           PIC 9(02) VALUE 1. 
      *    DEFINITION DU DU TABLEAU PRINCIPAL
       01  WS-INSU-TABLE. 
           03 WS-INSU  OCCURS 1 TO 99 TIMES
                       DEPENDING ON WS-INDEX.
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-ID        PIC X(04).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-ID-CLIENT PIC X(12).
               05 FILLER       PIC X(07) VALUE "|".
               05 WS-GROUP     PIC X(18).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-NAME      PIC X(29).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-LABEL     PIC X(41).
               05 FILLER       PIC X(07) VALUE "|".
               05 WS-STATUS    PIC X(14).
               05 FILLER       PIC X(06) VALUE "|".
               05 WS-FROM      PIC X(13).
               05 FILLER       PIC X(06) VALUE "|".
               05 WS-TO        PIC X(13).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-AMOUNT    PIC X(09).
               05 WS-EURO      PIC X(05).
               05 FILLER       PIC X(01) VALUE "|".
     
      *    tableau seccondaire ( conrtats)
       01  WS-LABEL-STATUS-TABLE.
           03 WS-LABEL-STATUS OCCURS 1 TO 99 TIMES   
                              DEPENDING ON WS-LS-INDEX.
              05 WS-LS-ID     PIC X(10).
              05 WS-LS-LABEL  PIC X(60).
              05 WS-LS-STATUS PIC X(30).

      *    DENOMBRER LES DIFFERENTS TYPES D'ABONNEMENT
       01  WS-COUNT-ACTIVE     PIC 9(02).
       01  WS-COUNT-SUSPENDED  PIC 9(02).
       01  WS-COUNT-CANCELED   PIC 9(02).
       01  WS-COUNT-RECORD1    PIC 9(02).
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-START.
           MOVE ALL "-" TO WS-DASH-LINE.
           MOVE ALL "*" TO WS-STAR-LINE.
           PERFORM 4000-HEADER-REPORT-START THRU 4000-HEADER-REPORT-END.
           PERFORM 1000-P1-HEADER-START     THRU 1000-P1-HEADER-END.
           PERFORM 1000-P1-READ-START       THRU 1000-P1-READ-END.
      *     PERFORM 1000-P1-WRITE-START      THRU 1000-P1-WRITE-END.
      *     PERFORM 2000-P2-HEADER-START     THRU 2000-P2-HEADER-END.
      *     PERFORM 2000-P2-READ-START       THRU 2000-P2-READ-END.
      *     PERFORM 2000-P2-WRITE-START      THRU 2000-P2-WRITE-END.

      *     PERFORM 3000-PART-LABEL-STATUS-WRITE-START
      *        THRU 3000-PART-LABEL-STATUS-WRITE-END.
      *     PERFORM 4000-FOOTER-REPORT-START
      *        THRU 4000-FOOTER-REPORT-END.
       0000-MAIN-END.
           STOP RUN.

       4000-HEADER-REPORT-START.
      *    oUVERTURE DU FICHIER EN ECRITURE 
           OPEN OUTPUT OUT-REP.
      *    ECRITURE DU TITRE
           WRITE INSU-REC-REPORT
           FROM "RAPPORT ASSURANCE CLIENT".
          
      *    ENREGISTRER LA DATE 
           MOVE FUNCTION CURRENT-DATE
           TO WS-US-CURRENT-DATE.
           MOVE CORR WS-US-CURRENT-DATE
           TO WS-FR-CURRENT-DATE.

           STRING "Généré le :" DELIMITED BY SIZE,
           SPACE WS-FR-CURRENT-DATE DELIMITED BY SIZE
           INTO WS-INLINE-TEXT.
           
           WRITE INSU-REC-REPORT FROM WS-INLINE-TEXT.

           WRITE INSU-REC-REPORT FROM SPACE.

       4000-HEADER-REPORT-END.           
           CLOSE OUT-REP.
       
       1000-P1-HEADER-START.
           WRITE INSU-REC-REPORT FROM WS-STAR-LINE.
           WRITE INSU-REC-REPORT FROM "PART 1".
           WRITE INSU-REC-REPORT FROM WS-STAR-LINE.
           WRITE INSU-REC-REPORT FROM SPACE.
           OPEN EXTEND OUT-REP.
                                 
           MOVE "ID"        TO WS-ID(1).
           MOVE "ID CLIENT" TO WS-ID-CLIENT(1).
           MOVE "GROUP"     TO WS-GROUP(1).
           MOVE "NAME"      TO WS-NAME(1).
           MOVE "LABEL"     TO WS-LABEL(1).
           MOVE "STATUS"    TO WS-STATUS(1).
           MOVE "FROM"      TO WS-FROM(1).
           MOVE "TO"        TO WS-TO(1).
           MOVE "AMOUNT"    TO WS-AMOUNT(1).

           WRITE INSU-REC-REPORT FROM WS-INSU(1).
       1000-P1-HEADER-END.
           CLOSE OUT-REP.
           EXIT.

       1000-P1-READ-START.
           OPEN INPUT INFILE-P1.

           INITIALIZE WS-INSU-TABLE.
           PERFORM UNTIL IN-1-EOF
           END PERFORM.
       
           UNSTRING INFILE-P1
           DELIMITED BY "*"
           INTO 
           WS-ID-CLIENT(WS-INDEX)
           WS-GROUP(WS-INDEX)
           WS-NAME(WS-INDEX)
           WS-LABEL(WS-INDEX)
           WS-STATUS(WS-INDEX)
           WS-FROM(WS-INDEX)
           WS-TO(WS-INDEX)
           WS-AMOUNT(WS-INDEX)

           EVALUATE WS-STATUS(WS-INDEX)
               WHEN "Actif"
                   ADD 1 TO WS-COUNT-ACTIVE
               WHEN "Suspendu"
                  ADD 1 TO WS-COUNT-SUSPENDED
               WHEN "Resilie"
                  ADD 1 TO WS-COUNT-CANCELED
               WHEN "Resilié"
                  ADD 1 TO WS-COUNT-CANCELED
               WHEN OTHER
                  CONTINUE
           END-EVALUATE

           MOVE WS-LABEL(WS-INDEX) 
             TO WS-LS-LABEL(WS-LS-INDEX)

             MOVE WS-STATUS(WS-INDEX) 
             TO WS-LS-STATUS(WS-LS-INDEX) 

             INITIALIZE WS-NUM
             MOVE WS-AMOUNT(WS-INDEX) TO WS-NUM
             ADD WS-NUM TO WS-TOTAL-AMOUNT-P1

             MOVE WS-INDEX TO WS-ID(WS-INDEX) 

             ADD 1 TO WS-INDEX
             ADD 1 TO WS-LS-INDEX
             ADD 1 TO WS-COUNT-RECORD1
           END-PERFORM.  
       1000-P1-READ-END.
           CLOSE INFILE-P1.




