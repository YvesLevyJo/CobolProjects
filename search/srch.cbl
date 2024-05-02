       IDENTIFICATION DIVISION.
       PROGRAM-ID. srch.
       AUTHOR. Yves.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-DEPT ASSIGN TO 'fr-liste-dept.txt'
       ORGANIZATION IS LINE SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL
       FILE STATUS IS WS-DEPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD F-DEPT 
           RECORD CONTAINS 52 CHARACTERS 
           RECORDING MODE IS F.

       01  R-DEPT.
           03 R-DEPT-ID        PIC X(03).
           03 R-DEPT-REGION    PIC X(23).
           03 R-DEPT-DEPT      PIC X(26).

      ******************************************************************         
       WORKING-STORAGE SECTION.
       01  WS-DEPT-STATUS      PIC X(02).
           88  DEPT-ST-EOF    VALUE '10'.
           88  DEPT-ST-OK     VALUE '00'.

       01  WS-SCH-INPUT        PIC X(03).

       01  WS-ID-D           pic 9(3) VALUE 1.
       01  DEP-DATAS.
           03 DEP-TAB OCCURS 101 TIMES.
               05 F-DEPT-ID        PIC X(03).
               05 F-DEPT-DEP       PIC X(23).
               05 F-DEPT-REGION    PIC X(26).
      ******************************************************************     
       PROCEDURE DIVISION.
       0000-MAIN-START.
           PERFORM 0001-DISPLAY-START
              THRU 0001-DISPLAY-END.
           
           PERFORM 0003-READ-INPUT-START
              THRU 0003-READ-INPUT-END.
           
           PERFORM 0002-SEARCH-DPT-START
              THRU 0002-SEARCH-DPT-END.
       0000-MAIN-END.           
           STOP RUN.

       0001-DISPLAY-START.
           DISPLAY "Entrez le numéro du département recherché.".
           ACCEPT WS-SCH-INPUT.
       0001-DISPLAY-END.
           EXIT.
       
       0002-SEARCH-DPT-START.
       0002-SEARCH-DPT-END.

           EXIT.
       
       0003-READ-INPUT-START.
           INITIALIZE WS-ID-D.
           OPEN INPUT F-DEPT.
           IF DEPT-ST-OK
               PERFORM UNTIL WS-ID-D > 101
                READ F-DEPT
                MOVE R-DEPT TO DEP-TAB(WS-ID-D)
                ADD 1 TO WS-ID-D
               END-PERFORM
           END-IF.
       0003-READ-INPUT-END.
           CLOSE F-DEPT.
       
       

