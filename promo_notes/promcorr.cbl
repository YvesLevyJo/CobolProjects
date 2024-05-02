      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. file.
       AUTHOR. Yves.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.

      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).
           66 R-S-FULLNAME RENAMES R-S-LASTNAME THRU R-S-FIRSTNAME.
     

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 2000 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(2000).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           03 STUDENT-LGTH     PIC 9(03) VALUE 1.
           03 STUDENT  
               OCCURS 1 TO 999 TIMES
               DEPENDING ON STUDENT-LGTH
               INDEXED BY IDX-STUDENT.
                   05 S-LASTNAME   PIC X(20).
                   05 S-FIRSTNAME  PIC X(20).
                   05 S-AGE        PIC 9(02).

       01  WS-CS-POS          PIC 9(02).
       01  WS-CS-IDX          PIC 9(01) VALUE 1.
       01  DATA-COURSE.
           03 COURSE-LGTH     PIC 9(03) VALUE 1.
           03 COURSE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-COURSE. 
                   05 C-COEF       PIC 9V9.
                   05 C-LABEL      PIC X(25).

       01  DATA-GRADE.
           03 GRADE-LGTH      PIC 9(03) VALUE 1.
           03 GRADE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON GRADE-LGTH
               INDEXED BY IDX-GRADE. 
                   05 G-S-FULLNAME     PIC X(40).
                   05 G-C-LABEL        PIC X(25).
                   05 G-GRADE          PIC 99V99.
       01  WS-BUFFER   PIC X(03) VALUE SPACE.
           88  WS-VALUE-NOT-PRESENT VALUE 'Y'.

       01  WS-I               PIC 999 VALUE 1.
       01  WS-J               PIC 999 VALUE 1.
       
       01  WS-PNT.
           03 WS-PNT-NBR      PIC Z9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.
      *    Gestion des affichages
       01  WS-DATA-LIGNE.
           03 WS-PNT-FN       PIC X(20).
           03 WS-PNT-LN       PIC X(20).
           03 WS-PNT-AVG      PIC Z99,99.
           03 WS-PNT-MARKS OCCURS 1 TO 9999
               DEPENDING ON COURSE-LGTH
               INDEXED BY MARKS-INDX.
               05 PNT-MARKS PIC Z99,99.
               
       PROCEDURE DIVISION.
       0000-MAIN-START.
           PERFORM 7000-READ-START THRU 7000-READ-END. 
           PERFORM 7100-WRITE-START THRU 7100-WRITE-END.
       0000-MAIN-END.
           STOP RUN.
      ****************************************************************** 
       7000-READ-START.
           OPEN INPUT F-INPUT.          

           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ERROR INPUT FILE'
               GO TO 7000-READ-END
           END-IF.

           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               IF F-INPUT-STATUS-EOF
                   GO TO 7000-READ-END
               END-IF

               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 8010-HANDLE-STUDENT-START 
                           THRU 8010-HANDLE-STUDENT-END
                   WHEN '02'
                       PERFORM 8020-HANDLE-COURSE-START 
                           THRU 8020-HANDLE-COURSE-END
                       PERFORM 8030-HANDLE-GRADE-START
                           THRU 8030-HANDLE-GRADE-END
                   WHEN OTHER
                       DISPLAY "Le type d'enregistrement" SPACE
                       REC-F-INPUT-2 SPACE "n'est pas géré."
                       "Arret du programme"
                       PERFORM 2050-CLOSE-FILE-START
                          THRU 2050-CLOSE-FILE-END
                       GO TO 0000-MAIN-END
           END-PERFORM.

       7000-READ-END.
           SET GRADE-LGTH COURSE-LGTH STUDENT-LGTH DOWN BY 1.
           CLOSE F-INPUT.  
      ******************************************************************
       7100-WRITE-START.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 9010-HEADER-START   THRU 9010-HEADER-END.

           PERFORM 9030-BODY-START     THRU 9030-BODY-END.

           PERFORM 9020-FOOTER-START   THRU 9020-FOOTER-END.
       7100-WRITE-END.
           CLOSE F-OUTPUT.
      ******************************************************************  
       8010-HANDLE-STUDENT-START.
           MOVE R-S-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGTH).
           MOVE R-S-LASTNAME   TO S-LASTNAME(STUDENT-LGTH).
           MOVE R-S-AGE        TO S-AGE(STUDENT-LGTH).

           SET STUDENT-LGTH UP BY 1.           
       8010-HANDLE-STUDENT-END.
      ****************************************************************** 
       8020-HANDLE-COURSE-START.
           INITIALIZE WS-BUFFER.
           SET IDX-COURSE TO 1.

           SEARCH COURSE VARYING IDX-COURSE
               AT END
                   SET WS-VALUE-NOT-PRESENT TO TRUE
               WHEN C-LABEL(IDX-COURSE) = R-C-LABEL
                   GO TO 8020-HANDLE-COURSE-END 
           END-SEARCH.

           IF WS-VALUE-NOT-PRESENT
               SET COURSE-LGTH UP BY 1
               MOVE R-C-COEF   TO C-COEF(COURSE-LGTH)
               MOVE R-C-LABEL  TO C-LABEL(COURSE-LGTH)
           END-IF.
       8020-HANDLE-COURSE-END.
      ****************************************************************** 
       8030-HANDLE-GRADE-START.
           SET GRADE-LGTH UP BY 1.
           STRING S-FIRSTNAME(STUDENT-LGTH) S-LASTNAME(STUDENT-LGTH) 
               DELIMITED BY SIZE 
           INTO G-S-FULLNAME(GRADE-LGTH).

           MOVE R-C-LABEL TO G-C-LABEL(GRADE-LGTH).
           DISPLAY R-C-GRADE.
      
           MOVE R-C-GRADE TO G-GRADE(GRADE-LGTH).
       8030-HANDLE-GRADE-END.
      ****************************************************************** 
       9010-HEADER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(200:1).
           MOVE 'BULLETIN DE NOTES' TO REC-F-OUTPUT(33:20).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT. 

           INITIALIZE REC-F-OUTPUT.
           MOVE "-" TO REC-F-OUTPUT(1:1)
           MOVE "FIRSTNAME" TO REC-F-OUTPUT(2:9).
           MOVE ALL "-" TO REC-F-OUTPUT(11:11).
           MOVE "-" TO REC-F-OUTPUT(23:1).
           MOVE "LASTNAME" TO REC-F-OUTPUT(24:8),
           MOVE ALL "-" TO REC-F-OUTPUT(32:13).
           SET WS-CS-POS TO 46.
           PERFORM VARYING WS-CS-IDX FROM 1 BY 1 UNTIL 
           WS-CS-IDX > COURSE-LGTH

               STRING "C",WS-CS-IDX INTO REC-F-OUTPUT(WS-CS-POS:2)
               SET WS-CS-POS UP BY 5
           END-PERFORM.

           WRITE REC-F-OUTPUT.         
       9010-HEADER-END.
      ****************************************************************** 
       9020-FOOTER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(200:1).
           MOVE 'NOMBRE DE' TO REC-F-OUTPUT(33:9).

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'ELEVES'   TO REC-F-OUTPUT(43:9).
           MOVE STUDENT-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'NOTES'    TO REC-F-OUTPUT(43:9).
           MOVE GRADE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'COURS'     TO REC-F-OUTPUT(43:9).
           MOVE COURSE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.
       9020-FOOTER-END.
      ****************************************************************** 
       9030-BODY-START.
      *    Boucler sur le nombre
           PERFORM VARYING IDX-STUDENT FROM 1 BY 1 UNTIL 
               IDX-STUDENT > STUDENT-LGTH
               INITIALIZE REC-F-OUTPUT
      
                 PERFORM VARYING IDX-GRADE FROM 1 BY 1 UNTIL
                 IDX-GRADE > GRADE-LGTH
                 
                 STRING S-LASTNAME(IDX-STUDENT) 
                 SPACE  S-FIRSTNAME(IDX-STUDENT) INTO REC-F-OUTPUT
                 
                 SET WS-CS-POS TO 46 
                 STRING G-GRADE(GRADE-LGTH) DELIMITED BY STUDENT-LGTH
                 INTO REC-F-OUTPUT(WS-CS-POS:5)
                 SET WS-CS-POS UP BY 5
                 
                 END-PERFORM
                 WRITE REC-F-OUTPUT IN F-OUTPUT
                 
           END-PERFORM.
       9030-BODY-END.

      ****************************************************************** 
       2050-CLOSE-FILE-START.
           CLOSE F-INPUT.
       2050-CLOSE-FILE-END.
       
