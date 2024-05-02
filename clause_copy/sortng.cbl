       IDENTIFICATION DIVISION.
       PROGRAM-ID. sortng.
       AUTHOR. Yves.
      *=================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE CONTROL.
           SELECT ENTREE ASSIGN TO "fr-liste-dept.txt".
           SELECT SORTIE ASSIGN TO "liste-triee.txt".
           SELECT TRAVAIL ASSIGN TO "working.txt"

       DATA DIVISION.

       FD  ENTREE.
       01  ENTREE-DEPT.
           03 DEPT-ID-I PIC X(03).
           03 DEPT-NM   PIC X(23).

       FD  SORTIE.
       01  SORTIE-DEPT.
           03 DEPT-ID-O PIC X(03).
           03 DEPT-NM   PIC X(23).

       FD  TRAVAIL.
       01  TRAVAIL-DEPT.
           03 DEPT-ID-W PIC X(03).
           03 DEPT-NM   PIC X(23).

       PROCEDURE DIVISION.
           
           SORT TRAVAIL ON ASCENDING KEY DEPT-ID-W
               USING ENTREE GIVING SORTIE.
               DISPLAY "Sort Successful"