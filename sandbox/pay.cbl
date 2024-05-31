       IDENTIFICATION DIVISION.
       PROGRAM-ID. pay.
       AUTHOR. Yves.
      *=================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE CONTROL.
           SELECT CLIENT-INPUT 
           ASSIGN TO 'FICHIERCLIENT.txt'
           FILE STATUS IS FS-CLIENT.
      *=================================================================
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  CLIENT-INPUT.
       01  CLIENT-REC.
           REC-C-ID       PIC X(9). 
           REC-C-NOM      PIC X(20).
           REC-C-PRENOM   PIC X(20).
           REC-C-JOB      PIC X(14).
           REC-C-REGION   PIC 9(02).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  FS-CLIENT      PIC X(02).
           88 CLIENT-FS-EOF VALUE '10'.
           88 CLIENT-FS-OK  VALUE '00'.
      *=================================================================
       PROCEDURE DIVISION.

      ******************************************************************