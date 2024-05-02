       IDENTIFICATION DIVISION.
       PROGRAM-ID. clcopy.
       AUTHOR. Yves.
      *=================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLOYE ASSIGN TO 'fichierclient.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-EMP-STATUS.

           SELECT F-DEPT ASSIGN TO 'fr-liste-dept.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-DEPT-STATUS.


       DATA DIVISION.
       FILE SECTION.
    
           COPY 'FCLIENT' REPLACING ==:CLIENT:== by ==EMPLOYE==.
           COPY 'FDEPT'.
       
      *FD  DEPART CONTAINS 
      *01  DEP COPY 'FDEPT'.
       WORKING-STORAGE SECTION.
       01  WS-EMP-STATUS    PIC X(02).
           88 WS-DEPT-STATUS-OK VALUE '10'.
       01  WS-DEPT-STATUS    Pic X(02).
           88 WWS-DEPT-STATUS-OK VALUE '00'.
       01  WS-RAP-STATUS      PIC X(02).
           88 S-RAP-STATUS-OK VALUE '00'.
       
       01  WS-EMPLOYEE.
           05 WS-EMP-ID       PIC 9(8).  
           05 WS-EMP-NOM      PIC x(20). 
           05 WS-EMP-PRENOM   PIC x(20).   
           05 WS-EMP-POSTE    PIC x(14).
           05 WS-EMP-SALAIRE  PIC 9(7).
           05 WS-EMP-AAGENCE  PIC x(3).
          
       PROCEDURE DIVISION.


           STOP RUN.
