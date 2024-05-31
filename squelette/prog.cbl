       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST 2².
       AUTHOR. YVES.
      ******************************************************************
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT INFILE ASSIGN TO 'INFILE.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS INF-STATUS.
      ******************************************************************
           DATA DIVISION.

       FD  INNPUT-P1.
       01  INFILE PIC X(200)

       WORKING-STORAGE-SECTION.

       01  INF-STATUS PIC X(02).
