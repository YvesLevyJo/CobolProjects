      *****************************************************************
      * Program name:    MYPROG                               
      * Original author: MYNAME                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 01/01/08 MYNAME  Created for COBOL class         
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  train.
       AUTHOR. yVES. 

      *****************************************************************
       ENVIRONMENT DIVISION. 

       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT TRAIN-INFILE ASSIGN TO "TRAIN1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-TR-ISTAT.
       
           SELECT TRAIN-OUTFILE ASSIGN TO "TRAIN3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-TR-OSTAT.
       
       DATA DIVISION.
       FILE SECTION.

       FD  TRAIN-INFILE
           RECORD CONTAINS 38 CHARACTERS.
       01  IN-TRAIN PIC X(38).

       FD  TRAIN-OUTFILE 
           RECORD CONTAINS 38 characters.
       COPY "TRAIN1-FDESCRIPTION.cpy".

       WORKING-STORAGE SECTION.
      * sTATUT DES DIFFÉRENTS FICHIERS 
       01  WS-TR-ISTAT PIC X(2).
           88 TR-ISTAT-EOF VALUE '10'.
       01  WS-TR-OSTAT PIC X(2).
      * compteur d'ENREGISTREMENT
       01   WS-COMPT   PIC 9(2) VALUE 0.
      *    compteur du nombre d'enregistrement

      *  description du rtain
       PROCEDURE DIVISION.
       OPEN INPUT TRAIN-INFILE
            output TRAIN-OUTFILE.
       
       PERFORM UNTIL TR-ISTAT-EOF
           ADD 1 TO WS-COMPT
           READ TRAIN-INFILE
           DISPLAY IN-TRAIN
      *   WRITE TRAIN-OUTFILE FROM TRAIN-PLANNING
       END-PERFORM.
       DISPLAY WS-COMPT.
       CLOSE TRAIN-INFILE TRAIN-OUTFILE.
 
       STOP RUN.
