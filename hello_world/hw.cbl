       IDENTIFICATION DIVISION.
      *================================================================*
      *    Ce programme demande son nom a un utilisateur              *
      *      à l'infinie                                               *
      *================================================================*
       PROGRAM-ID. hw.
       AUTHOR. Yves.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NOM PIC X(12).
       01  WS-CPT PIC 9 VALUE 0.
       
       PROCEDURE DIVISION.
      *================================================================*
      *    Pour repeter a l'infinie                               *
      *******************************************************************
           PERFORM 8000-BOUCLE-DEB
              THRU 8000-BOUCLE-FIN
              UNTIL WS-CPT > 3.
           STOP RUN.
      *================================================================*
       8000-BOUCLE-DEB.    
           DISPLAY 'BONJOUR Cobol'.
           DISPLAY 'Quel est ton nom'.

           ACCEPT WS-NOM.
           
           DiSPLAY 'Bonjour' SPACE WS-NOM.
           ADD 1 TO WS-CPT.
       8000-BOUCLE-FIN. 
          
           