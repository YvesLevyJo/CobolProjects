 ******************************************************************
      * Program name:    OPE                               
      * Original author: Yves                                
      *                                                
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  OPE.
       AUTHOR. Yves.
      *=================================================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPE    PIC X(4) VALUE 'none'.
       01  WS-NUM1   PIC 9(2) VALUE 0.
       01  WS-NUM2   PIC 9(2) VALUE 0.
       01  WS-RESULT PIC 9(6) VALUE 0.
      *=================================================================
       PROCEDURE DIVISION.
      ******************************************************************
      *    Addition
      ******************************************************************

           DISPLAY "================== ADDITION ===================".
           DISPLAY "Veuillez siaisir le premier chiffre Ã  additionner".
           ACCEPT WS-NUM1.
           DISPLAY "Veuillez siaisir le second chiffre Ã  additionner".       
           ACCEPT WS-NUM2.
    
           ADD WS-NUM1 WS-NUM2 TO WS-RESULT.
    
           DISPLAY WS-NUM1 SPACE " + " 
             SPACE WS-NUM2 SPACE "="
             SPACE WS-RESULT.

           INITIALIZE WS-NUM1.
           INITIALIZE WS-NUM2.

      ******************************************************************
      *    Soustration
      ******************************************************************
           DISPLAY "================= SOUSTRACTION ==================".
           DISPLAY "Veuillez siaisir la premiÃ¨re opÃ©rande".
           ACCEPT WS-NUM1.
           DISPLAY "Veuillez siaisir la seconde operande".
           ACCEPT WS-NUM2.
           MOVE WS-NUM1 TO WS-RESULT SUBTRACT WS-NUM2 FROM WS-RESULT.
           
           DISPLAY WS-NUM1 SPACE " - " 
            SPACE WS-NUM2 SPACE "="
            SPACE WS-RESULT.

           INITIALIZE WS-NUM1.
           INITIALIZE WS-NUM2.
      ******************************************************************
      *    Multiplication
      ******************************************************************
           DISPLAY "================ MULTIPLICATION =================".
           DISPLAY "Veuillez siaisir le premiÃ¨r facteur".
           ACCEPT WS-NUM1.
           DISPLAY "Veuillez siaisir le second facteur".
           ACCEPT WS-NUM2.
           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.
           
           DISPLAY WS-NUM1 SPACE " x " 
            SPACE WS-NUM2 SPACE "="
            SPACE WS-RESULT.

           INITIALIZE WS-NUM1.
           INITIALIZE WS-NUM2.          
  
      ******************************************************************
      *    Division
      ******************************************************************
           DISPLAY "================ DIVISION =================".
           DISPLAY "Veuillez siaisir le premierr facteur".
           ACCEPT WS-NUM1.
           DISPLAY "Veuillez siaisir le second facteur".
           ACCEPT WS-NUM2.
           DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.
           
           DISPLAY WS-NUM1 SPACE " / " 
            SPACE WS-NUM2 SPACE "="
            SPACE WS-RESULT.    
       STOP RUN.