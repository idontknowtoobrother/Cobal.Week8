       IDENTIFICATION DIVISION. 
       PROGRAM-ID. VARY-SIZE-OF-LINE.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT LONG-NAME-FILE ASSIGN TO NAME-OF-FILE
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LONG-NAME-FILE
           RECORD IS VARYING IN SIZE 
           DEPENDING ON NAME-LENGTH.
       01 LONG-NAME-REC   PIC X(40).
          88 END-OF-FILE            VALUE HIGH-VALUES.
       
       WORKING-STORAGE SECTION.
       01 NAME-LENGTH     PIC 99.
       01 NAME-OF-FILE    PIC X(20).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Enter the name of the file :- " WITH NO ADVANCING
           ACCEPT NAME-OF-FILE.
           
           OPEN INPUT LONG-NAME-FILE.
           READ LONG-NAME-FILE
           AT END
              SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
                   DISPLAY "***" LONG-NAME-REC(1:NAME-LENGTH)
                   READ LONG-NAME-FILE
                   AT END
                      SET END-OF-FILE TO TRUE
                   END-READ
           END-PERFORM
           
           CLOSE LONG-NAME-FILE
           STOP RUN.