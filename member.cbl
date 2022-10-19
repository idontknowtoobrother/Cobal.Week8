       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MEMBER-REPORT.
       AUTHOR. 62160246.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT MEMBER-FILE ASSIGN TO "member.dat"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MEMBER-REPORT-FILE ASSIGN TO "member.rpt"
              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD  MEMBER-REPORT-FILE.
       01 PRINT-LINE              PIC X(44).
       FD  MEMBER-FILE.
       01 MEMBER-REC.
          88 END-OF-MEMBER-FILE             VALUE HIGH-VALUE.
       05 MEMBER-ID               PIC X(5).
          05 MEMBER-NAME          PIC X(20).
          05 MEMBER-TYPE          PIC 9.
          05 MEMBER-GENDER        PIC X.
       WORKING-STORAGE SECTION. 
       01 PAGE-HEADER.
          05 FILLER               PIC X(44)
                                            VALUE
                "Rolling Greens Golf Club - Membership Report".
       01 PAGE-FOOTING.
          05 FILLER               PIC X(15) VALUE SPACES.
          05 FILLER               PIC X(7)  VALUE "PAGE : ".
          05 PRN-PAGE-NUM         PIC Z9.
       01 COLUMN-HEADING          PIC X(41)
                                            VALUE
             "MemberID   member Name       Type Gender".
       01 PRN-MEMBER-DETAIL-LINE.
          05 FILLER               PIC X     VALUE SPACES.
          05 PRN-MEMBER-ID        PIC X(5). 
          05 FILLER               PIC X(4)  VALUE SPACES.
          05 PRN-MEMBER-NAME      PIC X(20).
          05 FILLER               PIC XX    VALUE SPACES.
          05 PRN-MEMBER-TYPE      PIC X.
          05 FILLER               PIC X(4)  VALUE SPACES.
          05 PRN-GENDER           PIC X.
       01 LINE-COUNT              PIC 99    VALUE ZEROS.
          88 NEW-PAGE-REQUIRED              VALUE 40 THRU 99.
       01 PAGE-COUNT              PIC 99    VALUE ZEROS.

       PROCEDURE DIVISION.
       PROCESS-MEMBER-REPORT.
           OPEN INPUT MEMBER-FILE 
           OPEN OUTPUT MEMBER-REPORT-FILE 
           PERFORM READ-MEMBER-FILE 
           PERFORM PROCESS-PAGE UNTIL END-OF-MEMBER-FILE 


           CLOSE MEMBER-FILE, MEMBER-REPORT-FILE  
           GOBACK 
           .
       PROCESS-PAGE.
           PERFORM WRITE-HEADING
           PERFORM PROCESS-DETAIL UNTIL END-OF-MEMBER-FILE
              OR NEW-PAGE-REQUIRED
           PERFORM WRITE-FOOTER 
           .
       WRITE-HEADING.
           WRITE PRINT-LINE FROM PAGE-HEADER AFTER ADVANCING PAGE 
           WRITE PRINT-LINE FROM COLUMN-HEADING AFTER ADVANCING 2 LINES
           COMPUTE LINE-COUNT = LINE-COUNT + 3 
           COMPUTE PAGE-COUNT = PAGE-COUNT + 1
           MOVE ZEROS TO LINE-COUNT

           .
       PROCESS-DETAIL.
           MOVE MEMBER-ID TO PRN-MEMBER-ID
           MOVE MEMBER-NAME TO PRN-MEMBER-NAME 
           MOVE MEMBER-TYPE TO PRN-MEMBER-TYPE 
           MOVE MEMBER-GENDER TO PRN-GENDER
           WRITE PRINT-LINE FROM PRN-MEMBER-DETAIL-LINE
              AFTER ADVANCING 1 LINE
           COMPUTE LINE-COUNT = LINE-COUNT + 1
           PERFORM READ-MEMBER-FILE 
           .
       WRITE-FOOTER.
           MOVE PAGE-COUNT TO PRN-PAGE-NUM
           WRITE PRINT-LINE FROM PAGE-FOOTING AFTER ADVANCING 5 LINES
           .
       READ-MEMBER-FILE.
           READ MEMBER-FILE
           AT END
              SET END-OF-MEMBER-FILE TO TRUE
           END-READ
           .