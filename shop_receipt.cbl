       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SHOP-RECEIPT.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT SHOP-RECEIPT-FILE ASSIGN TO "shop_receipts.dat"
                 ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION. 
       FILE SECTION. 
       FD  SHOP-RECEIPT-FILE.
       01 SHOP-HEADER.
          88 END-OF-SHOP-RECEIPTS-FILE                 VALUE HIGH-VALUES
           .
       05 RECORD-TYPE-CODE              PIC X.
          88 IS-SHOP-HEADER                            VALUE "H".
          88 IS-SHOP-SALE                              VALUE "S".
       05 SHOP-ID                       PIC X(5).
          05 SHOP-LOCATION              PIC X(30).
       01 SALE-RECEIPT.
          05 RECORD-TYPE-CODE           PIC X.
          05 ITEM-ID                    PIC X(8).
          05 QTY-SOLD                   PIC 9(3).
          05 ITEM-COST                  PIC 999V99.

       WORKING-STORAGE SECTION. 
       01 PRN-SHOP-SALES-TOTAL.
          05 FILLER                     PIC X(21)      VALUE
                "TOTAL SALES FOR SHOP".
          05 PRN-SHOP-ID                PIC X(5).
          05 PRN-SHOP-TOTAL             PIC $$$$,$$9.99.
       01 SHOP-TOTAL                    PIC 9(5)V99.
       PROCEDURE DIVISION .
       SHOP-SALE-PROCESS.
           OPEN INPUT SHOP-RECEIPT-FILE 
           PERFORM READ-SHOP-RECEIPT-FILE.
           PERFORM PROCESS-SHOP-HEADER UNTIL END-OF-SHOP-RECEIPTS-FILE 

           CLOSE SHOP-RECEIPT-FILE 
           GOBACK 
           .
       
       PROCESS-SHOP-HEADER.
      *    DISPLAY "SHOP " SHOP-ID 
           MOVE SHOP-ID TO PRN-SHOP-ID
           MOVE ZEROS TO SHOP-TOTAL.
           PERFORM READ-SHOP-RECEIPT-FILE
           PERFORM PROCESS-SHOP-ITEM UNTIL END-OF-SHOP-RECEIPTS-FILE
              OR IS-SHOP-HEADER 
           MOVE SHOP-TOTAL TO PRN-SHOP-TOTAL
           DISPLAY PRN-SHOP-SALES-TOTAL
           .
       
       PROCESS-SHOP-ITEM.
      *    DISPLAY "ITEM " ITEM-ID 
           COMPUTE SHOP-TOTAL = SHOP-TOTAL +(QTY-SOLD * ITEM-COST)
           PERFORM READ-SHOP-RECEIPT-FILE
           .


       READ-SHOP-RECEIPT-FILE.
           READ SHOP-RECEIPT-FILE
           AT END
              SET END-OF-SHOP-RECEIPTS-FILE TO TRUE 
           END-READ
           .