       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SHOP-RECEIPTS-FOOTER.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT SHOP-RECEIPT-FILE ASSIGN TO "shop_receipts_footer.dat"
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
          88 IS-SHOP-FOOTER                            VALUE "F".
       05 SHOP-ID                       PIC X(5).
          05 SHOP-LOCATION              PIC X(30).
       01 SALE-RECEIPT.
          05 RECORD-TYPE-CODE           PIC X.
          05 ITEM-ID                    PIC X(8).
          05 QTY-SOLD                   PIC 9(3).
          05 ITEM-COST                  PIC 999V99.
       01 SHOP-FOOTER.
          05 RECORD-TYPE-CODE           PIC X.
          05 REC-COUNT                  PIC 9(5).


       WORKING-STORAGE SECTION. 
       01 PRN-SHOP-SALES-TOTAL.
          05 FILLER                     PIC X(21)      VALUE
                "TOTAL SALES FOR SHOP".
          05 PRN-SHOP-ID                PIC X(5).
          05 PRN-SHOP-TOTAL             PIC $$$$,$$9.99.
       01 SHOP-TOTAL                    PIC 9(5)V99.
       01 ITEM-COUNT                    PIC 9(5).

       01 PRN-ERROR-MESSAGE.
          05 FILLER                     PIC X(15)      VALUE
                "Error on shop: ".
          05 PRN-ERR-SHOP-ID            PIC X(5).        
          05 FILLER                     PIC X(10)      VALUE "RCount = "
           .  
          05 PRN-RECORD-COUNT           PIC 9(5).
          05 FILLER                     PIC X(10)      VALUE "ACount = "
           .  
          05 PRN-ACTUAL-COUNT           PIC 9(5).
          
       PROCEDURE DIVISION .
       SHOP-SALE-PROCESS.
           OPEN INPUT SHOP-RECEIPT-FILE 
           PERFORM READ-SHOP-RECEIPT-FILE.
           PERFORM PROCESS-SHOPS UNTIL END-OF-SHOP-RECEIPTS-FILE 

           CLOSE SHOP-RECEIPT-FILE 
           GOBACK 
           .
       
       PROCESS-SHOPS.
           PERFORM PROCESS-SHOP-HEADER.
           PERFORM PROCESS-SHOP-ITEM UNTIL END-OF-SHOP-RECEIPTS-FILE
              OR IS-SHOP-FOOTER

           PERFORM PROCESS-SHOP-FOOTER
           .
       PROCESS-SHOP-HEADER.
           MOVE SHOP-ID TO PRN-SHOP-ID
           MOVE ZEROS TO SHOP-TOTAL.
           MOVE ZEROS TO ITEM-COUNT.
           PERFORM READ-SHOP-RECEIPT-FILE
           .
       
       PROCESS-SHOP-ITEM.
      *    DISPLAY "ITEM " ITEM-ID 
           COMPUTE SHOP-TOTAL = SHOP-TOTAL +(QTY-SOLD * ITEM-COST)
           COMPUTE ITEM-COUNT = ITEM-COUNT + 1
           PERFORM READ-SHOP-RECEIPT-FILE
           .
       PROCESS-SHOP-FOOTER.
           IF ITEM-COUNT = REC-COUNT  
              MOVE SHOP-TOTAL TO PRN-SHOP-TOTAL 
              DISPLAY PRN-SHOP-SALES-TOTAL 
           ELSE
              MOVE REC-COUNT TO PRN-RECORD-COUNT
              MOVE ITEM-COUNT TO PRN-ACTUAL-COUNT
              MOVE PRN-SHOP-ID TO PRN-ERR-SHOP-ID
              DISPLAY PRN-ERROR-MESSAGE 
           END-IF
           PERFORM READ-SHOP-RECEIPT-FILE.


       READ-SHOP-RECEIPT-FILE.
           READ SHOP-RECEIPT-FILE
           AT END
              SET END-OF-SHOP-RECEIPTS-FILE TO TRUE 
           END-READ
           .