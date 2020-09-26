       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT PRODUCT-LIST ASSIGN TO "productsList.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ORDER-LIST ASSIGN TO FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD PRODUCT-LIST.
           01 DETAILS.
               05  PRODUCT-ID       PIC X(4).
               05  PRODUCT-NAME     PIC X(21).
               05  PRODUCT-PRICE    PIC 999V99.
               05  PRODUCT-CATEGORY PIC X(10).

           FD ORDER-LIST.
           01 NEWRECORD       PIC X(42).

           WORKING-STORAGE SECTION.
           01  WS-DETAILS.
               05  WS-PRODUCT-ID       PIC X(4).
               05  WS-PRODUCT-NAME     PIC X(21).
               05  WS-PRODUCT-PRICE    PIC 999V99.
               05  WS-PRODUCT-CATEGORY PIC X(10).
           01  WS-EOF PIC A(1).

           01  WS-INPUTS.
               05  WS-ID-INPUT         PIC 9(3).
               05  WS-QTY-INPUT         PIC 999V99.

           01  WS-NEW-PRODUCT.
               05  WS-NEW-PRO-ID       PIC X(3).
               05  WS-NEW-PRO-PRICE    PIC 9(6).
               05  WS-NEW-PRO-NAME     PIC X(20).
               05  WS-NEW-PRO-QTY      PIC 999V99.
               05  WS-NEW-PRO-TOTAL    PIC 999V99.

           01  WS-NEW-ORDER.
               05  WS-ORDER-ID         PIC 999 VALUE 001.
               05  WS-ORDER-CLIENT     PIC X(20).
               05  WS-ORDER-TOTAL      PIC 999V99.

           01  DISPLAYS.
               05  DISPLAY-PRO-PRICE   PIC $$$9.99.
               05  DISPLAY-PRO-TOTAL   PIC $$$9.99.
               05  DISPLAY-ORD-TOTAL   PIC $$$9.99.
               05  DISPLAY-PRO-QTY     PIC ZZ9.


           05 HEAD-LINE        PIC X(10) VALUE
               "ID  CLIENT".

           05 DIVISOR          PIC X(42) VALUE
               "******************************************".

           05 CART-LINE        PIC X(41) VALUE
               "PRODUCT              PRICE  QTY SUB-TOTAL".

           05 FOOT-LINE        PIC X(42).

           05 CONCAT-ORDER     PIC X(38).

           05 CONCAT-CART-ROW  PIC X(42).

           05 FILENAME         PIC X(13).

           05 IS-DONE-PRODUCT          PIC X.
               88  ENTER-NEW-PRODUCT   VALUE "y".

           05 IS-DONE-ORDER            PIC X.
               88  ENTER-NEW-ORDER     VALUE "y".

       PROCEDURE DIVISION.
       0100-START.
           MOVE 0 TO WS-ORDER-TOTAL.
           DISPLAY 'Enter client name >>>'.
           ACCEPT WS-ORDER-CLIENT.

           STRING  'ORDER: ' DELIMITED BY SIZE
                   WS-ORDER-ID (1:3)
                   'CLIENT: ' DELIMITED BY SIZE
                   WS-ORDER-CLIENT (1:20)
           INTO CONCAT-ORDER
           END-STRING.

           STRING  'order_'
                   WS-ORDER-ID
                   '.txt'
           INTO FILENAME
           END-STRING.

           OPEN OUTPUT ORDER-LIST.

           WRITE NEWRECORD FROM HEAD-LINE.
           WRITE NEWRECORD FROM CONCAT-ORDER.
           WRITE NEWRECORD FROM DIVISOR.
           WRITE NEWRECORD FROM CART-LINE.

           PERFORM 0200-ENTER-PRODUCTS THRU 0200-END.

           DISPLAY "Order: ", WS-ORDER-ID, " Client: ", WS-ORDER-CLIENT.
           DISPLAY "Total: ", DISPLAY-ORD-TOTAL.

           STRING  '******************* ORDER TOTAL'
                   ' '   DELIMITED BY SIZE
                   DISPLAY-ORD-TOTAL (1:6)
           INTO FOOT-LINE
           END-STRING.

           WRITE NEWRECORD FROM FOOT-LINE.
           CLOSE ORDER-LIST.

           DISPLAY "Do you want to enter another ORDER? (y) >>> "
           ACCEPT IS-DONE-ORDER.

           IF ENTER-NEW-ORDER THEN
               COMPUTE WS-ORDER-ID = WS-ORDER-ID + 1
               PERFORM 0100-START
           END-IF.

           PERFORM 0900-FINISH.

       0100-END.

       0200-ENTER-PRODUCTS.
           MOVE "N" TO WS-EOF
           DISPLAY 'Product ID >>> '.
           ACCEPT WS-ID-INPUT.
           MOVE WS-ID-INPUT TO WS-NEW-PRO-ID.
           DISPLAY 'Quantity >>> '.
           ACCEPT WS-QTY-INPUT.
           MOVE WS-QTY-INPUT TO WS-NEW-PRO-QTY.

           OPEN INPUT PRODUCT-LIST
               PERFORM UNTIL WS-EOF="Y"
                   READ PRODUCT-LIST INTO WS-DETAILS
                       AT END MOVE "Y" TO WS-EOF
                       NOT AT END
                           IF WS-PRODUCT-ID = WS-NEW-PRO-ID THEN
                               MOVE WS-PRODUCT-NAME TO WS-NEW-PRO-NAME
                               MOVE WS-PRODUCT-PRICE TO WS-NEW-PRO-PRICE
                           END-IF
                   END-READ
               END-PERFORM
           CLOSE PRODUCT-LIST.

           COMPUTE WS-NEW-PRO-TOTAL = WS-NEW-PRO-PRICE * WS-NEW-PRO-QTY.
           COMPUTE WS-ORDER-TOTAL = WS-ORDER-TOTAL + WS-NEW-PRO-TOTAL.

           MOVE WS-NEW-PRO-PRICE TO DISPLAY-PRO-PRICE.
           MOVE WS-NEW-PRO-TOTAL TO DISPLAY-PRO-TOTAL.
           MOVE WS-ORDER-TOTAL TO DISPLAY-ORD-TOTAL.
           MOVE WS-NEW-PRO-QTY TO DISPLAY-PRO-QTY.

           DISPLAY "Added: ", DISPLAY-PRO-QTY, " ", WS-NEW-PRO-NAME.
           DISPLAY "Subtotal: ", DISPLAY-PRO-TOTAL.
           DISPLAY "Order total: ", DISPLAY-ORD-TOTAL.

           STRING  WS-NEW-PRO-NAME (1:20)
                   ' '   DELIMITED BY SIZE
                   DISPLAY-PRO-PRICE (1:6)
                   ' '   DELIMITED BY SIZE
                   WS-NEW-PRO-QTY (1:3)
                   '  '   DELIMITED BY SIZE
                   DISPLAY-PRO-TOTAL (1:6)
           INTO CONCAT-CART-ROW
           END-STRING.

           WRITE NEWRECORD FROM CONCAT-CART-ROW.

           DISPLAY "Do you want to enter another PRODUCT? (y) >>> ".
           ACCEPT IS-DONE-PRODUCT.

           IF ENTER-NEW-PRODUCT THEN
               PERFORM 0200-ENTER-PRODUCTS
           END-IF.
       0200-END.

       0900-FINISH.
           DISPLAY "Thank you and goodbye.".
           STOP RUN.
           END PROGRAM ORDERS.
