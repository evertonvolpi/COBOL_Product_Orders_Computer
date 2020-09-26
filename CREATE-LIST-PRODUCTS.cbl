       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-LIST-PRODUCTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT NEWPRODFILE ASSIGN TO "productsList.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.

           FD NEWPRODFILE.
           01 NEWRECORD       PIC X(41).

           WORKING-STORAGE SECTION.
           01  WS-PRODUCT-INFO.
               05 PRODUCT-ID       PIC X(3).
               05 PRODUCT-NAME     PIC X(20).
               05 PRODUCT-PRICE    PIC 999.99.
               05 PRODUCT-CATEGORY PIC X(10).

           05 CONCAT-STRING    PIC X(41).

           05 HEAD-LINE        PIC X(41) VALUE
               "ID  NAME                 PRICE  CATEGORY ".

           05 IS-DONE          PIC X.
               88  ENTER-NEW    VALUE "y".

       PROCEDURE DIVISION.
       0100-START.

           OPEN OUTPUT NEWPRODFILE.

           WRITE NEWRECORD FROM HEAD-LINE.

           PERFORM 0200-PROCESS-NEW-LINE THRU 0200-END

           DISPLAY "Thank you and goodbye.".
           PERFORM 9000-END-PROGRAM.

       0100-END.

       0200-PROCESS-NEW-LINE.
           DISPLAY "Product ID >>> ".
           ACCEPT PRODUCT-ID.
           DISPLAY "Product NAME >>> ".
           ACCEPT PRODUCT-NAME.
           DISPLAY "Product PRICE >>> ".
           ACCEPT PRODUCT-PRICE.
           DISPLAY "Product CATEGORY >>> ".
           ACCEPT PRODUCT-CATEGORY.

           STRING  PRODUCT-ID (1:3)
                   ' '   DELIMITED BY SIZE
                   PRODUCT-NAME (1:20)
                   ' '   DELIMITED BY SIZE
                   PRODUCT-PRICE (1:6)
                   ' '   DELIMITED BY SIZE
                   PRODUCT-CATEGORY(1:10)
           INTO CONCAT-STRING
           END-STRING.

           WRITE NEWRECORD FROM CONCAT-STRING.

           DISPLAY "Do you want to enter another product? (y) >>> ".
           ACCEPT IS-DONE.

           IF ENTER-NEW THEN
               PERFORM 0200-PROCESS-NEW-LINE
           END-IF.

       0200-END.

       9000-END-PROGRAM.
           CLOSE NEWPRODFILE.
           STOP RUN.
           END PROGRAM CREATE-LIST-PRODUCTS.
