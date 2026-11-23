USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP98.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  WK-DATE1.
           05  WK-MTH   PIC 9(2).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       01  REC-KEY.
           02 KYR1      PIC X(2).
           02 KMTH      PIC X(2).

       01  SV-KEY      PIC X(10).
       01  SV-PWD      PIC X(10).

        01 REC1.
           02 CL-KEY.
              05  CL-YR1     PIC X(2).
              05  CL-MTH     PIC X(2).
           02 CDAY1          PIC X(2).
           02 CDAY2          PIC X(2).
           02 CDAY3          PIC X(2).
           02 CDAY4          PIC X(2).
           02 CDAY5          PIC X(2).
           02 CDAY6          PIC X(2).
           02 CDAY7          PIC X(2).
           02 CDAY8          PIC X(2).
           02 CDAY9          PIC X(2).
           02 CDAY10         PIC X(2).
           02 CDAY11         PIC X(2).
           02 CDAY12         PIC X(2).
           02 CDAY13         PIC X(2).
           02 CDAY14         PIC X(2).
           02 CDAY15         PIC X(2).
           02 CDAY16         PIC X(2).
           02 CDAY17         PIC X(2).
           02 CDAY18         PIC X(2).
           02 CDAY19         PIC X(2).
           02 CDAY20         PIC X(2).
           02 CDAY21         PIC X(2).
           02 CDAY22         PIC X(2).
           02 CDAY23         PIC X(2).
           02 CDAY24         PIC X(2).
           02 CDAY25         PIC X(2).
           02 CDAY26         PIC X(2).
           02 CDAY27         PIC X(2).
           02 CDAY28         PIC X(2).
           02 CDAY29         PIC X(2).
           02 CDAY30         PIC X(2).
           02 CDAY31         PIC X(2).
           02 CDAY32         PIC X(2).
           02 CDAY33         PIC X(2).
           02 CDAY34         PIC X(2).
           02 CDAY35         PIC X(2).
           02 FILLER         PIC X(21).

       COPY VMOAM98.
       COPY DFHBMSCA.

       PROCEDURE DIVISION.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE 'CALENDAR' TO DESCAO.

           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM98') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM98')
             END-EXEC.


           MOVE '26' TO KYR1.
           MOVE WK-MTH TO KMTH.
 
           EXEC KICKS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(REC-KEY)      
           END-EXEC.
  
           MOVE CDAY1 TO SDAY1O.
           MOVE CDAY2 TO SDAY2O.
           MOVE CDAY3 TO SDAY3O.
           MOVE CDAY4 TO SDAY4O.
           MOVE CDAY5 TO SDAY5O.
           MOVE CDAY6 TO SDAY6O.
           MOVE CDAY7 TO SDAY7O. 
           MOVE CDAY8 TO SDAY8O.
           MOVE CDAY9 TO SDAY9O.
           MOVE CDAY10 TO SDAY10O.
           MOVE CDAY11 TO SDAY11O.
           MOVE CDAY12 TO SDAY12O.
           MOVE CDAY13 TO SDAY13O.
           MOVE CDAY14 TO SDAY14O. 
           MOVE CDAY15 TO SDAY15O.
           MOVE CDAY16 TO SDAY16O.
           MOVE CDAY17 TO SDAY17O.
           MOVE CDAY18 TO SDAY18O.
           IF WK-DAY EQUAL '18'
                MOVE KIKGREEN TO SDAY18C.
           MOVE CDAY19 TO SDAY19O. 
           IF WK-DAY EQUAL '19'
                MOVE KIKGREEN TO SDAY19C.
           MOVE CDAY20 TO SDAY20O.
           IF WK-DAY EQUAL '20'
                MOVE KIKGREEN TO SDAY20C.
           MOVE CDAY21 TO SDAY21O.
           IF WK-DAY EQUAL '21'
                MOVE KIKGREEN TO SDAY21C.
           MOVE CDAY22 TO SDAY22O.
           IF WK-DAY EQUAL '22'
                MOVE KIKGREEN TO SDAY22C.
           MOVE CDAY23 TO SDAY23O.
           IF WK-DAY EQUAL '23'
                MOVE KIKGREEN TO SDAY23C.
           MOVE CDAY24 TO SDAY24O. 
           IF WK-DAY EQUAL '24'
                MOVE KIKGREEN TO SDAY24C.
           MOVE CDAY25 TO SDAY25O.
           IF WK-DAY EQUAL CDAY25
                MOVE KIKGREEN TO SDAY25C.
           MOVE CDAY26 TO SDAY26O.
           IF WK-DAY EQUAL '26'
                MOVE KIKGREEN TO SDAY26C.
           MOVE CDAY27 TO SDAY27O.
           IF WK-DAY EQUAL '27'
                MOVE KIKGREEN TO SDAY27C.
           MOVE CDAY28 TO SDAY28O.
           IF WK-DAY EQUAL '28'
                MOVE KIKGREEN TO SDAY28C.
           MOVE CDAY29 TO SDAY29O. 
           IF WK-DAY EQUAL '29'
                MOVE KIKGREEN TO SDAY29C.
           MOVE CDAY30 TO SDAY30O.
           IF WK-DAY EQUAL '30'
                MOVE KIKGREEN TO SDAY30C.
           MOVE CDAY31 TO SDAY31O.
           IF WK-DAY EQUAL '31'
                MOVE KIKGREEN TO SDAY31C.
           MOVE CDAY32 TO SDAY32O.
           IF WK-DAY EQUAL '32'
                MOVE KIKGREEN TO SDAY32C.
           MOVE CDAY33 TO SDAY33O.
           IF WK-DAY EQUAL '33'
                MOVE KIKGREEN TO SDAY33C.
           MOVE CDAY34 TO SDAY34O. 
           IF WK-DAY EQUAL '34'
                MOVE KIKGREEN TO SDAY34C.
           MOVE CDAY35 TO SDAY35O.
           IF WK-DAY EQUAL '35'
                MOVE KIKGREEN TO SDAY35C.

           MOVE TIME-OF-DAY TO CT2O.
           MOVE WS-CURRENT-DATE TO CD2O. 

           IF WK-MTH EQUAL 01
                MOVE 'JANUARY' TO MTHDO.
           IF WK-MTH EQUAL 02
                MOVE 'FEBURARY' TO MTHDO.
           IF WK-MTH EQUAL 03
                MOVE 'MARCH' TO MTHDO.
           
        
           MOVE WS-CURRENT-YEAR TO SV-PWD.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
 
       EXEC KICKS
             SEND MAP('MAPB') MAPSET('VMOAM98') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPB') MAPSET('VMOAM98')
             END-EXEC.


       ENDPGM.
           EXEC KICKS RETURN END-EXEC.
