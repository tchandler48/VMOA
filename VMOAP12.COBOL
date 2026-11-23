USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP12.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  WS-COMMON.
           05  WK-USER  PIC X(10).
           05  FILLER   PIC X(19).
           05  WK-OWN   PIC X(15).
           05  WK-CALDT.
               07  WK-CALMT  PIC X(2).
               07  FILLER    PIC X.
               07  WK-CALDY  PIC X(2).
               07  FILLER    PIC X.
               07  WK-CALYR  PIC X(2).
           05  COM-DAY       PIC X.
           05  FILLER   PIC X(48).

           01  SCR-DT.
               07  SK-CALMT  PIC X(2).
               07  FILLER    PIC X.
               07  SK-CALDY  PIC X(2).
               07  FILLER    PIC X.
               07  SK-CALYR  PIC X(2).


       01  REC1.
           05  R-KEY.
               07  USER          PIC X(10).
               07  CDATE.
                   09  WK-CDYR   PIC X(2).
                   09  FILLER    PIC X.
                   09  WK-CDMT   PIC X(2).
                   09  FILLER    PIC X.
                   09  WK-CDDY   PIC X(2).
               07  ST-TIME.
                   09  ST-HR  PIC 99.
                   09  ST-MN  PIC 99.
           05  SP-TIME.
               07  SP-HR  PIC 99.
               07  SP-MN  PIC 99.
           05  UCOMMENT   PIC X(49).
           05  FILLER     PIC X(08).

       01  TDATE.
           05  TDMTH    PIC 99.
           05  FILLER   PIC X VALUE '/'.
           05  TDDAY    PIC 99.
           05  FILLER   PIC X VALUE '/'.
           05  TDYR     PIC 99.

       01  W-LGTH       PIC 99.
       01  WK-CT        PIC 99.
       01  WK-SCDAYS    PIC 9.

       01  SV-KEY1      PIC X(22).
       01  SV-KEY2      PIC X(22).

       01  WK-DATE1.
           05  WK-MTH   PIC 9(2).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.


       COPY VMOAM12.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.

           MOVE DFHCOMMAREA     TO WS-COMMON.
           MOVE COM-DAY         TO HDAYSO.
           MOVE COM-DAY         TO NEXTDAYO.
           MOVE COM-DAY         TO NEXTD1O.
                    
           MOVE CURRENT-DATE    TO WS-CURRENT-DATE.
           MOVE WK-OWN          TO OWNERAO.
         
           MOVE WK-CALDT        TO CALDTAO.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
           MOVE COM-DAY         TO WK-SCDAYS.
          
       START-OVER.
           MOVE +80             TO W-LGTH.
           MOVE 00              TO WK-CT.
           MOVE LOW-VALUES      TO R-KEY.

           EXEC KICKS STARTBR FILE('VMOAURC')
               RIDFLD(R-KEY) KEYLENGTH(22)
           END-EXEC.

       LOOP56.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAURC')
               INTO(REC1) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               END-EXEC.

           IF WK-USER NOT EQUAL USER
               GO TO LOOP56.
           IF WK-CDDY NOT EQUAL WK-CALDY
               GO TO LOOP56.
           IF WK-CDMT NOT EQUAL WK-CALMT
               GO TO LOOP56.
           IF WK-CDYR NOT EQUAL WK-CALYR
               GO TO LOOP56. 
 
        LOOP57.
           ADD 1 TO WK-CT.
           IF WK-CT EQUAL     01 THEN
               MOVE WK-CDMT  TO TDMTH
               MOVE WK-CDDY  TO TDDAY
               MOVE WK-CDYR  TO TDYR
               MOVE TDATE    TO RSEL01O
               MOVE ST-HR    TO RSEL02O
               MOVE ST-MN    TO RSEL03O
               MOVE SP-HR    TO RSEL04O
               MOVE SP-MN    TO RSEL05O
               MOVE UCOMMENT TO RSEL06O
               GO TO LOOP56.

            IF WK-CT EQUAL    02 THEN
               MOVE WK-CDMT  TO TDMTH
               MOVE WK-CDDY  TO TDDAY
               MOVE WK-CDYR  TO TDYR
               MOVE TDATE    TO RSEL11O
               MOVE ST-HR    TO RSEL12O
               MOVE ST-MN    TO RSEL13O
               MOVE SP-HR    TO RSEL14O
               MOVE SP-MN    TO RSEL15O
               MOVE UCOMMENT TO RSEL16O
               GO TO LOOP56.

            IF WK-CT EQUAL    03 THEN
               MOVE ST-HR    TO RSEL22O
               MOVE ST-MN    TO RSEL23O
               MOVE SP-HR    TO RSEL24O
               MOVE SP-MN    TO RSEL25O
               MOVE UCOMMENT TO RSEL26O
               GO TO LOOP56.

            IF WK-CT EQUAL    04 THEN
               MOVE ST-HR    TO RSEL32O
               MOVE ST-MN    TO RSEL33O
               MOVE SP-HR    TO RSEL34O
               MOVE SP-MN    TO RSEL35O
               MOVE UCOMMENT TO RSEL36O
               GO TO LOOP56.

            IF WK-CT EQUAL    05 THEN
               MOVE ST-HR    TO RSEL42O
               MOVE ST-MN    TO RSEL43O
               MOVE SP-HR    TO RSEL44O
               MOVE SP-MN    TO RSEL45O
               MOVE UCOMMENT TO RSEL46O
               GO TO LOOP56.

            IF WK-CT EQUAL    06 THEN
               MOVE ST-HR    TO RSEL52O
               MOVE ST-MN    TO RSEL53O
               MOVE SP-HR    TO RSEL54O
               MOVE SP-MN    TO RSEL55O
               MOVE UCOMMENT TO RSEL56O
               GO TO LOOP56.

            IF WK-CT EQUAL    07 THEN
               MOVE ST-HR    TO RSEL62O
               MOVE ST-MN    TO RSEL63O
               MOVE SP-HR    TO RSEL64O
               MOVE SP-MN    TO RSEL65O
               MOVE UCOMMENT TO RSEL66O
               GO TO LOOP56.

            IF WK-CT EQUAL    08 THEN
               MOVE ST-HR    TO RSEL72O
               MOVE ST-MN    TO RSEL73O
               MOVE SP-HR    TO RSEL74O
               MOVE SP-MN    TO RSEL75O
               MOVE UCOMMENT TO RSEL76O
               GO TO LOOP56.

            IF WK-CT EQUAL    09 THEN
               MOVE ST-HR    TO RSEL82O
               MOVE ST-MN    TO RSEL83O
               MOVE SP-HR    TO RSEL84O
               MOVE SP-MN    TO RSEL85O
               MOVE UCOMMENT TO RSEL86O
               GO TO LOOP56.

           IF WK-CT EQUAL    10 THEN
               MOVE ST-HR    TO RSEL92O
               MOVE ST-MN    TO RSEL93O
               MOVE SP-HR    TO RSEL94O
               MOVE SP-MN    TO RSEL95O
               MOVE UCOMMENT TO RSEL96O
               GO TO LOOP56.

          
       LOOP55.
           EXEC KICKS ENDBR 
               FILE('VMOAURC')
           END-EXEC.

           EXEC KICKS SEND MAP('MAPA') MAPSET('VMOAM11') ERASE
             END-EXEC.
          
           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM11')
             END-EXEC.

           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.

