USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP07.
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
           05  FILLER   PIC X(49).

           01  SCR-DT.
               07  SK-CALMT  PIC X(2).
               07  FILLER    PIC X.
               07  SK-CALDY  PIC X(2).
               07  FILLER    PIC X.
               07  SK-CALYR  PIC X(2).

       01  SV-WK-USER        PIC X(10).

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
           05  CTYPE      PIC X.
           05  UCOMMENT   PIC X(49).
           05  FILLER     PIC X(07).

       01  W-LGTH       PIC 99.
       01  WK-CT        PIC 99.

       01  SV-KEY1      PIC X(22).
       01  SV-KEY2      PIC X(22).
       01  SV-KEY3      PIC X(22).

       01  WK-DATE1.
           05  WK-MTH   PIC 9(2).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       COPY VMOAM07.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.

           MOVE DFHCOMMAREA  TO WS-COMMON.
           MOVE WK-USER      TO SV-WK-USER.

       START-OVER.  
           MOVE LOW-VALUES   TO MAPAO.        
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE 'CONF-A'     TO OWNER4O.
           MOVE WK-CALDT     TO WDATE4O.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
            
           MOVE +80        TO W-LGTH.
           MOVE 00         TO WK-CT.
           MOVE LOW-VALUES TO R-KEY.

           MOVE 'CONF-A'        TO USER.
           MOVE WS-CURRENT-DATE TO CDATE4O.
           MOVE TIME-OF-DAY     TO CTIME4O.

           EXEC KICKS STARTBR FILE('VMOACRS')
               RIDFLD(R-KEY) KEYLENGTH(22)
           END-EXEC.

       LOOP56.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOACRS')
               INTO(REC1) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               END-EXEC.

           IF  USER NOT EQUAL 'CONF-A'
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
               MOVE R-KEY    TO SV-KEY1
               MOVE ST-HR    TO RSEL02O
               MOVE ST-MN    TO RSEL03O
               MOVE SP-HR    TO RSEL04O
               MOVE SP-MN    TO RSEL05O
               MOVE UCOMMENT TO RSEL06O
               GO TO LOOP56.

            IF WK-CT EQUAL    02 THEN
               MOVE R-KEY    TO SV-KEY2
               MOVE ST-HR    TO RSEL12O
               MOVE ST-MN    TO RSEL13O
               MOVE SP-HR    TO RSEL14O
               MOVE SP-MN    TO RSEL15O
               MOVE UCOMMENT TO RSEL16O
               GO TO LOOP56.

            IF WK-CT EQUAL    03 THEN
               MOVE R-KEY    TO SV-KEY3
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
               FILE('VMOACRS')
           END-EXEC.

           EXEC KICKS SEND MAP('MAPA') MAPSET('VMOAM07') ERASE
             END-EXEC.
          
           EXEC KICKS HANDLE AID PF10(HELPSC)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM07')
             END-EXEC.

           IF RSEL01I NOT EQUAL 'A'
              GO TO LOOP3.

           MOVE 'CONF-A'     TO  USER.
           MOVE WK-CALYR     TO  WK-CDYR.
           MOVE WK-CALMT     TO  WK-CDMT.
           MOVE WK-CALDY     TO  WK-CDDY.
           MOVE RSEL02I      TO  ST-HR.
           MOVE RSEL03I      TO  ST-MN.
           MOVE 'C'          TO  CTYPE.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP2) END-EXEC.
           EXEC CICS READ
             FILE('VMOACRS')      
             INTO(REC1)           
             RIDFLD(R-KEY)      
             END-EXEC.
          
           MOVE 'LOOP1 RCD NOT FOUND'  TO PCOMMO.
           GO TO START-OVER.

       LOOP2.
           MOVE RSEL04I  TO  SP-HR.
           MOVE RSEL05I  TO  SP-MN.
           MOVE RSEL06I  TO  UCOMMENT.

           EXEC KICKS WRITE FILE('VMOACRS')
                      FROM(REC1)
                      RIDFLD(R-KEY)
                      END-EXEC.
           GO TO LOOP10.

       LOOP3.
           IF RSEL01I NOT EQUAL 'C'
              GO TO LOOP6.
          
           MOVE SV-KEY1  TO  R-KEY.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP3A) END-EXEC.
           EXEC KICKS READ FILE('VMOACRS')
               INTO(REC1) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               UPDATE
               END-EXEC.

           MOVE RSEL04I  TO  SP-HR.
           MOVE RSEL05I  TO  SP-MN.
           MOVE RSEL06I  TO  UCOMMENT.

           EXEC KICKS REWRITE FILE('VMOACRS')
                      FROM(REC1)
                      END-EXEC.

           MOVE 'INSIDE LOO3 AFTER REWRITE'  TO PCOMMO.
           GO TO LOOP10.
       LOOP3A.
           MOVE 'LOOP3A RCD NOT FOUND' TO PCOMMO.
           GO TO START-OVER.

       LOOP6.
           IF RSEL01I NOT EQUAL 'D'
              GO TO LOOP7.

       LOOP7.
           IF RSEL01I NOT EQUAL 'S'
              GO TO LOOP10.
           MOVE SV-WK-USER   TO  USER.
           MOVE WK-CALYR     TO  WK-CDYR.
           MOVE WK-CALMT     TO  WK-CDMT.
           MOVE WK-CALDY     TO  WK-CDDY.
           MOVE RSEL02I      TO  ST-HR.
           MOVE RSEL03I      TO  ST-MN.
           MOVE 'C'          TO  CTYPE.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP2A) END-EXEC.
           EXEC CICS READ
             FILE('VMOAURC')      
             INTO(REC1)           
             RIDFLD(R-KEY)      
             END-EXEC.
          
           MOVE 'LOOP1 RCD NOT FOUND'  TO PCOMMO.
           GO TO START-OVER.

       LOOP2A.
           MOVE RSEL04I  TO  SP-HR.
           MOVE RSEL05I  TO  SP-MN.
           MOVE RSEL06I  TO  UCOMMENT.
           MOVE 'C'      TO  CTYPE.

           EXEC KICKS WRITE FILE('VMOAURC')
                      FROM(REC1)
                      RIDFLD(R-KEY)
                      END-EXEC.
           GO TO START-OVER.

       LOOP9.
           GO TO START-OVER.


       LOOP10.

           IF RSEL11I NOT EQUAL 'A'
              GO TO LOOP13.

           MOVE 'CONF-A'     TO  USER.
           MOVE WK-CALYR     TO  WK-CDYR.
           MOVE WK-CALMT     TO  WK-CDMT.
           MOVE WK-CALDY     TO  WK-CDDY.
           MOVE RSEL12I      TO  ST-HR.
           MOVE RSEL13I      TO  ST-MN.
           MOVE 'C'          TO  CTYPE.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP12) END-EXEC.
           EXEC CICS READ
             FILE('VMOACRS')      
             INTO(REC1)           
             RIDFLD(R-KEY)      
             END-EXEC.
         
             MOVE 'LOOP10 RCD NOT FOUND'  TO PCOMMO.
             GO TO START-OVER.

       LOOP12.
           MOVE RSEL14I  TO  SP-HR.
           MOVE RSEL15I  TO  SP-MN.
           MOVE RSEL16I  TO  UCOMMENT.

           EXEC KICKS WRITE FILE('VMOACRS')
                      FROM(REC1)
                      RIDFLD(R-KEY)
                      END-EXEC.
           GO TO LOOP20.

       LOOP13.
           IF RSEL11I NOT EQUAL 'C'
              GO TO LOOP16.

          
           MOVE SV-KEY2  TO  R-KEY.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP13B) END-EXEC.
           EXEC KICKS READ FILE('VMOACRS')
               INTO(REC1) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               UPDATE
               END-EXEC.

           MOVE RSEL14I  TO  SP-HR.
           MOVE RSEL15I  TO  SP-MN.
           MOVE RSEL16I  TO  UCOMMENT.

           EXEC KICKS REWRITE FILE('VMOACRS')
                      FROM(REC1)
                      END-EXEC.

           GO TO LOOP20.
       LOOP13B.
           MOVE 'LOOP13B RCD NOT FOUND' TO PCOMMO.
           GO TO START-OVER.

       LOOP16.
           IF RSEL11I NOT EQUAL 'D'
              GO TO LOOP20.

       LOOP19.
           GO TO START-OVER.


       LOOP20.	
           IF RSEL21I NOT EQUAL 'A'
              GO TO LOOP20C.

           MOVE 'CONF-A'     TO  USER.
           MOVE WK-CALYR     TO  WK-CDYR.
           MOVE WK-CALMT     TO  WK-CDMT.
           MOVE WK-CALDY     TO  WK-CDDY.
           MOVE RSEL22I      TO  ST-HR.
           MOVE RSEL23I      TO  ST-MN.
           MOVE 'C'          TO  CTYPE.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP22) END-EXEC.
           EXEC CICS READ
             FILE('VMOACRS')      
             INTO(REC1)           
             RIDFLD(R-KEY)      
             END-EXEC.
         
             MOVE 'LOOP10 RCD NOT FOUND'  TO PCOMMO.
             GO TO START-OVER.

       LOOP22.
           MOVE RSEL24I  TO  SP-HR.
           MOVE RSEL25I  TO  SP-MN.
           MOVE RSEL26I  TO  UCOMMENT.

           EXEC KICKS WRITE FILE('VMOACRS')
                      FROM(REC1)
                      RIDFLD(R-KEY)
                      END-EXEC.
           GO TO LOOP30.

       LOOP20C.
           IF RSEL21I NOT EQUAL 'C'
              GO TO LOOP20D.

          
           MOVE SV-KEY3  TO  R-KEY.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP23B) END-EXEC.
           EXEC KICKS READ FILE('VMOACRS')
               INTO(REC1) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               UPDATE
               END-EXEC.

           MOVE RSEL24I  TO  SP-HR.
           MOVE RSEL25I  TO  SP-MN.
           MOVE RSEL26I  TO  UCOMMENT.

           EXEC KICKS REWRITE FILE('VMOACRS')
                      FROM(REC1)
                      END-EXEC.

           GO TO LOOP30.
       LOOP23B.
           MOVE 'LOOP23B RCD NOT FOUND' TO PCOMMO.
           GO TO START-OVER.

       LOOP20D.
           IF RSEL21I NOT EQUAL 'D'
              GO TO LOOP30.

       LOOP29.
           GO TO START-OVER.


       LOOP30.	



           GO TO START-OVER.

       HELPSC.
           EXEC KICKS LINK PROGRAM('VMOAP27')
             END-EXEC.      
           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.
         
