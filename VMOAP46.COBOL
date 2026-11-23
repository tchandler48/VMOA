USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP46.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 REC1.
           02 UKEY     PIC X(10).
           02 UPWD     PIC X(10).
           02 UTYPE    PIC X.
           02 FILLER   PIC X(59).
    
        01 REC-KEY      PIC X(10).

        01 WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

        01 CL-REC1.
           02 CL-KEY.
              05  CL-YR1     PIC X(2).
              05  CL-MTH     PIC X(2).
           02 CDAY1          PIC 99.
           02 CDAY2          PIC 99.
           02 CDAY3          PIC 99.
           02 CDAY4          PIC 99.
           02 CDAY5          PIC 99.
           02 CDAY6          PIC 99.
           02 CDAY7          PIC 99.
           02 CDAY8          PIC 99.
           02 CDAY9          PIC 99.
           02 CDAY10         PIC 99.
           02 CDAY11         PIC 99.
           02 CDAY12         PIC 99.
           02 CDAY13         PIC 99.
           02 CDAY14         PIC 99.
           02 CDAY15         PIC 99.
           02 CDAY16         PIC 99.
           02 CDAY17         PIC 99.
           02 CDAY18         PIC 99.
           02 CDAY19         PIC 99.
           02 CDAY20         PIC 99.
           02 CDAY21         PIC 99.
           02 CDAY22         PIC 99.
           02 CDAY23         PIC 99.
           02 CDAY24         PIC 99.
           02 CDAY25         PIC 99.
           02 CDAY26         PIC 99.
           02 CDAY27         PIC 99.
           02 CDAY28         PIC 99.
           02 CDAY29         PIC X(2).
           02 CDAY30         PIC X(2).
           02 CDAY31         PIC X(2).
           02 CDAY32         PIC X(2).
           02 CDAY33         PIC X(2).
           02 CDAY34         PIC X(2).
           02 CDAY35         PIC X(2).
           02 CDAY36         PIC X(2).
           02 FILLER         PIC X(19).

        01 WK-DATE1.
           05  WK-MTH   PIC X(02).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.
 
       01  WK-DATE4.
           05  WK-MTH4   PIC XX.
           05  FILLER    PIC X.
           05  WK-DAY4   PIC XX.
           05  FILLER    PIC X.
           05  WK-YR4    PIC XX.

       01  WS-COMMAREA.
           02 CKEY     PIC X(10).
           02 CPWD     PIC X(10).
           02 CSYS     PIC X(08).
           02 CTYPE    PIC X.
           02 CNAME    PIC X(25).
           02 C-CALDT.
              07  C-CALMT   PIC 99.
              07  FILLER    PIC X.
              07  C-CALDY   PIC 99.
              07  FILLER    PIC X.
              07  C-CALYR   PIC 99.
           02 CSCDY    PIC X.
           02 FILLER   PIC X(17).

        01 REC10.
           05  R-KEY.
               07  USER          PIC X(10).
               07  CDATE.
                   09  WK-CDYR   PIC X(2).
                   09  FILLER    PIC X.
                   09  WK-CDMT   PIC X(02).
                   09  FILLER    PIC X.
                   09  WK-CDDY   PIC X(02).
               07  ST-TIME.
                   09  ST-HR  PIC 99.
                   09  ST-MN  PIC 99.
           05  SP-TIME.
               07  SP-HR  PIC 99.
               07  SP-MN  PIC 99.
           05  STYPE      PIC X.
           05  UCOMMENT   PIC X(49).
           05  FILLER     PIC X(07).

       01  W-LGTH         PIC 99.
       01  WK-CT          PIC 99.
       01  W1             PIC 99 VALUE ZERO.
       01  W2             PIC 99 VALUE ZERO.
       01  W3             PIC 99 VALUE ZERO.
       01  W4             PIC 99 VALUE ZERO.
       01  W5             PIC 99 VALUE ZERO.
       01  W6             PIC 99 VALUE ZERO.
       01  W7             PIC 99 VALUE ZERO.
       01  W8             PIC 99 VALUE ZERO.
       01  W9             PIC 99 VALUE ZERO.
       01  W10            PIC 99 VALUE ZERO.
       01  W11            PIC 99 VALUE ZERO.
       01  W12            PIC 99 VALUE ZERO.
       01  W13            PIC 99 VALUE ZERO.
       01  W14            PIC 99 VALUE ZERO.
       01  W15            PIC 99 VALUE ZERO.
       01  W16            PIC 99 VALUE ZERO.
       01  W17            PIC 99 VALUE ZERO.
       01  W18            PIC 99 VALUE ZERO.
       01  W19            PIC 99 VALUE ZERO.
       01  W20            PIC 99 VALUE ZERO.
       01  W21            PIC 99 VALUE ZERO.
       01  W22            PIC 99 VALUE ZERO.
       01  W23            PIC 99 VALUE ZERO.
       01  W24            PIC 99 VALUE ZERO.
       01  W25            PIC 99 VALUE ZERO.
       01  W26            PIC 99 VALUE ZERO.
       01  W27            PIC 99 VALUE ZERO.
       01  W28            PIC 99 VALUE ZERO.
       01  W29            PIC 99 VALUE ZERO.
       01  W30            PIC 99 VALUE ZERO.
       01  W31            PIC 99 VALUE ZERO.
       01  W32            PIC 99 VALUE ZERO.
       01  W33            PIC 99 VALUE ZERO.
       01  W34            PIC 99 VALUE ZERO.
       01  W35            PIC 99 VALUE ZERO.
       01  W36            PIC 99 VALUE ZERO.
       01  W37            PIC 99 VALUE ZERO.
       01  W38            PIC 99 VALUE ZERO.
       01  W39            PIC 99 VALUE ZERO.
       01  W40            PIC 99 VALUE ZERO.
       01  W41            PIC 99 VALUE ZERO.
       01  W42            PIC 99 VALUE ZERO.

       01  CW1            PIC 99 VALUE ZERO.
       01  CW2            PIC 99 VALUE ZERO.
       01  CW3            PIC 99 VALUE ZERO.
       01  CW4            PIC 99 VALUE ZERO.
       01  CW5            PIC 99 VALUE ZERO.
       01  CW6            PIC 99 VALUE ZERO.
       01  CW7            PIC 99 VALUE ZERO.
       01  CW8            PIC 99 VALUE ZERO.
       01  CW9            PIC 99 VALUE ZERO.
       01  CW10           PIC 99 VALUE ZERO.
       01  CW11           PIC 99 VALUE ZERO.
       01  CW12           PIC 99 VALUE ZERO.
       01  CW13           PIC 99 VALUE ZERO.
       01  CW14           PIC 99 VALUE ZERO.
       01  CW15           PIC 99 VALUE ZERO.
       01  CW16           PIC 99 VALUE ZERO.
       01  CW17           PIC 99 VALUE ZERO.
       01  CW18           PIC 99 VALUE ZERO.
       01  CW19           PIC 99 VALUE ZERO.
       01  CW20           PIC 99 VALUE ZERO.
       01  CW21           PIC 99 VALUE ZERO.
       01  CW22           PIC 99 VALUE ZERO.
       01  CW23           PIC 99 VALUE ZERO.
       01  CW24           PIC 99 VALUE ZERO.
       01  CW25           PIC 99 VALUE ZERO.
       01  CW26           PIC 99 VALUE ZERO.
       01  CW27           PIC 99 VALUE ZERO.
       01  CW28           PIC 99 VALUE ZERO.
       01  CW29           PIC 99 VALUE ZERO.
       01  CW30           PIC 99 VALUE ZERO.
       01  CW31           PIC 99 VALUE ZERO.
       01  CW32           PIC 99 VALUE ZERO.
       01  CW33           PIC 99 VALUE ZERO.
       01  CW34           PIC 99 VALUE ZERO.
       01  CW35           PIC 99 VALUE ZERO.
       01  CW36           PIC 99 VALUE ZERO.
       01  CW37           PIC 99 VALUE ZERO.
       01  CW38           PIC 99 VALUE ZERO.
       01  CW39           PIC 99 VALUE ZERO.
       01  CW40           PIC 99 VALUE ZERO.
       01  CW41           PIC 99 VALUE ZERO.
       01  CW42           PIC 99 VALUE ZERO.

       COPY VMOAM46.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMAREA.

       START-OVER.
           MOVE LOW-VALUES      TO MAPAO.
           MOVE CURRENT-DATE    TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
           MOVE TIME-OF-DAY     TO CTIMEO.
           ADD  1               TO C-CALMT.
           MOVE C-CALDT         TO WK-DATE4.

<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>
           MOVE LOW-VALUES       TO PCOMMO.
  
          
           IF WK-MTH4 EQUAL   01
                MOVE 'JANUARY'   TO SMTHO.
           IF WK-MTH4 EQUAL   02
                MOVE 'FEBURARY'  TO SMTHO.
           IF WK-MTH4  EQUAL  03
                MOVE 'MARCH'     TO SMTHO.
           IF WK-MTH4  EQUAL  04
                MOVE 'APRIL'     TO SMTHO.
           IF WK-MTH4  EQUAL  05
                MOVE 'MAY'       TO SMTHO.
           IF WK-MTH4  EQUAL  06
                MOVE 'JUNE'      TO SMTHO.
           IF WK-MTH4 EQUAL   07
                MOVE 'JULY'      TO SMTHO.
           IF WK-MTH4 EQUAL   08
                MOVE 'AUGUST'    TO SMTHO.
           IF WK-MTH4  EQUAL  09
                MOVE 'SEPTEMBER' TO SMTHO.
           IF WK-MTH4  EQUAL  10
                MOVE 'OCTOBER'   TO SMTHO.
           IF WK-MTH4  EQUAL  11
                MOVE 'NOVEMBER'  TO SMTHO.
           IF WK-MTH4  EQUAL  12
                MOVE 'DECEMBER'  TO SMTHO.

           MOVE WK-MTH4  TO CL-MTH.
           MOVE WK-YR4   TO CL-YR1.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP83) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(CL-REC1)           
             RIDFLD(CL-KEY)      
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
           MOVE CDAY19 TO SDAY19O.
           MOVE CDAY20 TO SDAY20O.
           MOVE CDAY21 TO SDAY21O.
           MOVE CDAY22 TO SDAY22O.
           MOVE CDAY23 TO SDAY23O.
           MOVE CDAY24 TO SDAY24O.
           MOVE CDAY25 TO SDAY25O.
           MOVE CDAY26 TO SDAY26O.
           MOVE CDAY27 TO SDAY27O.
           MOVE CDAY28 TO SDAY28O.
           MOVE CDAY29 TO SDAY29O.
           MOVE CDAY30 TO SDAY30O.
           MOVE CDAY31 TO SDAY31O.
           MOVE CDAY32 TO SDAY32O.
           MOVE CDAY33 TO SDAY33O.
           MOVE CDAY34 TO SDAY34O.
           MOVE CDAY35 TO SDAY35O.
           MOVE CDAY36 TO SDAY36O.
           
          
       LOOP83.
           MOVE ZEROS      TO W1.
           MOVE ZEROS      TO W2.
           MOVE ZEROS      TO W3.
           MOVE ZEROS      TO W4.
           MOVE ZEROS      TO W5.
           MOVE ZEROS      TO W6.
           MOVE ZEROS      TO W7.
           MOVE ZEROS      TO W8.
           MOVE ZEROS      TO W9.
           MOVE ZEROS      TO W10.
           MOVE ZEROS      TO W11.
           MOVE ZEROS      TO W12.
           MOVE ZEROS      TO W13.
           MOVE ZEROS      TO W14.
           MOVE ZEROS      TO W15.
           MOVE ZEROS      TO W16.
           MOVE ZEROS      TO W17.
           MOVE ZEROS      TO W18.
           MOVE ZEROS      TO W19.
           MOVE ZEROS      TO W20.
           MOVE ZEROS      TO W21.
           MOVE ZEROS      TO W22.
           MOVE ZEROS      TO W23.
           MOVE ZEROS      TO W24.
           MOVE ZEROS      TO W25.
           MOVE ZEROS      TO W26.
           MOVE ZEROS      TO W27.
           MOVE ZEROS      TO W28.
           MOVE ZEROS      TO W29.
           MOVE ZEROS      TO W30.
           MOVE ZEROS      TO W31.
           MOVE ZEROS      TO W32.
           MOVE ZEROS      TO W33.
           MOVE ZEROS      TO W34.
           MOVE ZEROS      TO W35.
           MOVE ZEROS      TO W36.
           MOVE ZEROS      TO W37.
           MOVE ZEROS      TO W38.
           MOVE ZEROS      TO W39.
           MOVE ZEROS      TO W40.
           MOVE ZEROS      TO W41.
           MOVE ZEROS      TO W42.

 
           MOVE +80        TO W-LGTH.
           MOVE 00         TO WK-CT.
           MOVE LOW-VALUES TO R-KEY.

           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS STARTBR FILE('VMOAURC')
               RIDFLD(R-KEY) KEYLENGTH(22)
           END-EXEC.

       LOOP56.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAURC')
               INTO(REC10) 
               RIDFLD(R-KEY) KEYLENGTH(22)
               END-EXEC.

           IF CKEY NOT EQUAL USER THEN
               GO TO LOOP56.
           
           IF WK-MTH4 NOT EQUAL WK-CDMT THEN
               GO TO LOOP56.

           IF WK-YR4  NOT EQUAL WK-CDYR THEN
               GO TO LOOP56.

       LOOPX.
           IF WK-CDDY EQUAL '01' THEN
              ADD 1     TO  W1
              GO TO LOOP56.
           IF WK-CDDY EQUAL '02' THEN
              ADD 1     TO  W2
              GO TO LOOP56.
           IF WK-CDDY EQUAL '03' THEN
              ADD 1     TO  W3
              GO TO LOOP56.
           IF WK-CDDY EQUAL '04' THEN
              ADD 1     TO  W4
              GO TO LOOP56.
           IF WK-CDDY EQUAL '05' THEN
              ADD 1     TO  W5
              GO TO LOOP56.
           IF WK-CDDY EQUAL '06' THEN
              ADD 1     TO  W6
              GO TO LOOP56.
           IF WK-CDDY EQUAL '07' THEN
              ADD 1     TO  W7
              GO TO LOOP56.
           IF WK-CDDY EQUAL '08' THEN
              ADD 1     TO  W8
              GO TO LOOP56.
           IF WK-CDDY EQUAL '09' THEN
              ADD 1     TO  W9
              GO TO LOOP56.
           IF WK-CDDY EQUAL '10' THEN
              ADD 1     TO  W10
              GO TO LOOP56.
           IF WK-CDDY EQUAL '11' THEN
              ADD 1     TO  W11
              GO TO LOOP56.
           IF WK-CDDY EQUAL '12' THEN
              ADD 1     TO  W12
              GO TO LOOP56.
           IF WK-CDDY EQUAL '13' THEN
              ADD 1     TO  W13
              GO TO LOOP56.
           IF WK-CDDY EQUAL '14' THEN
              ADD 1     TO  W14
              GO TO LOOP56.
           IF WK-CDDY EQUAL '15' THEN
              ADD 1     TO  W15
              GO TO LOOP56.
           IF WK-CDDY EQUAL '16' THEN
              ADD 1     TO  W16
              GO TO LOOP56.
           IF WK-CDDY EQUAL '17' THEN
              ADD 1     TO  W17
              GO TO LOOP56.
           IF WK-CDDY EQUAL '18' THEN
              ADD 1     TO  W18
              GO TO LOOP56.
           IF WK-CDDY EQUAL '19' THEN
              ADD 1     TO  W19
              GO TO LOOP56.
           IF WK-CDDY EQUAL '20' THEN
              ADD 1     TO  W20
              GO TO LOOP56.
           IF WK-CDDY EQUAL '21' THEN
              ADD 1     TO  W21
              GO TO LOOP56.
           IF WK-CDDY EQUAL '22' THEN
              ADD 1     TO  W22
              GO TO LOOP56.
           IF WK-CDDY EQUAL '23' THEN
              ADD 1     TO  W23
              GO TO LOOP56.
           IF WK-CDDY EQUAL '24' THEN
              ADD 1     TO  W24
              GO TO LOOP56.
           IF WK-CDDY EQUAL '25' THEN
              ADD 1     TO  W25
              GO TO LOOP56.
           IF WK-CDDY EQUAL '26' THEN
              ADD 1     TO  W26
              GO TO LOOP56.
           IF WK-CDDY EQUAL '27' THEN
              ADD 1     TO  W27
              GO TO LOOP56.
           IF WK-CDDY EQUAL '28' THEN
              ADD 1     TO  W28
              GO TO LOOP56.
           IF WK-CDDY EQUAL '29' THEN
              ADD 1     TO  W29
              GO TO LOOP56.
           IF WK-CDDY EQUAL '30' THEN
              ADD 1     TO  W30
              GO TO LOOP56.
           IF WK-CDDY EQUAL '31' THEN
              IF STYPE EQUAL 'C' THEN
                 ADD 1     TO  CW31
              ELSE
                 ADD 1     TO  W31
              GO TO LOOP56.
           IF WK-CDDY EQUAL '32' THEN
              ADD 1     TO  W32
              GO TO LOOP56.
           IF WK-CDDY EQUAL '33' THEN
              ADD 1     TO  W33
              GO TO LOOP56.
           IF WK-CDDY EQUAL '34' THEN
              ADD 1     TO  W34
              GO TO LOOP56.
           IF WK-CDDY EQUAL '35' THEN
              ADD 1     TO  W35
              GO TO LOOP56.
           IF WK-CDDY EQUAL '36' THEN
              ADD 1     TO  W36
              GO TO LOOP56.
           IF WK-CDDY EQUAL '37' THEN
              ADD 1     TO  W37
              GO TO LOOP56.
           IF WK-CDDY EQUAL '38' THEN
              ADD 1     TO  W38
              GO TO LOOP56.
           IF WK-CDDY EQUAL '39' THEN
              ADD 1     TO  W39
              GO TO LOOP56.
           IF WK-CDDY EQUAL '40' THEN
              ADD 1     TO  W40
              GO TO LOOP56.
           IF WK-CDDY EQUAL '41' THEN
              ADD 1     TO  W41
              GO TO LOOP56.
           IF WK-CDDY EQUAL '42' THEN
              ADD 1     TO  W42
              GO TO LOOP56.
           GO TO LOOP56.

       LOOP55.
           EXEC KICKS ENDBR FILE('VMOAURC')
               END-EXEC.

           IF CDAY1 NOT EQUAL 1 THEN
              GO TO LOOP10.

              MOVE W1   TO SW1O.
              MOVE W2   TO SW2O.
              MOVE W3   TO SW3O.
              MOVE W4   TO SW4O.
              MOVE W5   TO SW5O.
              MOVE W6   TO SW6O.
              MOVE W7   TO SW7O.
              MOVE W8   TO SW8O.
              MOVE W9   TO SW9O.
              MOVE W10  TO SW10O.
              MOVE W11  TO SW11O.
              MOVE W12  TO SW12O.
              MOVE W13  TO SW13O.
              MOVE W14  TO SW14O.
              MOVE W15  TO SW15O.
              MOVE W16  TO SW16O.
              MOVE W17  TO SW17O.
              MOVE W18  TO SW18O.
              MOVE W19  TO SW19O.
              MOVE W20  TO SW20O.
              MOVE W21  TO SW21O.
              MOVE W22  TO SW22O.
              MOVE W23  TO SW23O.
              MOVE W24  TO SW24O.
              MOVE W25  TO SW25O.
              MOVE W26  TO SW26O.
              MOVE W27  TO SW27O.
              MOVE W28  TO SW28O.
              MOVE W29  TO SW29O.
              MOVE W30  TO SW30O.
              MOVE W31  TO SW31O.
              MOVE CW31 TO CSW31O.
              MOVE W32  TO SW32O.
              MOVE W33  TO SW33O.
              MOVE W34  TO SW34O.
              MOVE W35  TO SW35O.
              MOVE W36  TO SW36O.
              MOVE W37  TO SW37O.
              MOVE W38  TO SW38O.
              MOVE W39  TO SW39O.
              MOVE W40  TO SW40O.
              MOVE W41  TO SW41O.
              MOVE W42  TO SW42O.
              GO TO LOOP73.

       LOOP10.
           IF CDAY4 NOT EQUAL 1 THEN
              GO TO LOOP11.
              MOVE W1   TO SW4O.
              MOVE W2   TO SW5O.
              MOVE W3   TO SW6O.
              MOVE W4   TO SW7O.
              MOVE W5   TO SW8O.
              MOVE W6   TO SW9O.
              MOVE W7   TO SW10O.
              MOVE W8   TO SW11O.
              MOVE W9   TO SW12O.
              MOVE W10  TO SW13O.
              MOVE W11  TO SW14O.
              MOVE W12  TO SW15O.
              MOVE W13  TO SW16O.
              MOVE W14  TO SW17O.
              MOVE W15  TO SW18O.
              MOVE W16  TO SW19O.
              MOVE W17  TO SW20O.
              MOVE W18  TO SW21O.
              MOVE W19  TO SW22O.
              MOVE W20  TO SW23O.
              MOVE W21  TO SW24O.
              MOVE W22  TO SW25O.
              MOVE W23  TO SW26O.
              MOVE W24  TO SW27O.
              MOVE W25  TO SW28O.
              MOVE W26  TO SW29O.
              MOVE W27  TO SW30O.
              MOVE W28  TO SW31O.
              MOVE W29  TO SW32O.
              MOVE W30  TO SW33O.
              MOVE W31  TO SW34O.
              MOVE CW31 TO CSW34O.
              MOVE W32  TO SW35O.
              MOVE W33  TO SW36O.
              MOVE W34  TO SW37O.
              MOVE W35  TO SW38O.
              MOVE W36  TO SW39O.
              MOVE W37  TO SW40O.
              MOVE W38  TO SW41O.
              MOVE W39  TO SW42O.
              GO TO LOOP73.


       LOOP11.

       LOOP73.
           
           MOVE CNAME          TO CALOWNO.
    
           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM46') ERASE
           END-EXEC.

           EXEC KICKS HANDLE AID PF1(VDAY)
                                 PF10(HELPSCR)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPA') MAPSET('VMOAM46')
           END-EXEC.
           GO TO START-OVER.

       VDAY.
           GO TO START-OVER.

       HELPSCR.
             EXEC KICKS LINK PROGRAM('VMOAP22')
                END-EXEC.      
             GO TO START-OVER.
       
       ENDPGM.
          EXEC KICKS RETURN END-EXEC. 

