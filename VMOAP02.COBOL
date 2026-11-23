USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP02.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  REC1.
           02 CL-KEY.
              05  CL-YR1     PIC 99.
              05  CL-MTH     PIC 99.
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
           02 CDAY29         PIC 99.
           02 CDAY30         PIC 99.
           02 CDAY31         PIC 99.
           02 CDAY32         PIC 99.
           02 CDAY33         PIC 99.
           02 CDAY34         PIC 99.
           02 CDAY35         PIC 99.
           02 CDAY36         PIC 99.
           02 CDAY37         PIC 99.
           02 CDAY38         PIC 99.
           02 CDAY39         PIC 99.
           02 CDAY40         PIC 99.
           02 CDAY41         PIC 99.
           02 CDAY42         PIC 99.
           02 FILLER         PIC X(02).
    
       01  SCR-YR1.
           02 SCR-1M    PIC X(2).
           02 SCR-1Y    PIC X(2).

       01  SCR-YR2.
           02 SCR-2M    PIC X(2).
           02 SCR-2Y    PIC X(2).

       01  WK-DATE1.
           05  WK-MTH   PIC 99.
           05  WK-FIL1  PIC X.
           05  WK-DAY   PIC 99.
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC XX.

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
      
       01  MAIL-COMM.
           02  FILLER  PIC X(16).
           02  FILLER  PIC X(4) VALUE 'MAIL'.
           02  FILLER  PIC X(12).

       01  DAY-YR      PIC 999.

       01  REC2.
           05  R-KEY2.
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
           05  VTYPE      PIC X.
           05  UCOMMENT   PIC X(49).
           05  FILLER     PIC X(07).

 
       01  REC-3.
           05  R3-KEY101.
               07  R3-USERID         PIC X(10).
               07  R3-MTYPE          PIC X.
               07  R3-TOID           PIC X(10).
               07  R3-TDATE.
                   09 R3-TYR         PIC XX.
                   09 R3-TMTH        PIC XX.
                   09 R3-TDAY        PIC XX.
               07  TTIME.
                   09 R3-THH         PIC XX.
                   09 R3-TMM         PIC XX.
                   09 R3-TSS         PIC XX.
       	   05  R3-TSTATUS            PIC X.
           05  R3-TSUB               PIC X(25).
           05  FILLER                PIC X(21).


       01  W-LGTH       PIC 99.
       01  WK-CT        PIC 99.
       01  CAL-CT       PIC 99.
       01  MSG-CT       PIC 99.

       01  MSG-LN1.
           05  FILLER   PIC X(08) VALUE 'YOU HAVE'.
           05  SCAL-CT  PIC ZZ9.
           05  FILLER   PIC X(22) VALUE ' CALENDAR EVENTS TODAY'.

       01  MSG-LN3.
           05  FILLER   PIC X(08) VALUE 'YOU HAVE'.
           05  SMSG-CT  PIC ZZ9.
           05  FILLER   PIC X(16) VALUE ' UNREAD MESSAGES'.

          
       COPY VMOAM02.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMAREA.
 
 
       START-OVER.
           MOVE LOW-VALUES TO MAPAO.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
         
           MOVE +20    TO SCR-1M.
           MOVE WK-YR  TO SCR-1Y.
           MOVE SCR-YR1 TO SYR1O.
           MOVE SCR-YR1 TO SYR2O.

           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE TIME-OF-DAY TO CTIME2O.
           
           IF WK-MTH EQUAL      01
                MOVE 'JANUARY' TO SMTHO.
           IF WK-MTH EQUAL      02
                MOVE 'FEBURARY' TO SMTHO.
           IF WK-MTH  EQUAL     03
                MOVE 'MARCH' TO SMTHO.
           IF WK-MTH  EQUAL     04
                MOVE 'APRIL' TO SMTHO.
           IF WK-MTH  EQUAL     05
                MOVE 'MAY' TO SMTHO.
           IF WK-MTH  EQUAL     06
                MOVE 'JUNE' TO SMTHO.
           IF WK-MTH EQUAL      07
                MOVE 'JULY' TO SMTHO.
           IF WK-MTH EQUAL      08
                MOVE 'AUGUST' TO SMTHO.
           IF WK-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTHO.
           IF WK-MTH  EQUAL 10
                MOVE 'OCTOBER'  TO SMTHO.
           IF WK-MTH  EQUAL 11
                MOVE 'NOVEMBER' TO SMTHO.
           IF WK-MTH  EQUAL 12
                MOVE 'DECEMBER' TO SMTHO.

           MOVE +00             TO DAY-YR.
           IF WK-MTH > 01
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 02
                ADD  +28        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 03     
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 04    
                ADD  +30        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 05    
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 06    
                ADD  +30        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 07    
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 08    
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 09    
                ADD  +30        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 10    
                ADD  +31        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
           IF WK-MTH > 11    
                ADD  +30        TO DAY-YR
           ELSE
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.
        
                ADD WK-DAY      TO DAY-YR
                MOVE DAY-YR     TO SCDAYRO
                GO TO END-DAYR.

       END-DAYR.

<NCB2>
           EXAMINE CTIME2O REPLACING ALL ' ' BY ':'.
</NCB2>
           
<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>

           MOVE WK-MTH TO CL-MTH.
           MOVE WK-YR  TO CL-YR1.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP83) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(CL-KEY)      
           END-EXEC.
           
           MOVE CDAY1 TO SDAY1O.
           IF WK-DAY EQUAL CDAY1
                MOVE KIKGREEN TO SDAY1C.
           MOVE CDAY2 TO SDAY2O.
           IF WK-DAY EQUAL CDAY2 
                MOVE KIKGREEN TO SDAY2C.
           MOVE CDAY3 TO SDAY3O.
           IF WK-DAY EQUAL CDAY3
                MOVE KIKGREEN TO SDAY3C.
           MOVE CDAY4 TO SDAY4O.
           IF WK-DAY EQUAL CDAY4
                MOVE KIKGREEN TO SDAY4C.
           MOVE CDAY5 TO SDAY5O.
           IF WK-DAY EQUAL CDAY5
                MOVE KIKGREEN TO SDAY5C.
           MOVE CDAY6 TO SDAY6O.
           IF WK-DAY EQUAL CDAY6
                MOVE KIKGREEN TO SDAY6C.
           MOVE CDAY7 TO SDAY7O. 
           IF WK-DAY EQUAL CDAY7
                MOVE KIKGREEN TO SDAY7C.

           MOVE CDAY8 TO SDAY8O.
           IF WK-DAY EQUAL CDAY8
                MOVE KIKGREEN TO SDAY8C.
           MOVE CDAY9 TO SDAY9O.
           IF WK-DAY EQUAL CDAY9
                MOVE KIKGREEN TO SDAY9C.
           MOVE CDAY10 TO SDAY10O.
           IF WK-DAY EQUAL CDAY10
                MOVE KIKGREEN TO SDAY10C.
           MOVE CDAY11 TO SDAY11O.
           IF WK-DAY EQUAL CDAY11
                MOVE KIKGREEN TO SDAY11C.
           MOVE CDAY12 TO SDAY12O.
           IF WK-DAY EQUAL CDAY12
                MOVE KIKGREEN TO SDAY12C.
           MOVE CDAY13 TO SDAY13O.
           IF WK-DAY EQUAL CDAY13
                MOVE KIKGREEN TO SDAY13C.
           MOVE CDAY14 TO SDAY14O. 
           IF WK-DAY EQUAL CDAY14
                MOVE KIKGREEN TO SDAY14C.
           MOVE CDAY15 TO SDAY15O.
           IF WK-DAY EQUAL CDAY15
                MOVE KIKGREEN TO SDAY15C.

           MOVE CDAY16 TO SDAY16O.
           IF WK-DAY EQUAL CDAY16
                MOVE KIKGREEN TO SDAY16C.
           MOVE CDAY17 TO SDAY17O.
           IF WK-DAY EQUAL CDAY17
                MOVE KIKGREEN TO SDAY17C.
           MOVE CDAY18 TO SDAY18O.
           IF WK-DAY EQUAL CDAY18
                MOVE KIKGREEN TO SDAY18C.
           MOVE CDAY19 TO SDAY19O. 
           IF WK-DAY EQUAL CDAY19
                MOVE KIKGREEN TO SDAY19C.
           IF WK-DAY EQUAL CDAY19
                MOVE KIKGREEN TO SDAY19C.
           MOVE CDAY20 TO SDAY20O.
           IF WK-DAY EQUAL CDAY20
                MOVE KIKGREEN TO SDAY20C.
           MOVE CDAY21 TO SDAY21O.
           IF WK-DAY EQUAL CDAY21
                MOVE KIKGREEN TO SDAY21C.
           MOVE CDAY22 TO SDAY22O.
           IF WK-DAY EQUAL CDAY22
                MOVE KIKGREEN TO SDAY22C.
           MOVE CDAY23 TO SDAY23O.
           IF WK-DAY EQUAL CDAY23
                MOVE KIKGREEN TO SDAY23C.
           MOVE CDAY24 TO SDAY24O. 
           IF WK-DAY EQUAL CDAY24
                MOVE KIKGREEN TO SDAY24C.

           MOVE CDAY25 TO SDAY25O.
           IF WK-DAY EQUAL CDAY25
                MOVE KIKGREEN TO SDAY25C.
           MOVE CDAY26 TO SDAY26O.
           IF WK-DAY EQUAL CDAY26
                MOVE KIKGREEN TO SDAY26C.
           MOVE CDAY27 TO SDAY27O.
           IF WK-DAY EQUAL CDAY27
                MOVE KIKGREEN TO SDAY27C.
           MOVE CDAY28 TO SDAY28O.
           IF WK-DAY EQUAL CDAY28
                MOVE KIKGREEN TO SDAY28C.
           MOVE CDAY29 TO SDAY29O.
           IF WK-DAY EQUAL CDAY29
                MOVE KIKGREEN TO SDAY29C.
           MOVE CDAY30 TO SDAY30O.
           IF WK-DAY EQUAL CDAY30
                MOVE KIKGREEN TO SDAY30C.
           MOVE CDAY31 TO SDAY31O.
           IF WK-DAY EQUAL CDAY31
                MOVE KIKGREEN TO SDAY31C.
           MOVE CDAY32 TO SDAY32O.
           IF WK-DAY EQUAL CDAY32
                MOVE KIKGREEN TO SDAY32C.
           MOVE CDAY33 TO SDAY33O.
           IF WK-DAY EQUAL CDAY33
                MOVE KIKGREEN TO SDAY33C.
           MOVE CDAY34 TO SDAY34O. 
           IF WK-DAY EQUAL CDAY34
                MOVE KIKGREEN TO SDAY34C.

           GO TO LOOP83.

           MOVE CDAY35 TO SDAY35O.
           IF WK-DAY EQUAL CDAY35
                MOVE KIKGREEN TO SDAY35C.
           MOVE CDAY36 TO SDAY36O.
           IF WK-DAY EQUAL CDAY36
                MOVE KIKGREEN TO SDAY36C.
           MOVE CDAY37 TO SDAY37O.
           IF WK-DAY EQUAL CDAY37
                MOVE KIKGREEN TO SDAY37C.
           MOVE CDAY38 TO SDAY38O.
           IF WK-DAY EQUAL CDAY38
                MOVE KIKGREEN TO SDAY38C.
           MOVE CDAY39 TO SDAY39O.
           IF WK-DAY EQUAL CDAY39
                MOVE KIKGREEN TO SDAY39C.
           MOVE CDAY40 TO SDAY40O.
           IF WK-DAY EQUAL CDAY40
                MOVE KIKGREEN TO SDAY40C.
           MOVE CDAY41 TO SDAY41O. 
           IF WK-DAY EQUAL CDAY41
                MOVE KIKGREEN TO SDAY41C.
           MOVE CDAY42 TO SDAY42O.
           IF WK-DAY EQUAL CDAY42
                MOVE KIKGREEN TO SDAY42C.


       LOOP83.

           MOVE +80        TO W-LGTH.
           MOVE 00         TO WK-CT.
           MOVE LOW-VALUES TO R-KEY2.
           MOVE ZEROS      TO CAL-CT.

           EXEC KICKS STARTBR FILE('VMOAURC')
               RIDFLD(R-KEY2) KEYLENGTH(22)
           END-EXEC.

       LOOP656.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP655) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAURC')
               INTO(REC2) 
               RIDFLD(R-KEY2) KEYLENGTH(22)
               END-EXEC.

           IF CKEY NOT EQUAL USER
               GO TO LOOP656.
          
           IF WK-CDYR NOT EQUAL WK-YR
               GO TO LOOP656.            

           IF WK-CDDY NOT EQUAL WK-DAY
               GO TO LOOP656.

           IF WK-CDMT NOT EQUAL WK-MTH
               GO TO LOOP656.

           ADD 1 TO CAL-CT.
           GO TO LOOP656.

       LOOP655.
           EXEC KICKS ENDBR 
               FILE('VMOAURC')
           END-EXEC.

           IF CAL-CT GREATER THAN 0 THEN
               MOVE CAL-CT    TO SCAL-CT
               MOVE MSG-LN1   TO SLN1O.

           MOVE ZEROS      TO MSG-CT.
           MOVE LOW-VALUES TO R3-KEY101.
           EXEC KICKS STARTBR FILE('VMOAMML')
               RIDFLD(R3-KEY101) KEYLENGTH(33)
           END-EXEC.

       LOOP777.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP778) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAMML')
               INTO(REC-3) 
               RIDFLD(R3-KEY101)
               END-EXEC.

           IF R3-USERID EQUAL CKEY THEN
               GO TO LOOP771.
           IF R3-TOID   EQUAL CKEY THEN
               GO TO LOOP771.
           GO TO LOOP777.
       
       LOOP771.
           IF R3-TSTATUS EQUAL 'U' THEN
                ADD 1 TO MSG-CT.
           GO TO LOOP777.

       LOOP778.

           EXEC KICKS ENDBR 
               FILE('VMOAMML')
           END-EXEC.

           IF MSG-CT GREATER THAN 0 THEN
               MOVE MSG-CT    TO SMSG-CT
               MOVE MSG-LN3   TO SLN2O.



       LOOP1.
          

           EXEC KICKS
              SEND MAP('MAPA') MAPSET('VMOAM02') ERASE
              END-EXEC.

           EXEC KICKS HANDLE AID PF1(CLANDER)
                                 PF2(CONFCAL)
                                 PF3(DISTLS)
                                 PF4(PHONE)
                                 PF5(NMSG)
                                 PF8(CALCPGM)
                                 PF10(HELPSCR)
                                 PF12(ENDPGM) 
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPA') MAPSET('VMOAM02')
              END-EXEC.

           IF SEL2I EQUAL '1' THEN
              GO TO CLANDER.
           IF SEL2I EQUAL '3' THEN
              GO TO PHONE.
           IF SEL2I EQUAL '7' THEN
              GO TO CALCPGM.
           IF SEL2I EQUAL '11' THEN
              GO TO HELPSCR.
           IF SEL2I EQUAL '12' THEN
              GO TO ENDPGM.
           GO TO ENDPGM.


       CONFCAL.
             EXEC KICKS LINK PROGRAM('VMOAP13')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       CLANDER.
             EXEC KICKS LINK PROGRAM('VMOAP03')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       CALCPGM.
             EXEC KICKS LINK PROGRAM('KKPGM') END-EXEC.      
             GO TO START-OVER.

       PHONE.
             EXEC KICKS LINK PROGRAM('VMOAP09')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       NMSG.
             EXEC KICKS LINK PROGRAM('VMOAP36')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       DISTLS.
             EXEC KICKS LINK PROGRAM('VMOAP30')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       HELPSCR.
             EXEC KICKS LINK PROGRAM('VMOAP17')
             END-EXEC.      
             GO TO START-OVER.

       ENDPGM.
           EXEC KICKS XCTL PROGRAM('KSSFPGM') NOHANDLE END-EXEC. 
     
