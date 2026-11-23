USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP13.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

        01 REC1.
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
           02 CDAY21         PIC XX.
           02 CDAY22         PIC XX.
           02 CDAY23         PIC XX.
           02 CDAY24         PIC XX.
           02 CDAY25         PIC XX.
           02 CDAY26         PIC XX.
           02 CDAY27         PIC XX.
           02 CDAY28         PIC XX.
           02 CDAY29         PIC XX.
           02 CDAY30         PIC XX.
           02 CDAY31         PIC XX.
           02 CDAY32         PIC XX.
           02 CDAY33         PIC XX.
           02 CDAY34         PIC XX.
           02 CDAY35         PIC XX.
           02 FILLER         PIC X(21).
    
        01 REC-KEY.
           02 KYR1      PIC X(2).
           02 KMTH      PIC X(2).

       01  SCR-YR1.
           02 SCR-1M    PIC X(2).
           02 SCR-1Y    PIC X(2).

       01  SCR-YR2.
           02 SCR-2M    PIC X(2).
           02 SCR-2Y    PIC X(2).

       01  WK-DATE1.
           05  WK-MTH   PIC 9(2).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  FILLER   PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

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


       01  DAY-YR       PIC 999.


       COPY VMOAM13.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMAREA.

       START-OVER.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
           MOVE WS-CURRENT-DATE TO CALDTO.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
           MOVE WS-CURRENT-DATE TO C-CALDT.

           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE TIME-OF-DAY TO CTIME3O.

<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>

<NCB2>
           EXAMINE CTIME3O REPLACING ALL ' ' BY ':'.
</NCB2>


           MOVE WK-MTH TO CL-MTH.
           MOVE WK-YR  TO CL-YR1.
           MOVE 20     TO SCR-1M.
           MOVE 20     TO KMTH.
           MOVE WK-YR  TO SCR-1Y.
           MOVE WK-YR  TO KYR1.
           MOVE SCR-YR1 TO SYR1O.
           MOVE SCR-YR1 TO SYR2O.
    
           IF WK-MTH EQUAL 01
                MOVE 'JANUARY'   TO SMTHO.
           IF WK-MTH EQUAL 02
                MOVE 'FEBURARY'  TO SMTHO.
           IF WK-MTH  EQUAL 03
                MOVE 'MARCH'     TO SMTHO.
           IF WK-MTH  EQUAL 04
                MOVE 'APRIL'     TO SMTHO.
           IF WK-MTH  EQUAL 05
                MOVE 'MAY'       TO SMTHO.
           IF WK-MTH  EQUAL 06
                MOVE 'JUNE'      TO SMTHO.
           IF WK-MTH EQUAL 07
                MOVE 'JULY'      TO SMTHO.
           IF WK-MTH EQUAL 08
                MOVE 'AUGUST'    TO SMTHO.
           IF WK-MTH  EQUAL 09
                MOVE 'SEPTEMBER' TO SMTHO.
           IF WK-MTH  EQUAL 10
                MOVE 'OCTOBER'   TO SMTHO.
           IF WK-MTH  EQUAL 11
                MOVE 'NOVEMBER'  TO SMTHO.
           IF WK-MTH  EQUAL 12
                MOVE 'DECEMBER'  TO SMTHO.

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
           MOVE CDAY35 TO SDAY35O.
           IF WK-DAY EQUAL CDAY35
                MOVE KIKGREEN TO SDAY35C.

       LOOP83.
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

           MOVE CNAME        TO OWNERO.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM13') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF1(DSPCONF)
                                 PF2(CONFUP)
                                 PF10(HELPSCR)
                                 PF12(ENDPGM) 
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM13')
             END-EXEC.

 
           IF SEL3I EQUAL 1
             GO TO DSPCONF.
           IF SEL3I EQUAL 2
             GO TO CONFUP.
           IF SEL3I EQUAL 10
             GO TO HELPSCR.
           IF SEL3I EQUAL 12
             GO TO ENDPGM.
           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.
           GO TO END-EOJ.

       DSPCONF.
           MOVE CALDTI   TO C-CALDT.
           EXEC KICKS LINK PROGRAM('VMOAP14')
             COMMAREA(WS-COMMAREA)
             LENGTH(80)
           END-EXEC. 
           GO TO START-OVER.

       CONFUP.
           MOVE CALDTI   TO C-CALDT.
           EXEC KICKS LINK PROGRAM('VMOAP07')
             COMMAREA(WS-COMMAREA)
             LENGTH(80)
           END-EXEC. 
           GO TO START-OVER.

       HELPSCR.
           EXEC KICKS LINK PROGRAM('VMOAP19')
           END-EXEC. 
           GO TO START-OVER.


       END-EOJ.

          

     
