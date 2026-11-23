USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP05.
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
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       01  SV-KEY       PIC X(10).
       01  SV-PWD       PIC X(10).

       01  SV-CAL.
           02  SVC-YR   PIC XX.
           02  SVC-MTH  PIC XX.

        01 REC1.
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

       01  W-LGTH       PIC S9(8) COMP.
       01  K-LGTH       PIC S9(4) COMP.

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

       COPY VMOAM05.
       COPY DFHBMSCA.

       PROCEDURE DIVISION.
 
       RESEND.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE LOW-VALUES TO MAPAO.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE 'CALENDAR' TO DESCAO.
           MOVE WS-CURRENT-DATE TO DATEAO. 

       START-OVER.

           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM05') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELPA)
                                 PF12(ENDPGM) 
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM05')
             END-EXEC.

           MOVE MTHAI  TO SVC-MTH.
           MOVE YRAI   TO SVC-YR.

           IF SELAI EQUAL 'A'
              GO TO ADDU.
           IF SELAI EQUAL 'B'
              GO TO BRWU.
           IF SELAI EQUAL 'C'
              GO TO CHGU.
           IF SELAI EQUAL 'D'
              GO TO DELU.
           GO TO RESEND.


       ADDU.
           MOVE LOW-VALUES TO MAPCO.
           MOVE SVC-YR  TO CL-YR1.
           MOVE SVC-MTH TO CL-MTH.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOPA) END-EXEC.
           EXEC KICKS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(CL-KEY)      
           END-EXEC.
       
           MOVE 'YR/MTH ALREADY IN DATABASE....' TO ERR1O.
           GO TO RESEND.

       LOOPA.
           
           MOVE TIME-OF-DAY TO CT2O.
           MOVE 'CALENDAR-ADD' TO DESC2O.
           MOVE WS-CURRENT-DATE TO CD2O. 

           IF SVC-MTH EQUAL      01
                MOVE 'JANUARY' TO SMTHO.
           IF SVC-MTH EQUAL      02
                MOVE 'FEBURARY' TO SMTHO.
           IF SVC-MTH  EQUAL     03
                MOVE 'MARCH' TO SMTHO.
           IF SVC-MTH  EQUAL     04
                MOVE 'APRIL' TO SMTHO.
           IF SVC-MTH  EQUAL     05
                MOVE 'MAY' TO SMTHO.
           IF SVC-MTH  EQUAL     06
                MOVE 'JUNE' TO SMTHO.
           IF SVC-MTH EQUAL      07
                MOVE 'JULY' TO SMTHO.
           IF SVC-MTH EQUAL      08
                MOVE 'AUGUST' TO SMTHO.
           IF SVC-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTHO.
           IF SVC-MTH  EQUAL 10
                MOVE 'OCTOBER'  TO SMTHO.
           IF SVC-MTH  EQUAL 11
                MOVE 'NOVEMBER' TO SMTHO.
           IF SVC-MTH  EQUAL 12
                MOVE 'DECEMBER' TO SMTHO.
     
           MOVE '2026'      TO SYRA1O.
           MOVE '2026'      TO SYRA2O.

           EXEC KICKS
             SEND MAP('MAPB') MAPSET('VMOAM05') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELPB)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPB') MAPSET('VMOAM05')
             END-EXEC.

           MOVE TDAY1I TO CDAY1.
           MOVE TDAY2I TO CDAY2.
           MOVE TDAY3I TO CDAY3.
           MOVE TDAY4I TO CDAY4.
           MOVE TDAY5I TO CDAY5.
           MOVE TDAY6I TO CDAY6.
           MOVE TDAY7I TO CDAY7.
           MOVE TDAY8I TO CDAY8.
           MOVE TDAY9I TO CDAY9.
           MOVE TDAY10I TO CDAY10.
           MOVE TDAY11I TO CDAY11.
           MOVE TDAY12I TO CDAY12.
           MOVE TDAY13I TO CDAY13.
           MOVE TDAY14I TO CDAY14.
           MOVE TDAY15I TO CDAY15.
           MOVE TDAY16I TO CDAY16.
           MOVE TDAY17I TO CDAY17.
           MOVE TDAY18I TO CDAY18.
           MOVE TDAY19I TO CDAY19.
           MOVE TDAY20I TO CDAY20.
           MOVE TDAY21I TO CDAY21.
           MOVE TDAY22I TO CDAY22.
           MOVE TDAY23I TO CDAY23.
           MOVE TDAY24I TO CDAY24.
           MOVE TDAY25I TO CDAY25.
           MOVE TDAY26I TO CDAY26.
           MOVE TDAY27I TO CDAY27.
           MOVE TDAY28I TO CDAY28.
           MOVE TDAY29I TO CDAY29.
           MOVE TDAY30I TO CDAY30.
           MOVE TDAY31I TO CDAY31.
           MOVE TDAY32I TO CDAY32.
           MOVE TDAY33I TO CDAY33.
           MOVE TDAY34I TO CDAY34.
           MOVE TDAY35I TO CDAY35.
           MOVE TDAY36I TO CDAY36.
           MOVE TDAY37I TO CDAY37.
           MOVE TDAY38I TO CDAY38.
           MOVE TDAY39I TO CDAY39.
           MOVE TDAY40I TO CDAY40.
           MOVE TDAY41I TO CDAY41.
           MOVE TDAY42I TO CDAY42.

           EXEC KICKS WRITE FILE('VMOACAL')
                      FROM(REC1)
                      RIDFLD(CL-KEY)
           END-EXEC.

           GO TO RESEND.

       BRWU.

           GO TO ENDPGM.

       CHGU.
           MOVE LOW-VALUES TO MAPCO.
           MOVE SVC-YR  TO CL-YR1.
           MOVE SVC-MTH TO CL-MTH.

 

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP10) END-EXEC.
           EXEC KICKS READ
             FILE('VMOACAL') INTO(REC1)     
             UPDATE RIDFLD(CL-KEY) KEYLENGTH(04) 
           END-EXEC.
           GO TO LOOP1.

       LOOP10.
           MOVE 'MTH/YR NOT FOUND - RE-ENTER' TO ERR1O.
           GO TO RESEND.
  
       LOOP1.
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
           MOVE CDAY37 TO SDAY37O.
           MOVE CDAY38 TO SDAY38O.
           MOVE CDAY39 TO SDAY39O.
           MOVE CDAY40 TO SDAY40O.
           MOVE CDAY41 TO SDAY41O. 
           MOVE CDAY42 TO SDAY42O.

           MOVE WS-CURRENT-DATE TO CD3O. 
           MOVE 'CALENDAR-CHANGE' TO DESC3O.
           MOVE TIME-OF-DAY TO CT3O.
           MOVE WS-CURRENT-DATE TO CD3O.
 
           IF CL-MTH EQUAL      '01'
                MOVE 'JANUARY'  TO MTHD3O.
           IF CL-MTH EQUAL      '02'
                MOVE 'FEBURARY' TO MTHD3O.
           IF CL-MTH  EQUAL     '03'
                MOVE 'MARCH'    TO MTHD3O.
           IF CL-MTH  EQUAL     '04'
                MOVE 'APRIL'    TO MTHD3O.
           IF CL-MTH  EQUAL     '05'
                MOVE 'MAY'      TO MTHD3O.
           IF CL-MTH  EQUAL     '06'
                MOVE 'JUNE'     TO MTHD3O.
           IF CL-MTH EQUAL      '07'
                MOVE 'JULY'     TO MTHD3O.
           IF CL-MTH EQUAL      '08'
                MOVE 'AUGUST'   TO MTHD3O.
           IF CL-MTH  EQUAL     '09'
                MOVE 'SEPTEMBER' TO MTHD3O.
           IF CL-MTH  EQUAL '10'
                MOVE 'OCTOBER'  TO MTHD3O.
           IF CL-MTH  EQUAL '11'
                MOVE 'NOVEMBER' TO MTHD3O.
           IF CL-MTH  EQUAL '12'
                MOVE 'DECEMBER' TO MTHD3O.

           MOVE '2026'      TO SYRA3O.
           MOVE '2026'      TO SYRA4O.

           EXEC KICKS
             SEND MAP('MAPC') MAPSET('VMOAM05') ERASE
           END-EXEC.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPC') MAPSET('VMOAM05')
             END-EXEC.

           MOVE SVC-YR  TO CL-YR1.
           MOVE SVC-MTH TO CL-MTH.
           MOVE +04   TO W-LGTH.

           EXEC KICKS DELETE 
              FILE('VMOACAL')
              RIDFLD(CL-KEY) KEYLENGTH(W-LGTH)
           END-EXEC.

           MOVE SDAY1I TO CDAY1.
           MOVE SDAY2I TO CDAY2.
           MOVE SDAY3I TO CDAY3.
           MOVE SDAY4I TO CDAY4.
           MOVE SDAY5I TO CDAY5.
           MOVE SDAY6I TO CDAY6.
           MOVE SDAY7I TO CDAY7.
           MOVE SDAY8I TO CDAY8.
           MOVE SDAY9I TO CDAY9.
           MOVE SDAY10I TO CDAY10.
           MOVE SDAY11I TO CDAY11.
           MOVE SDAY12I TO CDAY12.
           MOVE SDAY13I TO CDAY13.
           MOVE SDAY14I TO CDAY14.
           MOVE SDAY15I TO CDAY15.
           MOVE SDAY16I TO CDAY16.
           MOVE SDAY17I TO CDAY17.
           MOVE SDAY18I TO CDAY18.
           MOVE SDAY19I TO CDAY19.
           MOVE SDAY20I TO CDAY20.
           MOVE SDAY21I TO CDAY21.
           MOVE SDAY22I TO CDAY22.
           MOVE SDAY23I TO CDAY23.
           MOVE SDAY24I TO CDAY24.
           MOVE SDAY25I TO CDAY25.
           MOVE SDAY26I TO CDAY26.
           MOVE SDAY27I TO CDAY27.
           MOVE SDAY28I TO CDAY28.
           MOVE SDAY29I TO CDAY29.
           MOVE SDAY30I TO CDAY30.
           MOVE SDAY31I TO CDAY31.
           MOVE SDAY32I TO CDAY32.
           MOVE SDAY33I TO CDAY33.
           MOVE SDAY34I TO CDAY34.
           MOVE SDAY35I TO CDAY35.
           MOVE SDAY36I TO CDAY36.
           MOVE SDAY37I TO CDAY37.
           MOVE SDAY38I TO CDAY38.
           MOVE SDAY39I TO CDAY39.
           MOVE SDAY40I TO CDAY40.
           MOVE SDAY41I TO CDAY41.
           MOVE SDAY42I TO CDAY42.

           MOVE SVC-YR  TO CL-YR1.
           MOVE SVC-MTH TO CL-MTH.


	   MOVE +80     TO W-LGTH.

           EXEC KICKS WRITE FILE('VMOACAL')
                      FROM(REC1)
                      LENGTH(80)
                      RIDFLD(CL-KEY)
                      END-EXEC.

           MOVE LOW-VALUES TO MAPAO.
           GO TO RESEND.


       DELU.

           MOVE SVC-YR  TO CL-YR1.
           MOVE SVC-MTH TO CL-MTH.

           MOVE +04   TO W-LGTH.
           EXEC KICKS DELETE 
              FILE('VMOACAL')
              RIDFLD(CL-KEY) KEYLENGTH(W-LGTH)
           END-EXEC.
 
           GO TO ENDPGM.

       HELPA.
           EXEC KICKS LINK PROGRAM('VMOAP52')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
           GO TO START-OVER.

       HELPB.
           EXEC KICKS LINK PROGRAM('VMOAP53')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
           GO TO LOOPA.

       HELPC.
           EXEC KICKS LINK PROGRAM('VMOAP54')
               COMMAREA(WS-COMMAREA)
               LENGTH(80)
             END-EXEC.      
           GO TO CHGU.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC. 
