USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP10.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

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
           05  WK-FIL3  PIC X.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       01  DAY-YR      PIC 999.

       01  WS-COMMON    PIC X(80).
          
       COPY VMOAM10.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMON.

       START-OVER.
           MOVE LOW-VALUES TO MAPAO.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATE4O.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
         
           MOVE +20    TO SCR-1M.
           MOVE WK-YR  TO SCR-1Y.
           MOVE SCR-YR1 TO SYR1O.
           MOVE SCR-YR1 TO SYR2O.

           MOVE TIME-OF-DAY TO CTIME2O.

<NCB2>
           EXAMINE CTIME2O REPLACING ALL ' ' BY ':'.
</NCB2>
       
           MOVE WK-MTH TO CL-MTH.
           MOVE WK-YR  TO CL-YR1.
           MOVE WK-MTH TO CL-MTH.
           MOVE WK-YR  TO CL-YR1.
                
           EXEC KICKS HANDLE CONDITION NOTFND(LOOP50) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(REC1)           
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
           MOVE CDAY37 TO SDAY37O.
           MOVE CDAY38 TO SDAY38O.
           MOVE CDAY39 TO SDAY39O.
           MOVE CDAY40 TO SDAY40O.
           MOVE CDAY41 TO SDAY41O. 
           MOVE CDAY42 TO SDAY42O.

          

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

           GO TO LOOP61.

           IF CL-MTH EQUAL TO 01
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 02
    
           IF CL-MTH EQUAL TO 03
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 04
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 05
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 06
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 07
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 08
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 09
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 10
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 11
               GO TO LOOP60.
           IF CL-MTH EQUAL TO 12
               GO TO LOOP60.
           GO TO LOOP61.

       LOOP60.
           IF WK-DAY EQUAL CDAY1
                MOVE KIKGREEN TO SDAY1C.
           IF WK-DAY EQUAL CDAY2
                MOVE KIKGREEN TO SDAY2C.
           IF WK-DAY EQUAL CDAY3
                MOVE KIKGREEN TO SDAY3C.
           IF WK-DAY EQUAL CDAY4
                MOVE KIKGREEN TO SDAY4C.
           IF WK-DAY EQUAL CDAY5
                MOVE KIKGREEN TO SDAY5C.
           IF WK-DAY EQUAL CDAY6
                MOVE KIKGREEN TO SDAY6C.
           IF WK-DAY EQUAL CDAY7
                MOVE KIKGREEN TO SDAY7C.
           IF WK-DAY EQUAL CDAY8
                MOVE KIKGREEN TO SDAY8C.
           IF WK-DAY EQUAL CDAY9
                MOVE KIKGREEN TO SDAY9C.
           IF WK-DAY EQUAL CDAY10
                MOVE KIKGREEN TO SDAY10C.
           IF WK-DAY EQUAL CDAY11
                MOVE KIKGREEN TO SDAY11C.
           IF WK-DAY EQUAL CDAY12
                MOVE KIKGREEN TO SDAY12C.
           IF WK-DAY EQUAL CDAY13
                MOVE KIKGREEN TO SDAY13C.
           IF WK-DAY EQUAL CDAY14
                MOVE KIKGREEN TO SDAY14C.
           IF WK-DAY EQUAL CDAY15
                MOVE KIKGREEN TO SDAY15C.
           IF WK-DAY EQUAL CDAY16
                MOVE KIKGREEN TO SDAY16C.
           IF WK-DAY EQUAL CDAY17
                MOVE KIKGREEN TO SDAY17C.
           IF WK-DAY EQUAL CDAY18
                MOVE KIKGREEN TO SDAY18C.
           IF WK-DAY EQUAL CDAY19
                MOVE KIKGREEN TO SDAY19C.
           IF WK-DAY EQUAL CDAY20
                MOVE KIKGREEN TO SDAY20C.
           IF WK-DAY EQUAL CDAY21
                MOVE KIKGREEN TO SDAY21C.
           IF WK-DAY EQUAL CDAY22
                MOVE KIKGREEN TO SDAY22C.
           IF WK-DAY EQUAL CDAY23
                MOVE KIKGREEN TO SDAY23C.
           IF WK-DAY EQUAL CDAY24
                MOVE KIKGREEN TO SDAY24C.
           IF WK-DAY EQUAL CDAY25
                MOVE KIKGREEN TO SDAY25C.
           IF WK-DAY EQUAL CDAY26
                MOVE KIKGREEN TO SDAY26C.
           IF WK-DAY EQUAL CDAY27
                MOVE KIKGREEN TO SDAY27C.
           IF WK-DAY EQUAL CDAY28
                MOVE KIKGREEN TO SDAY28C.
           IF WK-DAY EQUAL CDAY29
                MOVE KIKGREEN TO SDAY29C.
           IF WK-DAY EQUAL CDAY30
                MOVE KIKGREEN TO SDAY30C.
           IF WK-DAY EQUAL CDAY31
                MOVE KIKGREEN TO SDAY31C.
           IF WK-DAY EQUAL CDAY32
                MOVE KIKGREEN TO SDAY32C.
           IF WK-DAY EQUAL CDAY33
                MOVE KIKGREEN TO SDAY33C.
           IF WK-DAY EQUAL CDAY34
                MOVE KIKGREEN TO SDAY34C.
           IF WK-DAY EQUAL CDAY35
                MOVE KIKGREEN TO SDAY35C.
           IF WK-DAY EQUAL CDAY36
                MOVE KIKGREEN TO SDAY36C.
           IF WK-DAY EQUAL CDAY37
                MOVE KIKGREEN TO SDAY37C.
           IF WK-DAY EQUAL CDAY38
                MOVE KIKGREEN TO SDAY38C.
           IF WK-DAY EQUAL CDAY39
                MOVE KIKGREEN TO SDAY39C.
           IF WK-DAY EQUAL CDAY40
                MOVE KIKGREEN TO SDAY40C.
           IF WK-DAY EQUAL CDAY41
                MOVE KIKGREEN TO SDAY41C.
           IF WK-DAY EQUAL CDAY42
                MOVE KIKGREEN TO SDAY42C.

       LOOP61.
           IF WK-MTH EQUAL      01
                MOVE 'JANUARY'   TO SMTHO.
           IF WK-MTH EQUAL      02
                MOVE 'FEBURARY'  TO SMTHO.
           IF WK-MTH  EQUAL     03
                MOVE 'MARCH'     TO SMTHO.
           IF WK-MTH  EQUAL     04
                MOVE 'APRIL'     TO SMTHO.
           IF WK-MTH  EQUAL     05
                MOVE 'MAY'       TO SMTHO.
           IF WK-MTH  EQUAL     06
                MOVE 'JUNE'      TO SMTHO.
           IF WK-MTH EQUAL      07
                MOVE 'JULY'      TO SMTHO.
           IF WK-MTH EQUAL      08
                MOVE 'AUGUST'    TO SMTHO.
           IF WK-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTHO.
           IF WK-MTH  EQUAL     10
                MOVE 'OCTOBER'   TO SMTHO.
           IF WK-MTH  EQUAL     11
                MOVE 'NOVEMBER'  TO SMTHO.
           IF WK-MTH  EQUAL     12
                MOVE 'DECEMBER'  TO SMTHO.
       LOOP50.

           ADD  +1  TO CL-MTH.
           MOVE +26 TO CL-YR1.
           ADD  +1  TO WK-MTH.

           MOVE +20    TO SCR-1M.
           MOVE WK-YR  TO SCR-1Y.
           MOVE SCR-YR1 TO SYR51O.
           MOVE SCR-YR1 TO SYR53O.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP51) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(CL-KEY)      
           END-EXEC.
           
           MOVE CDAY1 TO TDAY1O.
           MOVE CDAY2 TO TDAY2O.
           MOVE CDAY3 TO TDAY3O.
           MOVE CDAY4 TO TDAY4O.
           MOVE CDAY5 TO TDAY5O.
           MOVE CDAY6 TO TDAY6O.
           MOVE CDAY7 TO TDAY7O. 
           MOVE CDAY8 TO TDAY8O.
           MOVE CDAY9 TO TDAY9O.
           MOVE CDAY10 TO TDAY10O.
           MOVE CDAY11 TO TDAY11O.
           MOVE CDAY12 TO TDAY12O.
           MOVE CDAY13 TO TDAY13O.
           MOVE CDAY14 TO TDAY14O. 
           MOVE CDAY15 TO TDAY15O.
           MOVE CDAY16 TO TDAY16O.
           MOVE CDAY17 TO TDAY17O.
           MOVE CDAY18 TO TDAY18O.
           MOVE CDAY19 TO TDAY19O. 
           MOVE CDAY20 TO TDAY20O.
           MOVE CDAY21 TO TDAY21O.
           MOVE CDAY22 TO TDAY22O.
           MOVE CDAY23 TO TDAY23O.
           MOVE CDAY24 TO TDAY24O. 
           MOVE CDAY25 TO TDAY25O.
           MOVE CDAY26 TO TDAY26O.
           MOVE CDAY27 TO TDAY27O.
           MOVE CDAY28 TO TDAY28O.
           MOVE CDAY29 TO TDAY29O. 
           MOVE CDAY30 TO TDAY30O.
           MOVE CDAY31 TO TDAY31O.
           MOVE CDAY32 TO TDAY32O.
           MOVE CDAY33 TO TDAY33O.
           MOVE CDAY34 TO TDAY34O. 
           MOVE CDAY35 TO TDAY35O.
           MOVE CDAY36 TO TDAY36O.
           MOVE CDAY37 TO TDAY37O.
           MOVE CDAY38 TO TDAY38O.
           MOVE CDAY39 TO TDAY39O. 
           MOVE CDAY40 TO TDAY40O.
           MOVE CDAY41 TO TDAY41O.
           MOVE CDAY42 TO TDAY42O.

           IF WK-MTH EQUAL      01
                MOVE 'JANUARY'   TO SMTH52O.
           IF WK-MTH EQUAL      02
                MOVE 'FEBURARY'  TO SMTH52O.
           IF WK-MTH  EQUAL     03
                MOVE 'MARCH'     TO SMTH52O.
           IF WK-MTH  EQUAL     04
                MOVE 'APRIL'     TO SMTH52O.
           IF WK-MTH  EQUAL     05
                MOVE 'MAY'       TO SMTH52O.
           IF WK-MTH  EQUAL     06
                MOVE 'JUNE'      TO SMTH52O.
           IF WK-MTH EQUAL      07
                MOVE 'JULY'      TO SMTH52O.
           IF WK-MTH EQUAL      08
                MOVE 'AUGUST'    TO SMTH52O.
           IF WK-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTH52O.
           IF WK-MTH  EQUAL     10
                MOVE 'OCTOBER'   TO SMTH52O.
           IF WK-MTH  EQUAL     11
                MOVE 'NOVEMBER'  TO SMTH52O.
           IF WK-MTH  EQUAL     12
                MOVE 'DECEMBER'  TO SMTH52O.


       LOOP51.
           ADD  +1  TO CL-MTH.
           MOVE +26 TO CL-YR1.
           ADD  +1  TO WK-MTH.

           MOVE +20    TO SCR-1M.
           MOVE WK-YR  TO SCR-1Y.
           MOVE SCR-YR1 TO SYR13O.
           MOVE SCR-YR1 TO SYR15O.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP53) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(CL-KEY)      
           END-EXEC.

           MOVE CDAY1 TO UDAY1O.
           MOVE CDAY2 TO UDAY2O.
           MOVE CDAY3 TO UDAY3O.
           MOVE CDAY4 TO UDAY4O.
           MOVE CDAY5 TO UDAY5O.
           MOVE CDAY6 TO UDAY6O.
           MOVE CDAY7 TO UDAY7O. 
           MOVE CDAY8 TO UDAY8O.
           MOVE CDAY9 TO UDAY9O.
           MOVE CDAY10 TO UDAY10O.
           MOVE CDAY11 TO UDAY11O.
           MOVE CDAY12 TO UDAY12O.
           MOVE CDAY13 TO UDAY13O.
           MOVE CDAY14 TO UDAY14O. 
           MOVE CDAY15 TO UDAY15O.
           MOVE CDAY16 TO UDAY16O.
           MOVE CDAY17 TO UDAY17O.
           MOVE CDAY18 TO UDAY18O.
           MOVE CDAY19 TO UDAY19O. 
           MOVE CDAY20 TO UDAY20O.
           MOVE CDAY21 TO UDAY21O.
           MOVE CDAY22 TO UDAY22O.
           MOVE CDAY23 TO UDAY23O.
           MOVE CDAY24 TO UDAY24O.
           MOVE CDAY25 TO UDAY25O.
           MOVE CDAY26 TO UDAY26O. 
           MOVE CDAY27 TO UDAY27O.
           MOVE CDAY28 TO UDAY28O.
           MOVE CDAY29 TO UDAY29O.
           MOVE CDAY30 TO UDAY30O.
           MOVE CDAY31 TO UDAY31O.
           MOVE CDAY32 TO UDAY32O.
           MOVE CDAY33 TO UDAY33O. 
           MOVE CDAY34 TO UDAY34O.
           MOVE CDAY35 TO UDAY35O.
           MOVE CDAY36 TO UDAY36O.
           MOVE CDAY37 TO UDAY37O.
           MOVE CDAY38 TO UDAY38O.
           MOVE CDAY39 TO UDAY39O.
           MOVE CDAY40 TO UDAY40O. 
           MOVE CDAY41 TO UDAY41O.
           MOVE CDAY42 TO UDAY42O.

           IF WK-MTH EQUAL      01
                MOVE 'JANUARY'   TO SMTH14O.
           IF WK-MTH EQUAL      02
                MOVE 'FEBURARY'  TO SMTH14O.
           IF WK-MTH  EQUAL     03
                MOVE 'MARCH'     TO SMTH14O.
           IF WK-MTH  EQUAL     04
                MOVE 'APRIL'     TO SMTH14O.
           IF WK-MTH  EQUAL     05
                MOVE 'MAY'       TO SMTH14O.
           IF WK-MTH  EQUAL     06
                MOVE 'JUNE'      TO SMTH14O.
           IF WK-MTH EQUAL      07
                MOVE 'JULY'      TO SMTH14O.
           IF WK-MTH EQUAL      08
                MOVE 'AUGUST'    TO SMTH14O.
           IF WK-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTH14O.
           IF WK-MTH  EQUAL     10
                MOVE 'OCTOBER'   TO SMTH14O.
           IF WK-MTH  EQUAL     11
                MOVE 'NOVEMBER'  TO SMTH14O.
           IF WK-MTH  EQUAL     12
                MOVE 'DECEMBER'  TO SMTH14O.


       LOOP52.
           ADD  +1  TO CL-MTH.
           MOVE +26 TO CL-YR1.
           ADD  +1  TO WK-MTH.

           MOVE +20    TO SCR-1M.
           MOVE WK-YR  TO SCR-1Y.
           MOVE SCR-YR1 TO SYR16O.
           MOVE SCR-YR1 TO SYR18O.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP53) END-EXEC.
           EXEC CICS READ
             FILE('VMOACAL')      
             INTO(REC1)           
             RIDFLD(CL-KEY)      
           END-EXEC.

           MOVE CDAY1 TO VDAY1O.
           MOVE CDAY2 TO VDAY2O.
           MOVE CDAY3 TO VDAY3O.
           MOVE CDAY4 TO VDAY4O.
           MOVE CDAY5 TO VDAY5O.
           MOVE CDAY6 TO VDAY6O.
           MOVE CDAY7 TO VDAY7O. 
           MOVE CDAY8 TO VDAY8O.
           MOVE CDAY9 TO VDAY9O.
           MOVE CDAY10 TO VDAY10O.
           MOVE CDAY11 TO VDAY11O.
           MOVE CDAY12 TO VDAY12O.
           MOVE CDAY13 TO VDAY13O.
           MOVE CDAY14 TO VDAY14O. 
           MOVE CDAY15 TO VDAY15O.
           MOVE CDAY16 TO VDAY16O.
           MOVE CDAY17 TO VDAY17O.
           MOVE CDAY18 TO VDAY18O.
           MOVE CDAY19 TO VDAY19O.
           MOVE CDAY30 TO VDAY20O.
           MOVE CDAY21 TO VDAY21O. 
           MOVE CDAY22 TO VDAY22O.
           MOVE CDAY23 TO VDAY23O.
           MOVE CDAY24 TO VDAY24O.
           MOVE CDAY25 TO VDAY25O.
           MOVE CDAY26 TO VDAY26O.
           MOVE CDAY27 TO VDAY27O.
           MOVE CDAY28 TO VDAY28O. 
           MOVE CDAY29 TO VDAY29O.
           MOVE CDAY30 TO VDAY30O.
           MOVE CDAY31 TO VDAY31O.
           MOVE CDAY32 TO VDAY32O.
           MOVE CDAY33 TO VDAY33O.
           MOVE CDAY34 TO VDAY34O.
           MOVE CDAY35 TO VDAY35O. 
           MOVE CDAY36 TO VDAY36O.
           MOVE CDAY37 TO VDAY37O.
           MOVE CDAY38 TO VDAY38O.
           MOVE CDAY39 TO VDAY39O.
           MOVE CDAY40 TO VDAY40O.
           MOVE CDAY41 TO VDAY41O.
           MOVE CDAY42 TO VDAY42O. 

           IF WK-MTH EQUAL      01
                MOVE 'JANUARY'   TO SMTH17O.
           IF WK-MTH EQUAL      02
                MOVE 'FEBURARY'  TO SMTH17O.
           IF WK-MTH  EQUAL     03
                MOVE 'MARCH'     TO SMTH17O.
           IF WK-MTH  EQUAL     04
                MOVE 'APRIL'     TO SMTH17O.
           IF WK-MTH  EQUAL     05
                MOVE 'MAY'       TO SMTH17O.
           IF WK-MTH  EQUAL     06
                MOVE 'JUNE'      TO SMTH17O.
           IF WK-MTH EQUAL      07
                MOVE 'JULY'      TO SMTH17O.
           IF WK-MTH EQUAL      08
                MOVE 'AUGUST'    TO SMTH17O.
           IF WK-MTH  EQUAL     09
                MOVE 'SEPTEMBER' TO SMTH17O.
           IF WK-MTH  EQUAL     10
                MOVE 'OCTOBER'   TO SMTH17O.
           IF WK-MTH  EQUAL     11
                MOVE 'NOVEMBER'  TO SMTH17O.
           IF WK-MTH  EQUAL     12
                MOVE 'DECEMBER'  TO SMTH17O.

       LOOP53.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM10') ERASE
             END-EXEC.
         
           EXEC KICKS RECEIVE MAP('MAPA') MAPSET('VMOAM10')
             END-EXEC.


       ENDPGM.
             EXEC KICKS RETURN END-EXEC.
            

      

     
