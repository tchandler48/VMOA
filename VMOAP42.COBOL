USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP42.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  REC100.
           05  KEY101.
               07  USERID         PIC X(10).
               07  MTYPE          PIC X.
               07  TOID           PIC X(10).
               07  TDATE.
                   09 TYR         PIC XX.
                   09 TMTH        PIC XX.
                   09 TDAY        PIC XX.
               07  TTIME.
                   09 THH         PIC XX.
                   09 TMM         PIC XX.
                   09 TSS         PIC XX.
       	   05  TSTATUS            PIC X.
           05  TSUB               PIC X(25).
           05  FILLER             PIC X(21).

       01  REC-2.
           05  KEY2.
               07  R2-USER        PIC X(10).
               07  R2-MTYPE       PIC X.
               07  R2-TOID        PIC X(10).
               07  R2-TDATE.
                   09  R2-TYR     PIC XX.
                   09  R2-TMTH    PIC XX.
                   09  R2-TDAY    PIC XX.
               07  R2-TTIME.
                   09  R2-HH      PIC XX.
                   09  R2-MM      PIC XX.
                   09  R2-SEC     PIC XX.
               07  R2-CT          PIC 9.
       	   05  R2-TSTATUS         PIC X.
           05  R2-TSUB            PIC X(25).
           05  R2-MSG             PIC X(65).
           05  FILLER             PIC X(03).

       01  WK-CT                  PIC 99.

       01  TMP-DATE.
           05  TMP-MTH            PIC XX.
           05  FILLER             PIC X VALUE '/'.
           05  TMP-DAY            PIC XX.
           05  FILLER             PIC X VALUE '/'.
           05  TMP-YR             PIC XX.

       01  TMP-TIME.
           05  TMP-HH             PIC XX.
           05  FILLER             PIC X VALUE ':'.
           05  TMP-MM             PIC XX.
           05  FILLER             PIC X VALUE ':'.
           05  TMP-SEC            PIC XX.

       01  WK-LINE.
	   05  WK-FROM            PIC X(10).
           05  FILLER             PIC X(12) VALUE SPACES.
           05  WK-TO              PIC X(10).
           05  FILLER             PIC X(09) VALUE SPACES.
           05  WK-TYPE            PIC X(07).
           05  FILLER             PIC X(05) VALUE SPACES.
           05  WK-DATE            PIC X(08).
           05  FILLER             PIC X VALUE SPACES.
           05  WK-TIME            PIC X(08). 
           05  FILLER             PIC X VALUE SPACES. 

       01  RET-LINE.
	   05  RET-FROM            PIC X(10).
           05  RET-TO              PIC X(10).
           05  FILLER             PIC X(09) VALUE SPACES.
           05  RE-TYPE            PIC X(07).
           05  FILLER             PIC X(05) VALUE SPACES.
           05  RE-DATE            PIC X(08).
           05  FILLER             PIC X VALUE SPACES.
           05  RET-TIME            PIC X(08). 
           05  FILLER             PIC X VALUE SPACES. 

       01  WS-COMMON.
           02 CUSER    PIC X(10).
           02 CPWD     PIC X(10).
           02 CTYPE    PIC X.
	   02 CSYS     PIC X(08).
           02 FILLER   PIC X(51).
               
       01  W-LGTH       PIC S9(8) COMP.
       01  K-LGTH       PIC S9(4) COMP.

       01  XX-DATE.
           05  XX-MM    PIC XX.
           05  FILLER   PIC X.
           05  XX-DAY   PIC XX.
           05  FILLER   PIC X.
           05  XX-YR    PIC XX.

       01  XX-TIME.
           05  XX-HR    PIC XX.
           05  FILLER   PIC X.
           05  XX-MIN   PIC XX.
           05  FILLER   PIC X.
           05  XX-SEC   PIC XX.

       01  SV-KEY1      PIC X(34).
           

       COPY VMOAM42.
       COPY DFHBMSCA.


       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMON.
           MOVE LOW-VALUES  TO MAPAO.
           MOVE LOW-VALUES  TO MAPBO.

       START-OVER.
           MOVE CURRENT-DATE    TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
           MOVE TIME-OF-DAY     TO CTIMEO.
           MOVE ZEROS           TO WK-CT.
<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>
           MOVE LOW-VALUES TO KEY101.
           EXEC KICKS STARTBR FILE('VMOAMML')
               RIDFLD(KEY101) KEYLENGTH(33)
           END-EXEC.

       LOOP77.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP78) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAMML')
               INTO(REC100) 
               RIDFLD(KEY101)
               END-EXEC.

           IF USERID EQUAL CUSER THEN
               GO TO LOOP71.
           IF TOID   EQUAL CUSER THEN
               GO TO LOOP71.
           GO TO LOOP77.

       LOOP71.
               ADD 1 TO WK-CT.
               IF WK-CT GREATER THAN 7 THEN
                    GO TO LOOP78.

               IF WK-CT EQUAL 1 THEN
                    MOVE LOW-VALUES   TO LN1O
                    MOVE LOW-VALUES   TO WK-LINE
                    MOVE USERID       TO WK-FROM
                    MOVE TOID         TO WK-TO
                    MOVE TSTATUS      TO LN1STO
                    MOVE TYR          TO TMP-YR
                    MOVE TMTH         TO TMP-MTH
                    MOVE TDAY         TO TMP-DAY
                    MOVE TMP-DATE     TO WK-DATE
                    MOVE THH          TO TMP-HH
                    MOVE TMM          TO TMP-MM
                    MOVE TSS          TO TMP-SEC
                    MOVE TMP-TIME     TO WK-TIME
                    IF MTYPE EQUAL 'M' THEN
                       MOVE 'MESSAGE' TO WK-TYPE
                    MOVE WK-LINE      TO LN1O
                    GO TO LOOP77.

               IF WK-CT EQUAL 2 THEN
                    MOVE LOW-VALUES   TO LN2O
                    MOVE LOW-VALUES   TO WK-LINE
                    MOVE USERID       TO WK-FROM
                    MOVE TOID         TO WK-TO
                    MOVE TSTATUS      TO LN2STO
                    MOVE TYR          TO TMP-YR
                    MOVE TMTH         TO TMP-MTH
                    MOVE TDAY         TO TMP-DAY
                    MOVE TMP-DATE     TO WK-DATE
                    MOVE THH          TO TMP-HH
                    MOVE TMM          TO TMP-MM
                    MOVE TSS          TO TMP-SEC
                    MOVE TMP-TIME     TO WK-TIME
                    IF MTYPE EQUAL 'M' THEN
                       MOVE 'MESSAGE' TO WK-TYPE
                    MOVE WK-LINE      TO LN2O
                    GO TO LOOP77.


       LOOP78.
           EXEC KICKS ENDBR 
               FILE('VMOAMML')
           END-EXEC.

           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM42') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELP10)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM42')
             END-EXEC.

       
           IF SEL1I NOT EQUAL 'D' THEN
             GO TO LOOP101.
           MOVE LN1I       TO RET-LINE.
           MOVE RET-FROM   TO USERID.
           MOVE 'M'        TO MTYPE.
           MOVE RET-TO     TO TOID.
           MOVE WK-DATE    TO XX-DATE.
           MOVE WK-TIME    TO XX-TIME.
           MOVE XX-YR      TO TYR.
           MOVE XX-MM      TO TMTH.
           MOVE XX-DAY     TO TDAY.
           MOVE XX-HR      TO THH.
           MOVE XX-MIN     TO TMM.
           MOVE XX-SEC     TO TSS.
         
           MOVE +33       TO W-LGTH.
           EXEC KICKS DELETE 
              FILE('VMOAMML')
              RIDFLD(KEY101) KEYLENGTH(W-LGTH)
           END-EXEC.
           GO TO START-OVER.

       LOOP101.
           IF SEL1I NOT EQUAL 'R' THEN
             GO TO LOOP100.
           MOVE LN1I       TO RET-LINE.
           MOVE RET-FROM   TO R2-USER.
           MOVE 'M'        TO R2-MTYPE.
           MOVE RET-TO     TO R2-TOID.
           MOVE WK-DATE    TO XX-DATE.
           MOVE WK-TIME    TO XX-TIME.
           MOVE XX-YR      TO R2-TYR.
           MOVE XX-MM      TO R2-TMTH.
           MOVE XX-DAY     TO R2-TDAY.
           MOVE XX-HR      TO R2-HH.
           MOVE XX-MIN     TO R2-MM.
           MOVE XX-SEC     TO R2-SEC.
           MOVE 1          TO R2-CT.

           MOVE KEY2       TO SV-KEY1.
            
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP50) END-EXEC.
           EXEC KICKS READ FILE('VMOAMDT')
               INTO(REC-2) 
               RIDFLD(KEY2)
               END-EXEC.
           MOVE LOW-VALUES  TO SDL1BO.
           MOVE R2-MSG      TO SDL1BO.
           MOVE RET-TO      TO SDMSGBO.
           MOVE RET-FROM    TO BFROMO.
           MOVE WK-DATE     TO BDATEO.
           MOVE WK-TIME     TO BTIMEO.
  

           MOVE SV-KEY1    TO KEY2.
           MOVE 2          TO R2-CT.

    

           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP50) END-EXEC.
           EXEC KICKS READ FILE('VMOAMDT')
               INTO(REC-2) 
               RIDFLD(KEY2)
               END-EXEC.
           MOVE R2-MSG      TO SDL2BO.

           MOVE SV-KEY1    TO KEY2.
           MOVE 3          TO R2-CT.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP50) END-EXEC.
           EXEC KICKS READ FILE('VMOAMDT')
               INTO(REC-2) 
               RIDFLD(KEY2)
               END-EXEC.
           MOVE R2-MSG      TO SDL3BO.

       LOOP50.

           MOVE SV-KEY1    TO KEY101.
	   
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP178) END-EXEC.
           EXEC KICKS READ FILE('VMOAMML')
               INTO(REC100) 
               RIDFLD(KEY101)
               UPDATE
               END-EXEC.
           MOVE 'R'      TO TSTATUS.
       
           EXEC KICKS REWRITE FILE('VMOAMML')
             FROM(REC100)
             END-EXEC.



       LOOP178.
           MOVE TIME-OF-DAY TO CTIMEBO.
<NCB2>
           EXAMINE CTIMEBO REPLACING ALL ' ' BY ':'.
</NCB2>
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEBO.

           EXEC KICKS
             SEND MAP('MAPB') MAPSET('VMOAM42') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELP10)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPB') MAPSET('VMOAM42')
             END-EXEC.


           GO TO START-OVER.

       LOOP100.
           EXEC KICKS RETURN END-EXEC.
           GO TO START-OVER.

       HELP10.
           EXEC KICKS LINK PROGRAM('VMOAP44')
               COMMAREA(WS-COMMON)
               LENGTH(80)
           END-EXEC.      
           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.
           GO TO START-OVER.

KICKS