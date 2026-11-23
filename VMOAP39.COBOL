USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP39.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  REC-1.
           05  KEY1.
               07  USERID         PIC X(10).
               07  MTYPE          PIC X.
               07  TOID           PIC X(10).
               07  TDATE.
                   09  TYR        PIC XX.
                   09  TMTH       PIC XX.
                   09  TDAY       PIC XX.
               07  TTIME          PIC X(6).
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
               07  R2-TTIME       PIC X(6).
               07  R2-CT          PIC 9.
       	   05  R2-TSTATUS         PIC X.
           05  R2-TSUB            PIC X(25).
           05  R2-MSG             PIC X(65).
           05  FILLER             PIC X(03).

       01  WS-COMMON.
           02 CUSER    PIC X(10).
           02 CPWD     PIC X(10).
           02 CTYPE    PIC X.
	   02 CSYS     PIC X(08).
           02 FILLER   PIC X(51).

       01  WK-CT       PIC 99.

       01  WK-TIME.
           05  WK-HR   PIC 99.
           05  WK-MIN  PIC 99.
           05  WK-SEC  PIC 99.

       01  DT-DATE.
           05  DT-MTH      PIC XX.
           05  FILLER      PIC X.
           05  DT-DAY      PIC XX.
           05  FILLER      PIC X.
           05  DT-YR       PIC XX.


 
       COPY VMOAM39.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMON.

       START-OVER.
           MOVE LOW-VALUES TO MAPAO.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE TIME-OF-DAY TO WK-TIME.

<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>
           MOVE CURRENT-DATE TO  DT-DATE.
 
           MOVE DT-YR      TO  TYR.
           MOVE DT-MTH     TO  TMTH.
           MOVE DT-DAY     TO  TDAY.
           MOVE TDATE      TO  ERRAO.

           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM39') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF7(MWRITE)
                                 PF10(HELP41)
                                 PF12(ENDPGM) 
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM39')
             END-EXEC.

           EXEC KICKS RETURN END-EXEC.
           GO TO START-OVER.

       MWRITE.
          
           MOVE CUSER        TO USERID.
           MOVE 'M'          TO MTYPE.
           MOVE 'U'          TO TSTATUS.
           MOVE SDMSGI       TO TOID.
           MOVE TIME-OF-DAY  TO WK-TIME.
           MOVE WK-TIME      TO TTIME.
           MOVE CURRENT-DATE TO  DT-DATE.
           MOVE DT-YR      TO  TYR.
           MOVE DT-MTH     TO  TMTH.
           MOVE DT-DAY     TO  TDAY.
           MOVE SPACES      TO TSUB.

          EXEC KICKS WRITE FILE('VMOAMML')
                      FROM(REC-1)
                      RIDFLD(KEY1)
                      END-EXEC.

           MOVE CUSER        TO R2-USER.
           MOVE 'M'          TO R2-MTYPE.
           MOVE SDMSGI       TO R2-TOID.
           MOVE WK-TIME      TO R2-TTIME.
           MOVE DT-YR        TO R2-TYR.
           MOVE DT-MTH       TO R2-TMTH.
           MOVE DT-DAY       TO R2-TDAY.
           MOVE SPACES       TO TSUB.
           MOVE 1            TO R2-CT.
           MOVE SDL1I        TO R2-MSG.

           EXEC KICKS WRITE FILE('VMOAMDT')
                      FROM(REC-2)
                      RIDFLD(KEY2)
                      END-EXEC.

           MOVE CUSER        TO R2-USER.
           MOVE 'M'          TO R2-MTYPE.
           MOVE SDMSGI       TO R2-TOID.
           MOVE WK-TIME      TO R2-TTIME.
           MOVE DT-YR        TO R2-TYR.
           MOVE DT-MTH       TO R2-TMTH.
           MOVE DT-DAY       TO R2-TDAY.
           MOVE SPACES       TO TSUB.
           MOVE 2            TO R2-CT.
           MOVE SDL2I        TO R2-MSG.

           EXEC KICKS WRITE FILE('VMOAMDT')
                      FROM(REC-2)
                      RIDFLD(KEY2)
                      END-EXEC.

           MOVE CUSER        TO R2-USER.
           MOVE 'M'          TO R2-MTYPE.
           MOVE SDMSGI       TO R2-TOID.
           MOVE WK-TIME      TO R2-TTIME.
           MOVE DT-YR        TO R2-TYR.
           MOVE DT-MTH       TO R2-TMTH.
           MOVE DT-DAY       TO R2-TDAY.
           MOVE SPACES       TO TSUB.
           MOVE 3            TO R2-CT.
           MOVE SDL3I        TO R2-MSG.

           EXEC KICKS WRITE FILE('VMOAMDT')
                      FROM(REC-2)
                      RIDFLD(KEY2)
                      END-EXEC.





           GO TO START-OVER.

       HELP41.
           EXEC KICKS LINK PROGRAM('VMOAP41')
              END-EXEC.      
           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.
           GO TO START-OVER.

 

