USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP36.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  WS-COMMON.
           02 CUSER    PIC X(10).
           02 CPWD     PIC X(10).
           02 CTYPE    PIC X.
	   02 CSYS     PIC X(08).
           02 FILLER   PIC X(51).
 
       COPY VMOAM36.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WS-COMMON.

       START-OVER.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO CDATEO.
  
           MOVE TIME-OF-DAY TO CTIMEO.
<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>
           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM36') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF1(NMESS)
                                 PF2(SMESS)
                                 PF3(MSGLOG)
                                 PF10(HELP10)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM36')
             END-EXEC.

           EXEC KICKS RETURN END-EXEC.
           GO TO START-OVER.


       NMESS.
             EXEC KICKS LINK PROGRAM('VMOAP38')
             COMMAREA(WS-COMMON)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       MSGLOG.
             EXEC KICKS LINK PROGRAM('VMOAP42')
               COMMAREA(WS-COMMON)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.

       HELP10.
             EXEC KICKS LINK PROGRAM('VMOAP43')
                END-EXEC.      
             GO TO START-OVER.

       SMESS.
             EXEC KICKS LINK PROGRAM('VMOAP39')
               COMMAREA(WS-COMMON)
               LENGTH(80)
             END-EXEC.      
             GO TO START-OVER.


       ENDPGM.
           EXEC KICKS RETURN END-EXEC.

 