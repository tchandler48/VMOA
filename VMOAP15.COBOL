USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP15.
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
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       01  RECC.
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
    
       01  REC1.
           05  R-KEY4.
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


       01  REC99.
           02 R-KEY    PIC X(10).
           02 UPWD     PIC X(10).
           02 USYS     PIC X(8).
           02 UTYPE    PIC X. 
           02 UNAME    PIC X(25).
           02 UTEL     PIC X(12).
           02 FILLER   PIC X(14).

       01  REC95.
           05 R-KEY5.
              07 R-USR5   PIC X(10).
              07 R-LIST5  PIC X(10).
              07 R-TYPE5  PIC X.
           02 R-ID5       PIC X(10). 
           02 FILLER      PIC X(49).


       01  REC100.
           05  KEY101.
               07  USERID         PIC X(10).
               07  MTYPE          PIC X.
               07  TOID           PIC X(10).
               07  TDATE          PIC X(6).
               07  TTIME          PIC X(6).
       	   05  TSTATUS            PIC X.
           05  TSUB               PIC X(25).
           05  FILLER             PIC X(21).




       01  W-LGTH      PIC S9(4) COMP.

       01  FCT-1       PIC 9999.
       01  FCT-2       PIC 9999.
       01  FCT-3       PIC 9999.
       01  FCT-4       PIC 9999.
       01  FCT-5       PIC 9999.
       01  FCT-6       PIC 9999.
       01  FCT-7       PIC 9999.

       COPY VMOAM15.
       COPY DFHBMSCA.

       PROCEDURE DIVISION.
       START-OVER.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY TO CTIMEO.
           MOVE WS-CURRENT-DATE TO DATEAO.

<NCB2>
           EXAMINE CTIMEO REPLACING ALL ' ' BY ':'.
</NCB2>

           MOVE +80        TO W-LGTH.
           MOVE 00         TO FCT-1.
           MOVE 00         TO FCT-2.
           MOVE 00         TO FCT-3.
           MOVE 00         TO FCT-4.
           MOVE 00         TO FCT-5.
           MOVE LOW-VALUES TO R-KEY.
           EXEC KICKS STARTBR FILE('VMOASEC')
               RIDFLD(R-KEY) KEYLENGTH(10)
           END-EXEC.

       LOOP56.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOASEC')
               INTO(REC99) LENGTH(W-LGTH)
               RIDFLD(R-KEY) KEYLENGTH(10)
               END-EXEC.
           ADD 1 TO FCT-2.
 
           GO TO LOOP56.

       LOOP55.
           EXEC KICKS ENDBR 
               FILE('VMOASEC')
           END-EXEC.

           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
            
           MOVE +80        TO W-LGTH.
           MOVE ZEROS      TO FCT-3.
           MOVE LOW-VALUES TO R-KEY4.

           EXEC KICKS STARTBR FILE('VMOAURC')
               RIDFLD(R-KEY4) KEYLENGTH(22)
           END-EXEC.

       LOOP46.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP45) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOAURC')
               INTO(REC1) 
               RIDFLD(R-KEY4) KEYLENGTH(22)
               END-EXEC.

               ADD 1      TO  FCT-3.
           
               GO TO LOOP46.

       LOOP45.
           EXEC KICKS ENDBR 
               FILE('VMOAURC')
           END-EXEC.


           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
            
           MOVE +80        TO W-LGTH.
           MOVE ZEROS      TO FCT-4.
           MOVE LOW-VALUES TO R-KEY4.

           EXEC KICKS STARTBR FILE('VMOACRS')
               RIDFLD(R-KEY4) KEYLENGTH(22)
           END-EXEC.

       LOOP36.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP35) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOACRS')
               INTO(REC1) 
               RIDFLD(R-KEY4) KEYLENGTH(22)
               END-EXEC.

               ADD 1      TO  FCT-4.
               GO TO LOOP36.

       LOOP35.
           EXEC KICKS ENDBR 
               FILE('VMOACRS')
           END-EXEC.


           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO WK-DATE1.
            
           MOVE +80        TO W-LGTH.
           MOVE ZEROS      TO FCT-1.
           MOVE LOW-VALUES TO CL-KEY.

           EXEC KICKS STARTBR FILE('VMOACAL')
               RIDFLD(CL-KEY) KEYLENGTH(04)
           END-EXEC.

       LOOP26.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP25) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOACAL')
               INTO(RECC) 
               RIDFLD(CL-KEY) KEYLENGTH(04)
               END-EXEC.

               ADD 1      TO  FCT-1.
               GO TO LOOP26.

       LOOP25.
           EXEC KICKS ENDBR 
               FILE('VMOACAL')
           END-EXEC.

 

           MOVE LOW-VALUES TO R-KEY5.

           EXEC KICKS STARTBR FILE('VMOADST')
               RIDFLD(R-KEY5) KEYLENGTH(31)
           END-EXEC.

       LOOP27.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP28) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOADST')
               INTO(REC95) 
               RIDFLD(R-KEY5) KEYLENGTH(31)
               END-EXEC.

               ADD 1      TO  FCT-5.
               GO TO LOOP27.

       LOOP28.
           EXEC KICKS ENDBR 
               FILE('VMOADST')
           END-EXEC.

       GO TO LOOPX.

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

               ADD 1      TO  FCT-6.
               GO TO LOOP77.

       LOOP78.
           EXEC KICKS ENDBR 
               FILE('VMOAMML')
           END-EXEC.


       LOOPX. 

           MOVE  FCT-1     TO   SCT1O.
           MOVE  FCT-2     TO   SCT2O.
           MOVE  FCT-3     TO   SCT3O.
           MOVE  FCT-4     TO   SCT4O.
           MOVE  FCT-5     TO   SCT5O.
           MOVE  FCT-6     TO   SCT6O.

           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM15') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM15')
             END-EXEC.

           IF SELAI EQUAL '12'
             GO TO ENDPGM.

           GO TO START-OVER.

       ENDPGM.
           EXEC KICKS RETURN END-EXEC.

