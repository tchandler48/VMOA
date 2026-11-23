USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP09.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).
    
        01 REC-KEY.
           02 KYR1      PIC X(2).
           02 KMTH      PIC X(2).

        01 REC99.
           02 R-KEY    PIC X(10).
           02 UPWD     PIC X(10).
           02 USYS     PIC X(8).
           02 UTYPE    PIC X.
           02 UNAME    PIC X(25).
           02 UTELE    PIC X(12). 
           02 FILLER   PIC X(27).

       01  WK-DATE1.
           05  WK-MTH   PIC 9(2).
           05  WK-FIL1  PIC 9.
           05  WK-DAY   PIC 99.
           05  WK-YR    PIC 99.
           05  WK-FIL2  PIC 9.

       01  TMP-USR      PIC X(10).

       01  W-LGTH       PIC S9(4) COMP.

       01  WK-CT        PIC 99.

       01  WK-DT.
           05  DY1      PIC XX.
           05  FILLER   PIC X. 
           05  DY2      PIC XX.

       01  WS-COMMON.
           02 CUSER    PIC X(10).
           02 CPWD     PIC X(10).
           02 CTYPE    PIC X.
	   02 CSYS     PIC X(08).
           02 FILLER   PIC X(51).


       COPY VMOAM09.
       COPY DFHBMSCA.
     
       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
      
       START-OVER.
           MOVE CURRENT-DATE     TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY      TO CTIMEFO.
           MOVE WS-CURRENT-DATE  TO CDATEFO.

       BRWU.
           MOVE +80        TO W-LGTH.
           MOVE 00         TO WK-CT.
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
           ADD 1 TO WK-CT.
           IF WK-CT EQUAL 01 THEN
               MOVE R-KEY   TO F01UO
               MOVE UNAME   TO F01NO
               MOVE UTYPE   TO F01TO
               MOVE USYS    TO F01SO
               MOVE UTELE   TO F01PO.
           IF WK-CT EQUAL 02 THEN
               MOVE R-KEY   TO F02UO
               MOVE UNAME   TO F02NO
               MOVE UTYPE   TO F02TO
               MOVE USYS    TO F02SO
               MOVE UTELE   TO F02PO.
           IF WK-CT EQUAL 03 THEN
               MOVE R-KEY   TO F03UO
               MOVE UNAME   TO F03NO
               MOVE UTYPE   TO F03TO
               MOVE USYS    TO F03SO
               MOVE UTELE   TO F03PO.
           IF WK-CT EQUAL 04 THEN
               MOVE R-KEY   TO F04UO
               MOVE UNAME   TO F04NO
               MOVE UTYPE   TO F04TO
               MOVE USYS    TO F04SO
               MOVE UTELE   TO F04PO.
           IF WK-CT EQUAL 05 THEN
               MOVE R-KEY   TO F05UO
               MOVE UNAME   TO F05NO
               MOVE UTYPE   TO F05TO
               MOVE USYS    TO F05SO
               MOVE UTELE   TO F05PO.
           IF WK-CT EQUAL 06 THEN
               MOVE R-KEY   TO F06UO
               MOVE UNAME   TO F06NO
               MOVE UTYPE   TO F06TO
               MOVE USYS    TO F06SO
               MOVE UTELE   TO F06PO.
           IF WK-CT EQUAL 07 THEN
               MOVE R-KEY   TO F07UO
               MOVE UNAME   TO F07NO
               MOVE UTYPE   TO F07TO
               MOVE USYS    TO F07SO
               MOVE UTELE   TO F07PO.
           IF WK-CT EQUAL 08 THEN
               MOVE R-KEY   TO F08UO
               MOVE UNAME   TO F08NO
               MOVE UTYPE   TO F08TO
               MOVE USYS    TO F08SO
               MOVE UTELE   TO F08PO.
           IF WK-CT EQUAL 09 THEN
               MOVE R-KEY   TO F09UO
               MOVE UNAME   TO F09NO
               MOVE UTYPE   TO F09TO
               MOVE USYS    TO F09SO
               MOVE UTELE   TO F09PO.
           IF WK-CT EQUAL 10 THEN
               MOVE R-KEY   TO F10UO
               MOVE UNAME   TO F10NO
               MOVE UTYPE   TO F10TO
               MOVE USYS    TO F10SO
               MOVE UTELE   TO F10PO.
           IF WK-CT EQUAL 11 THEN
               MOVE R-KEY   TO F11UO
               MOVE UNAME   TO F11NO
               MOVE UTYPE   TO F11TO
               MOVE USYS    TO F11SO
               MOVE UTELE   TO F11PO.
           IF WK-CT EQUAL 12 THEN
               MOVE R-KEY   TO F12UO
               MOVE UNAME   TO F12NO
               MOVE UTYPE   TO F12TO
               MOVE USYS    TO F12SO
               MOVE UTELE   TO F12PO.
           IF WK-CT EQUAL 13 THEN
               MOVE R-KEY   TO F13UO
               MOVE UNAME   TO F13NO
               MOVE UTYPE   TO F13TO
               MOVE USYS    TO F13SO
               MOVE UTELE   TO F13PO.
           IF WK-CT EQUAL 14 THEN
               MOVE R-KEY   TO F14UO
               MOVE UNAME   TO F14NO
               MOVE UTYPE   TO F14TO
               MOVE USYS    TO F14SO
               MOVE UTELE   TO F14PO.
           IF WK-CT EQUAL 15 THEN
               MOVE R-KEY   TO F15UO
               MOVE UNAME   TO F15NO
               MOVE UTYPE   TO F15TO
               MOVE USYS    TO F15SO
               MOVE UTELE   TO F15PO.

           GO TO LOOP56.

       LOOP55.
           EXEC KICKS ENDBR 
               FILE('VMOASEC')
           END-EXEC.

           EXEC KICKS
              SEND MAP('MAPF') MAPSET('VMOAM09') ERASE 
              END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELPSC)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPF') MAPSET('VMOAM09')
              END-EXEC.

           GO TO START-OVER.

       HELPSC.
              EXEC KICKS LINK PROGRAM('VMOAP28')
              END-EXEC.      
              GO TO START-OVER.


       ENDPGM.
            EXEC KICKS RETURN END-EXEC.
    
     
    

