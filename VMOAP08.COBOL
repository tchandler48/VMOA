USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP08.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

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


       01  REC99.
           02 R-KEY    PIC X(10).
           02 UPWD     PIC X(10).
           02 USYS     PIC X(8).
           02 UTYPE    PIC X. 
           02 UNAME    PIC X(25).
           02 UTEL     PIC X(12).
           02 FILLER   PIC X(14).

       01  W-LGTH      PIC S9(4) COMP.

       01  WK-CT       PIC 99.


       COPY VMOAM08.
       COPY DFHBMSCA.
     
       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA   TO  WS-COMMAREA.
           MOVE LOW-VALUES TO USRCO.
       START-OVER.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY TO CTIMECO.
           MOVE WS-CURRENT-DATE TO CDATECO.

<NCB2>
           EXAMINE CTIMECO REPLACING ALL ' ' BY ':'.
</NCB2>

           MOVE WS-COMMAREA TO ERRCO.
       ERR11.

           EXEC KICKS
             SEND MAP('MAPC') MAPSET('VMOAM08') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELPSC)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPC') MAPSET('VMOAM08')
             END-EXEC.

           IF SELI EQUAL 'B'
              GO TO BRWU.

       LOOP32.
           IF SELI EQUAL 'A'
              GO TO ADDU.
           IF SELI EQUAL 'C'
              GO TO CHGU.
           IF SELI EQUAL 'D'
              GO TO DELU.
           GO TO START-OVER.

       ADDU.
           MOVE USRCI TO R-KEY.
           EXEC KICKS HANDLE CONDITION NOTFND(LOOP52) 
				       END-EXEC. 
           EXEC KICKS READ
             FILE('VMOASEC') 
             UPDATE 
             EQUAL    
             INTO(REC99)           
             RIDFLD(R-KEY)
           END-EXEC.
           
       LOOP51.
           MOVE 'USER ALREADY IN FILE' TO ERRCO.
           GO TO ERR11.

       LOOP52.
           MOVE LOW-VALUES TO MAPEO.
           MOVE TIME-OF-DAY TO CTIMEEO.
    
           MOVE '  ADD' TO MAPNMEO.
           MOVE WS-COMMAREA  TO PCOMMO.
           MOVE WS-CURRENT-DATE TO CDATEEO.

<NCB2>
           EXAMINE CTIMEEO REPLACING ALL ' ' BY ':'.
</NCB2>

           MOVE USRCI         TO EUSRO.
           MOVE USRCI         TO EUSRI.

           EXEC KICKS
              SEND MAP('MAPE') MAPSET('VMOAM08') ERASE
              END-EXEC.

           EXEC KICKS HANDLE AID PF1(START-OVER)
                                 PF3(MAPDWE)
                                 PF10(HELPE)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPE') MAPSET('VMOAM08')
              END-EXEC.
           GO TO START-OVER.

       MAPDWE.
           MOVE EUSRI     TO R-KEY.
           MOVE PWDEI     TO UPWD.
           MOVE VSYSEI    TO USYS.
           MOVE VNAMEEI   TO UNAME.
           MOVE VTYPEEI   TO UTYPE.
           MOVE UTELEI    TO UTEL.
           MOVE +80       TO W-LGTH.
  
           EXEC KICKS WRITE FILE('VMOASEC')
                      FROM(REC99)
                      LENGTH(W-LGTH)
                      RIDFLD(R-KEY)
           END-EXEC.
           GO TO START-OVER.

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
               MOVE UPWD    TO F01PO
               MOVE UNAME   TO F01NO
               MOVE UTYPE   TO F01TO
               MOVE USYS    TO F01SO
               MOVE UTEL    TO F01PHO.
           IF WK-CT EQUAL 02 THEN
               MOVE R-KEY   TO F02UO
               MOVE UPWD    TO F02PO
               MOVE UNAME   TO F02NO
               MOVE UTYPE   TO F02TO
               MOVE USYS    TO F02SO
               MOVE UTEL    TO F02PHO.
           IF WK-CT EQUAL 03 THEN
               MOVE R-KEY   TO F03UO
               MOVE UPWD    TO F03PO
               MOVE UNAME   TO F03NO
               MOVE UTYPE   TO F03TO
               MOVE USYS    TO F03SO
               MOVE UTEL    TO F03PHO.
           IF WK-CT EQUAL 04 THEN
               MOVE R-KEY   TO F04UO
               MOVE UPWD    TO F04PO
               MOVE UNAME   TO F04NO
               MOVE UTYPE   TO F04TO
               MOVE USYS    TO F04SO
               MOVE UTEL    TO F04PHO.
           IF WK-CT EQUAL 05 THEN
               MOVE R-KEY   TO F05UO
               MOVE UPWD    TO F05PO
               MOVE UNAME   TO F05NO
               MOVE UTYPE   TO F05TO
               MOVE USYS    TO F05SO
               MOVE UTEL    TO F05PHO.
           IF WK-CT EQUAL 06 THEN
               MOVE R-KEY   TO F06UO
               MOVE UPWD    TO F06PO
               MOVE UNAME   TO F06NO
               MOVE UTYPE   TO F06TO
               MOVE USYS    TO F06SO
               MOVE UTEL    TO F06PHO.
           IF WK-CT EQUAL 07 THEN
               MOVE R-KEY   TO F07UO
               MOVE UPWD    TO F07PO
               MOVE UNAME   TO F07NO
               MOVE UTYPE   TO F07TO
               MOVE USYS    TO F07SO
               MOVE UTEL    TO F07PHO.
           IF WK-CT EQUAL 08 THEN
               MOVE R-KEY   TO F08UO
               MOVE UPWD    TO F08PO
               MOVE UNAME   TO F08NO
               MOVE UTYPE   TO F08TO
               MOVE USYS    TO F08SO
               MOVE UTEL    TO F08PHO.
           IF WK-CT EQUAL 09 THEN
               MOVE R-KEY   TO F09UO
               MOVE UPWD    TO F09PO
               MOVE UNAME   TO F09NO
               MOVE UTYPE   TO F09TO
               MOVE USYS    TO F09SO
               MOVE UTEL    TO F09PHO.
           IF WK-CT EQUAL 10 THEN
               MOVE R-KEY   TO F010UO
               MOVE UPWD    TO F010PO
               MOVE UNAME   TO F010NO
               MOVE UTYPE   TO F010TO
               MOVE USYS    TO F010SO
               MOVE UTEL    TO F010PHO.
           IF WK-CT EQUAL 11 THEN
               MOVE R-KEY   TO F011UO
               MOVE UPWD    TO F011PO
               MOVE UNAME   TO F011NO
               MOVE UTYPE   TO F011TO
               MOVE USYS    TO F011SO
               MOVE UTEL    TO F011PHO.
           IF WK-CT EQUAL 12 THEN
               MOVE R-KEY   TO F012UO
               MOVE UPWD    TO F012PO
               MOVE UNAME   TO F012NO
               MOVE UTYPE   TO F012TO
               MOVE USYS    TO F012SO
               MOVE UTEL    TO F012PHO.
           IF WK-CT EQUAL 13 THEN
               MOVE R-KEY   TO F013UO
               MOVE UPWD    TO F013PO
               MOVE UNAME   TO F013NO
               MOVE UTYPE   TO F013TO
               MOVE USYS    TO F013SO
               MOVE UTEL    TO F013PHO.
           IF WK-CT EQUAL 14 THEN
               MOVE R-KEY   TO F014UO
               MOVE UPWD    TO F014PO
               MOVE UNAME   TO F014NO
               MOVE UTYPE   TO F014TO
               MOVE USYS    TO F014SO
               MOVE UTEL    TO F014PHO.
           IF WK-CT EQUAL 15 THEN
               MOVE R-KEY   TO F015UO
               MOVE UPWD    TO F015PO
               MOVE UNAME   TO F015NO
               MOVE UTYPE   TO F015TO
               MOVE USYS    TO F015SO
               MOVE UTEL    TO F015PHO.
           GO TO LOOP56.

       LOOP55.
           EXEC KICKS ENDBR 
               FILE('VMOASEC')
           END-EXEC.

           MOVE 'BROWSE'  TO MAPNMFO.
           MOVE TIME-OF-DAY TO CTIMEFO.
           MOVE WS-CURRENT-DATE TO CDATEFO.

           EXEC KICKS
              SEND MAP('MAPF') MAPSET('VMOAM08') ERASE 
              END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELPSC1)
                                 PF12(START-OVER)
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPF') MAPSET('VMOAM08')
              END-EXEC.
           GO TO START-OVER.


       CHGU.
           MOVE USRCI TO R-KEY.  
           EXEC KICKS HANDLE CONDITION NOTFND(LOOP19) END-EXEC.
           EXEC KICKS READ FILE('VMOASEC')
                      INTO(REC99)
                      UPDATE 
                      RIDFLD(R-KEY)
           END-EXEC.

           MOVE TIME-OF-DAY TO CTIMEDO.
           MOVE LOW-VALUES TO USRDO.
           MOVE R-KEY      TO USRDO.
           MOVE UPWD       TO PWDDO.
           MOVE USYS       TO VSYSDO.
           MOVE UNAME      TO VNAMEDO.
           MOVE UTYPE      TO VTYPEDO.
           MOVE UTEL       TO VTELEO.
           MOVE 'CHANGE'   TO MAPNMDO.
           GO TO LOOP20.

       LOOP19.
           MOVE 'USER NOT FOUND - REENTER' TO ERRCO.
           GO TO START-OVER.

       LOOP20.
           MOVE WS-CURRENT-DATE TO CDATEDO.

           EXEC KICKS
              SEND MAP('MAPD') MAPSET('VMOAM08') ERASE 
              END-EXEC.

           EXEC KICKS HANDLE AID PF3(MAPDW)
                                 PF10(HELPD)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPD') MAPSET('VMOAM08')
              END-EXEC.

           IF SELDI EQUAL '3' THEN
              GO TO MAPDW.
           IF SELDI EQUAL '12' THEN
              GO TO ENDPGM.
           GO TO START-OVER.

        MAPDW.
           MOVE +10   TO W-LGTH.

           EXEC KICKS DELETE 
              FILE('VMOASEC')
              RIDFLD(R-KEY) KEYLENGTH(W-LGTH)
           END-EXEC.
           
           MOVE PWDDI      TO UPWD.
           MOVE VSYSDI     TO USYS.
           MOVE VNAMEDI    TO UNAME.
           MOVE VTYPEDI    TO UTYPE.
           MOVE VTELEI     TO UTEL.
           MOVE +80        TO W-LGTH.
 
           EXEC KICKS WRITE FILE('VMOASEC')
                      FROM(REC99)
                      LENGTH(W-LGTH)
                      RIDFLD(R-KEY)
           END-EXEC.

           GO TO ENDPGM.


       DELU.
           MOVE +10   TO W-LGTH.
           MOVE USRCI TO R-KEY.

           EXEC KICKS DELETE 
              FILE('VMOASEC')
              RIDFLD(R-KEY) KEYLENGTH(W-LGTH)
           END-EXEC.
           GO TO START-OVER.

       HELPD.
           EXEC KICKS LINK PROGRAM('VMOAP50')
              END-EXEC.      
           GO TO LOOP20.

       HELPE.
           EXEC KICKS LINK PROGRAM('VMOAP49')
              END-EXEC.      
           GO TO ADDU.

       HELPSC.
           EXEC KICKS LINK PROGRAM('VMOAP25')
              END-EXEC.      
           GO TO START-OVER.

       HELPSC1.
           EXEC KICKS LINK PROGRAM('VMOAP26')
              END-EXEC.      
           GO TO BRWU.

       ENDPGM.
            EXEC KICKS RETURN END-EXEC.
    

