USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP30.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  WS-COMMON.
           02 CUSER    PIC X(10).
           02 CPWD     PIC X(10).
           02 CTYPE    PIC X.
	   02 CSYS     PIC X(08).
           02 FILLER   PIC X(51).

       01  REC99.
           05 R-KEY.
              07 R-USR    PIC X(10).
              07 R-ID     PIC X(10).
              07 R-TYPE   PIC X.
              07 R-LIST   PIC X(10).
           05 FILLER      PIC X(49).

       01  W-LGTH      PIC S9(4) COMP.

       01  WK-CT       PIC 99.


       COPY VMOAM30.
       COPY DFHBMSCA.
     
       LINKAGE SECTION.
       01  DFHCOMMAREA    PIC X(80).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA   TO  WS-COMMON.

       START-OVER.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY TO CTIMECO.
           MOVE WS-CURRENT-DATE TO CDATECO.

<NCB2>
           EXAMINE CTIMECO REPLACING ALL ' ' BY ':'.
</NCB2>

           EXEC KICKS
             SEND MAP('MAPC') MAPSET('VMOAM30') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELP31)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPC') MAPSET('VMOAM30')
             END-EXEC.

           IF SELI EQUAL 'B'
              GO TO BRWU.

       LOOP32.
           IF SELI EQUAL 'A'
              GO TO ADDU.
           IF SELI EQUAL 'D'
              GO TO DELU.
           GO TO START-OVER.

       ADDU.
 
           MOVE LOW-VALUES TO MAPEO.
           MOVE TIME-OF-DAY TO CTIMEDO.
           MOVE '  ADD' TO MAPNMEO.
           MOVE WS-CURRENT-DATE TO CDATEEO.

<NCB2>
           EXAMINE CTIMEDO REPLACING ALL ' ' BY ':'.
</NCB2>

 
           EXEC KICKS
              SEND MAP('MAPE') MAPSET('VMOAM30') ERASE
              END-EXEC.

           EXEC KICKS HANDLE AID PF1(START-OVER)
                                 PF3(MAPDWE)
                                 PF10(HELP32)
                                 PF12(ENDPGM) END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPE') MAPSET('VMOAM30')
              END-EXEC.
           GO TO START-OVER.

       MAPDWE.
           MOVE EUSRI     TO R-USR.
           MOVE VNAMEEI   TO R-ID.
           MOVE VTYPEEI   TO R-TYPE.
           MOVE PWDEI     TO R-LIST.
           MOVE +80       TO W-LGTH.

           MOVE REC99      TO PCOMMEO.

           EXEC KICKS HANDLE CONDITION NOTFND(LOOP84) END-EXEC. 
           EXEC KICKS READ
             FILE('VMOADST') 
             INTO(REC99)           
             RIDFLD(R-KEY)
           END-EXEC.
        
       LOOP82.
           MOVE 'RECORD ALREADY ON FILE - REENTER' TO PCOMMEO.
           MOVE REC99 TO PCOM10O.
           GO TO ADDU.

       LOOP84.
           EXEC KICKS WRITE FILE('VMOADST')
                      FROM(REC99)
                      LENGTH(W-LGTH)
                      RIDFLD(R-KEY)
           END-EXEC.
           GO TO START-OVER.

       BRWU.
           MOVE +80        TO W-LGTH.
           MOVE 00         TO WK-CT.
           MOVE LOW-VALUES TO R-KEY.
           EXEC KICKS STARTBR FILE('VMOADST')
               RIDFLD(R-KEY) KEYLENGTH(31)
           END-EXEC.

       LOOP56.
           EXEC KICKS HANDLE CONDITION ENDFILE(LOOP55) END-EXEC.
           EXEC KICKS READNEXT FILE('VMOADST')
               INTO(REC99) LENGTH(W-LGTH)
               RIDFLD(R-KEY) KEYLENGTH(31)
               END-EXEC.
           ADD 1 TO WK-CT.
           IF WK-CT EQUAL 01 THEN
               MOVE R-USR   TO F01UO
               MOVE R-LIST  TO F01LO
               MOVE R-ID    TO F01RO
               MOVE R-TYPE  TO F01TO.
           IF WK-CT EQUAL 02 THEN
               MOVE R-USR   TO F02UO
               MOVE R-LIST  TO F02LO
               MOVE R-ID    TO F02RO
               MOVE R-TYPE  TO F02TO.
           IF WK-CT EQUAL 03 THEN
               MOVE R-USR   TO F03UO
               MOVE R-LIST  TO F03LO
               MOVE R-ID    TO F03RO
               MOVE R-TYPE  TO F03TO.
           IF WK-CT EQUAL 04 THEN
               MOVE R-USR   TO F04UO
               MOVE R-LIST  TO F04LO
               MOVE R-ID    TO F04RO
               MOVE R-TYPE  TO F04TO.
           IF WK-CT EQUAL 05 THEN
               MOVE R-USR   TO F05UO
               MOVE R-LIST  TO F05LO
               MOVE R-ID    TO F05RO
               MOVE R-TYPE  TO F05TO.
           IF WK-CT EQUAL 06 THEN
               MOVE R-USR   TO F06UO
               MOVE R-LIST  TO F06LO
               MOVE R-ID    TO F06RO
               MOVE R-TYPE  TO F06TO.
           IF WK-CT EQUAL 07 THEN
               MOVE R-USR   TO F07UO
               MOVE R-LIST  TO F07LO
               MOVE R-ID    TO F07RO
               MOVE R-TYPE  TO F07TO.
           IF WK-CT EQUAL 08 THEN
               MOVE R-USR   TO F08UO
               MOVE R-LIST  TO F08LO
               MOVE R-ID    TO F08RO
               MOVE R-TYPE  TO F08TO.
           IF WK-CT EQUAL 09 THEN
               MOVE R-USR   TO F09UO
               MOVE R-LIST  TO F09LO
               MOVE R-ID    TO F09RO
               MOVE R-TYPE  TO F09TO.
            GO TO LOOP56.

       LOOP55.
           EXEC KICKS ENDBR 
               FILE('VMOADST')
           END-EXEC.

           MOVE 'BROWSE'  TO MAPNMFO.
           MOVE TIME-OF-DAY TO CTIMEFO.
           MOVE WS-CURRENT-DATE TO CDATEFO.


<NCB2>
           EXAMINE CTIMEFO REPLACING ALL ' ' BY ':'.
</NCB2>


           EXEC KICKS
              SEND MAP('MAPF') MAPSET('VMOAM30') ERASE 
              END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELP33)
                                 PF12(START-OVER)
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPF') MAPSET('VMOAM30')
              END-EXEC.

           GO TO BRWU.



       DELU.
           MOVE LOW-VALUES      TO MAPGO.
           MOVE 'DELETE'        TO MAPNMGO.
           MOVE CURRENT-DATE    TO WS-CURRENT-DATE.
           MOVE TIME-OF-DAY     TO CTIMEGO.
           MOVE WS-CURRENT-DATE TO CDATEGO.
           MOVE CUSER           TO GOWNO.

<NCB2>
           EXAMINE CTIMEGO REPLACING ALL ' ' BY ':'.
</NCB2>


           EXEC KICKS
              SEND MAP('MAPG') MAPSET('VMOAM30') ERASE 
              END-EXEC.

           EXEC KICKS HANDLE AID PF10(HELP34)
                                 PF12(ENDPGM)
                                 END-EXEC.
           EXEC KICKS
              RECEIVE MAP('MAPG') MAPSET('VMOAM30')
              END-EXEC.


           MOVE +31   TO W-LGTH.
      
           EXEC KICKS DELETE 
              FILE('VMOADST')
              RIDFLD(R-KEY) KEYLENGTH(W-LGTH)
           END-EXEC.
           GO TO START-OVER. 

       HELP31.
           EXEC KICKS LINK PROGRAM('VMOAP31') END-EXEC.      
           GO TO START-OVER.

       HELP32.
           EXEC KICKS LINK PROGRAM('VMOAP32') END-EXEC.      
           GO TO START-OVER.

       HELP33.
           EXEC KICKS LINK PROGRAM('VMOAP33') END-EXEC.      
           GO TO BRWU.

       HELP34.
           EXEC KICKS LINK PROGRAM('VMOAP34') END-EXEC.      
           GO TO BRWU.


       ENDPGM.
            EXEC KICKS RETURN END-EXEC.
    

