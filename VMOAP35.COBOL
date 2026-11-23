USERID KICKS
       ID DIVISION.
       PROGRAM-ID.     VMOAP35.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR  PIC 9(04).
           05  WS-CURRENT-MONTH PIC 9(02).
           05  WS-CURRENT-DAY   PIC 9(02).

       01  WKSIZE        PIC 9(3)    VALUE 30.
       01  EXMODE        PIC 9       VALUE 1.
       01  RCT           PIC 9(5)    VALUE 0.
       01  RCT1          PIC 9(2)    VALUE 0.
       01  RCT2          PIC 9(2)    VALUE 0.
       01  CCT           PIC 9(5)    VALUE 1.
       01  DROW          PIC XX      VALUE SPACES.
       01  II            PIC 99      VALUE 0.
       01  J             PIC 99      VALUE 0.
       01  KK            PIC 99      VALUE 0.
       01  K3            PIC 99      VALUE 0.

       01  FLAG-TABLE.
           05  FLAG      PIC 9(6)  OCCURS 810 TIMES.

       01  SS-TABLE.
           05  SSH       PIC 9(12) OCCURS 810 TIMES.

       01  SHEET-TABLE.
           05  SHEET     PIC X(20) OCCURS 810 TIMES.

       01  SRC-TABLE.
           05  SRC       PIC X(20) OCCURS 810 TIMES.
 
       COPY VMOAM35.
       COPY DFHBMSCA.

       PROCEDURE DIVISION.

       START-OVER.
           
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO SDATEO.
           MOVE TIME-OF-DAY TO STIMEO.
<NCB2>
           EXAMINE STIMEO REPLACING ALL ' ' BY ':'.
</NCB2>

           MOVE 'CELL'  TO  WSHEETO.
           MOVE 'EMPTY' TO  WSPNMO.
           GO TO SCR-OUT.

       LOOP1.


           GO TO START-OVER.

       SCR-OUT.

           IF EXMODE EQUAL 1 THEN
                 MOVE 'CELL'  TO  WSHEETO.
           IF EXMODE EQUAL 2 THEN
                 MOVE 'PROG'  TO  WSHEETO.

           IF RCT EQUAL 0 THEN
                 MOVE +1      TO  RCT.
       

           MOVE 'A'     TO HD1O.
           MOVE 'B'     TO HD2O.
           MOVE 'C'     TO HD3O.
           MOVE 'D'     TO HD4O.
           
           MOVE RCT       TO RCT1.
           MOVE RCT1      TO RD1O.
           COMPUTE RCT2 = RCT1 + 1.
           MOVE RCT2      TO RD2O.
           COMPUTE RCT2 = RCT1 + 2.
           MOVE RCT2      TO RD3O.
           COMPUTE RCT2 = RCT1 + 3.
           MOVE RCT2      TO RD4O.
           COMPUTE RCT2 = RCT1 + 4.
           MOVE RCT2      TO RD5O.
           COMPUTE RCT2 = RCT1 + 5.
           MOVE RCT2      TO RD6O.
           COMPUTE RCT2 = RCT1 + 6.
           MOVE RCT2      TO RD7O.
           COMPUTE RCT2 = RCT1 + 7.
           MOVE RCT2      TO RD8O.
           COMPUTE RCT2 = RCT1 + 8.
           MOVE RCT2      TO RD9O.
           COMPUTE RCT2 = RCT1 + 9.
           MOVE RCT2      TO RD10O.
           COMPUTE RCT2 = RCT1 + 10.
           MOVE RCT2      TO RD11O.
           COMPUTE RCT2 = RCT1 + 11.
           MOVE RCT2      TO RD12O.
           COMPUTE RCT2 = RCT1 + 12.
           MOVE RCT2      TO RD13O.
           COMPUTE RCT2 = RCT1 + 13.
           MOVE RCT2      TO RD14O.
           COMPUTE RCT2 = RCT1 + 14.
           MOVE RCT2      TO RD15O.


           EXEC KICKS
             SEND MAP('MAPA') MAPSET('VMOAM35') ERASE
             END-EXEC.

           EXEC KICKS HANDLE AID PF12(ENDPGM) END-EXEC.
           EXEC KICKS
             RECEIVE MAP('MAPA') MAPSET('VMOAM35')
             END-EXEC.
    
           GO TO LOOP1.

       

       ENDPGM.
           EXEC KICKS XCTL PROGRAM('KSSFPGM') NOHANDLE END-EXEC. 

