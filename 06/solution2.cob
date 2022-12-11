           IDENTIFICATION DIVISION.
           PROGRAM-ID. AOC-06-2.
           AUTHOR. INIAS PEETERS.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INPUTFILE ASSIGN TO "input"
               ORGANIZATION IS LINE SEQUENTIAL.

           DATA DIVISION.
           FILE SECTION.
             FD INPUTFILE
             RECORD IS VARYING IN SIZE FROM 1 to 5000
             DEPENDING ON REC-LEN.
             01 INPUTRECORD PIC A(5000).
           WORKING-STORAGE SECTION.
             01 FILE-STATUS PIC 9 VALUE 0.
             01 REC-LEN PIC 9(5) COMP.
             01 WS-WINDOW-SIZE PIC 9(2) VALUE 14.
             01 WS-WINDOW PIC A(14).
             01 WS-INDEX PIC 9(4) VALUE 1.
             01 WS-INDEX-IN-WINDOW PIC 9(2).
             01 WS-DUP-IN-WINDOW PIC 9(1) VALUE 0.
             01 WS-COUNT PIC 9(1).
             01 WS-CHAR-TO-SEARCH PIC A(1).

           PROCEDURE DIVISION.
           001-MAIN.
               OPEN INPUT INPUTFILE.
               PERFORM 002-READ.
               CLOSE INPUTFILE.
               ADD WS-WINDOW-SIZE TO WS-INDEX.
               SUBTRACT 1 FROM WS-INDEX.
               DISPLAY WS-INDEX.
               STOP RUN.

           002-READ.
               READ INPUTFILE
               AT END MOVE 1 TO FILE-STATUS
               NOT AT END PERFORM 003-PROCESS-RECORD
               END-READ.

           003-PROCESS-RECORD.
               IF REC-LEN > 0 THEN
                  PERFORM 004-FIND-MARKER
               END-IF.

           004-FIND-MARKER.
               PERFORM FOREVER
                   MOVE INPUTRECORD (WS-INDEX:WS-WINDOW-SIZE)
                       TO WS-WINDOW
                   PERFORM 005-TREAT-WINDOW
                   IF WS-DUP-IN-WINDOW NOT = 0 THEN
                       MOVE 0 TO WS-DUP-IN-WINDOW
                   ELSE
                       EXIT PERFORM
                   END-IF
                   ADD 1 TO WS-INDEX
               END-PERFORM.

           005-TREAT-WINDOW.
               PERFORM 006-COUNT-IN-WINDOW
                   VARYING WS-INDEX-IN-WINDOW
                   FROM 1 BY 1
                   UNTIL WS-INDEX-IN-WINDOW > WS-WINDOW-SIZE.

           006-COUNT-IN-WINDOW.
               MOVE 0 TO WS-COUNT.
               MOVE WS-WINDOW(WS-INDEX-IN-WINDOW:1)
                   TO WS-CHAR-TO-SEARCH.
               INSPECT WS-WINDOW
                   TALLYING WS-COUNT
                   FOR ALL WS-CHAR-TO-SEARCH.
               IF WS-COUNT > 1 THEN
                   MOVE 1 TO WS-DUP-IN-WINDOW
               END-IF.
