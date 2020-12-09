       IDENTIFICATION DIVISION.
       PROGRAM-ID. part1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT input-file ASSIGN TO 'input'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD input-file.
           01 command.
               05 command-name PIC A(3).
               05 blank-space PIC X.
               05 command-value PIC 9(5).

           WORKING-STORAGE SECTION.
           01 ws-command.
               05 ws-command-name PIC A(3).
               05 blank-space PIC X.
               05 ws-command-value PIC 9(5).
           01 state PIC 9.
               88 done-reading value 1.
               88 solved       value 2.
           01 eof PIC 9.
           01 pc PIC 9(5) USAGE IS COMP.
           01 instructions.
               05 row OCCURS 1000 TIMES.
                   10 row-name PIC A(3).
                   10 row-value USAGE IS COMP-2.
                   10 row-seen PIC 9.
           01 acc PIC 9(5) USAGE IS COMP.

       PROCEDURE DIVISION.
           OPEN INPUT input-file.
           MOVE 1 TO pc.
           PERFORM UNTIL done-reading
               READ input-file INTO ws-command
                   AT END MOVE 1 TO state
               END-READ
               MOVE ws-command-name TO row-name(pc)
               COMPUTE row-value(pc) = FUNCTION NUMVAL(ws-command-value)
               MOVE 0 to row-seen(pc)
               ADD 1 TO pc
           END-PERFORM.
           CLOSE input-file.

           MOVE 1 TO pc.
           PERFORM UNTIL row-seen(pc)
               MOVE 1 TO row-seen(pc)
               IF row-name(pc) IS EQUAL TO 'acc' THEN
                   ADD row-value(pc) TO acc
               END-IF
               IF row-name(pc) IS EQUAL TO 'jmp' THEN
                   COMPUTE pc = row-value(pc) + pc
               ELSE
                   ADD 1 TO pc
               END-IF
           END-PERFORM.
           DISPLAY acc.
              STOP RUN.
