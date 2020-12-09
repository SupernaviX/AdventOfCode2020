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
           01 row-count PIC 9(5) USAGE IS COMP.
           01 instructions.
               05 row OCCURS 1000 TIMES.
                   10 row-name PIC A(3).
                   10 row-value USAGE IS COMP-2.
                   10 row-seen PIC 9.
                   10 row-region PIC 9(5) USAGE IS COMP.
           01 pc PIC 9(5) USAGE IS COMP.
           01 acc PIC 9(5) USAGE IS COMP.

           01 reg-count PIC 9(5) USAGE IS COMP.
           01 regions.
               05 region OCCURS 1000 TIMES.
                   10 reg-lo PIC 9(5) USAGE IS COMP.
                   10 reg-hi PIC 9(5) USAGE IS COMP.
                   10 reg-seen PIC 9.
                   10 reg-from PIC 9(5) USAGE IS COMP.
                   10 reg-from-table OCCURS 100 TIMES.
                       15 neighbor PIC 9(5) USAGE IS COMP.
                       15 neighbor-bc PIC 9(5) USAGE IS COMP.

           01 frontier-count PIC 9(5) USAGE IS COMP.
           01 frontier-table.
               05 frontier-row OCCURS 100 TIMES.
                   10 frontier PIC 9(5) USAGE IS COMP.

           01 seen-table.
               05 seen-row OCCURS 1000 TIMES.
                   10 seen PIC 9(5) USAGE IS COMP.

           01 offset PIC 9(5) USAGE IS COMP.
           01 target PIC 9(5) USAGE IS COMP.
           01 change PIC 9(5) USAGE IS COMP.

       PROCEDURE DIVISION.
           OPEN INPUT input-file.
           MOVE 0 TO row-count.
           PERFORM UNTIL done-reading
               ADD 1 TO row-count
               READ input-file INTO ws-command
                   AT END MOVE 1 TO state
               END-READ
               MOVE ws-command-name TO row-name(row-count)
               COMPUTE row-value(row-count) =
                   FUNCTION NUMVAL(ws-command-value)
               MOVE 0 to row-seen(row-count)
           END-PERFORM.
           CLOSE input-file.

           MOVE 1 TO pc.
           MOVE 0 TO acc.
           PERFORM RUN-STEP UNTIL row-seen(pc) IS EQUAL TO 1.

           PERFORM INIT-REGIONS.
           MOVE 1 TO pc.
           MOVE 1 TO frontier-count.
           MOVE reg-count TO frontier(frontier-count).
           PERFORM CHECK-FRONTIER UNTIL change IS NOT EQUAL TO 0
               OR frontier-count IS EQUAL TO 0.

           IF change IS NOT EQUAL TO 0 THEN
               DISPLAY "Instruction to change: " change
           ELSE
               STOP RUN
           END-IF.

           IF row-name(change) IS EQUAL TO 'jmp'
               MOVE 'nop' TO row-name(change)
           ELSE
               MOVE 'jmp' TO row-name(change)
           END-IF.

           MOVE 1 TO pc.
           MOVE 0 TO acc.
           PERFORM RUN-STEP UNTIL pc IS GREATER THAN row-count.
           DISPLAY acc.
           STOP RUN.

           RUN-STEP.
           MOVE 1 TO row-seen(pc).
           IF row-name(pc) IS EQUAL TO 'acc' THEN
               ADD row-value(pc) TO acc
           END-IF.
           IF row-name(pc) IS EQUAL TO 'jmp' THEN
               COMPUTE pc = row-value(pc) + pc
           ELSE
               ADD 1 TO pc
           END-IF.

           INIT-REGIONS.
           MOVE 1 TO pc.
           MOVE 1 TO reg-count.
           MOVE 1 TO reg-lo(reg-count).
      * Partition the program up into "regions" to JMP into
           PERFORM UNTIL pc IS GREATER THAN row-count
               MOVE reg-count TO row-region(pc)
               IF row-seen(pc) IS EQUAL TO 1 THEN
                   MOVE 1 TO reg-seen(reg-count)
               END-IF
               IF row-name(pc) IS EQUAL TO 'jmp' THEN
                   MOVE pc TO reg-hi(reg-count)
                   ADD 1 TO reg-count
                   COMPUTE reg-lo(reg-count) = pc + 1
               END-IF
               ADD 1 TO pc
           END-PERFORM.
           SUBTRACT 2 FROM reg-count.
           MOVE 1 TO pc.
           PERFORM UNTIL pc IS GREATER THAN row-count
               IF row-name(pc) IS NOT EQUAL TO 'acc' THEN
                   COMPUTE offset = pc + row-value(pc)
                   MOVE row-region(offset) TO target
                   IF neighbor(target, reg-from(target))
                              IS NOT EQUAL TO row-region(pc) THEN
                       ADD 1 TO reg-from(target)
                       MOVE row-region(pc)
                           TO neighbor(target, reg-from(target))
                       MOVE pc TO neighbor-bc(target, reg-from(target))
                   END-IF
               END-IF
               ADD 1 TO pc
           END-PERFORM.

           CHECK-FRONTIER.
           MOVE frontier(frontier-count) TO target.
           SUBTRACT 1 FROM frontier-count.
           IF reg-seen(target - 1) IS EQUAL TO 1 THEN
      * We visited the region before this, so change its ending JMP 
               COMPUTE change = reg-lo(target) - 1
           ELSE
               MOVE 1 TO pc
      * Add everything that could JMP here to the frontier
               PERFORM UNTIL pc IS GREATER THAN reg-from(target)
      * If we visited one of the instructions that COULD JMP here,
      * it must be a NOP which we can switch to a JMP.
                   IF row-seen(neighbor-bc(target, pc))
                           IS NOT EQUAL TO 0 THEN
                       MOVE neighbor-bc(target, pc) TO change
                   END-IF
                   IF seen(neighbor(target, pc)) IS EQUAL TO 0 THEN
                       ADD 1 TO frontier-count
                       MOVE neighbor(target, pc)
                           TO frontier(frontier-count)
                       MOVE 1 TO seen(neighbor(target, pc))
                   END-IF
                   ADD 1 TO pc
               END-PERFORM
           END-IF.
