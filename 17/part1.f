      SUBROUTINE iterate(grid)
        IMPLICIT NONE

        INTEGER, DIMENSION(:,:,:) :: grid
        INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: scratch
        INTEGER :: grid_size
        INTEGER :: x, y, z
        INTEGER :: x1, x2, y1, y2, z1, z2
        INTEGER :: next
        LOGICAL :: active

        grid_size = SIZE(grid, 1)
        ALLOCATE( scratch(grid_size, grid_size, grid_size) )
        scratch = 0

        DO z = 1, grid_size
          DO y = 1, grid_size
            DO x = 1, grid_size
              x1 = MAX(x-1, 1)
              y1 = MAX(y-1, 1)
              z1 = MAX(z-1, 1)
              x2 = MIN(x+1, grid_size)
              y2 = MIN(y+1, grid_size)
              z2 = MIN(z+1, grid_size)
              next = SUM(grid(x1:x2, y1:y2, z1:z2)) - grid(x, y, z)
              IF (grid(x, y, z) .EQ. 1) THEN
                IF (next .EQ. 2 .OR. next .EQ. 3) THEN
                  scratch(x, y, z) = 1
                END IF
              ELSE
                IF (next .EQ. 3) THEN
                  scratch(x, y, z) = 1
                END IF
              END IF
            END DO
          END DO
        END DO
        
        grid = scratch
        DEALLOCATE(scratch)
      END SUBROUTINE

      PROGRAM part1
        IMPLICIT NONE

        INTEGER :: iterations = 6

        INTEGER :: file_size
        INTEGER :: grid_size
        INTEGER :: max_grid_size
        CHARACTER (LEN=:), ALLOCATABLE :: line
        CHARACTER :: char
        INTEGER, DIMENSION (:,:,:), ALLOCATABLE :: grid
        INTEGER :: index, inner
        INTEGER :: midpoint
        INTEGER :: x, y
        INTEGER :: cell

        INTERFACE
          SUBROUTINE iterate(grid)
            INTEGER, DIMENSION(:,:,:) :: grid
          END SUBROUTINE
        END INTERFACE

        INQUIRE(FILE="input", SIZE=file_size)
        ! Find grid size assuming input is a perfect square plus newlines
        DO index = 1, 20
          grid_size = index * (index + 2) - 2
          IF (grid_size .EQ. file_size) THEN
            grid_size = index
            EXIT
          END IF
        END DO
        max_grid_size = grid_size + (iterations * 2)
        midpoint = max_grid_size / 2

        ALLOCATE( grid(max_grid_size, max_grid_size, max_grid_size) )
        ALLOCATE( CHARACTER(grid_size) :: line )

        OPEN(1, FILE="input")
        DO index = 1, grid_size
          y = midpoint - (grid_size / 2) + index
          READ(1, *) line
          DO inner = 1, grid_size
            x = midpoint - (grid_size / 2) + inner
            IF (line(inner:inner) .EQ. '#') THEN
              grid(x, y, midpoint + 1) = 1
            END IF
          END DO
        END DO

        DO index = 1, iterations
          !PRINT *, SUM(grid)
          CALL iterate(grid)
        END DO
        PRINT *, SUM(grid)
      END PROGRAM