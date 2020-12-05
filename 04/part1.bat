@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

SET /A linenumber=0
SET /A valid=0
MKDIR tmp
FOR /F "USEBACKQ DELIMS=" %%G IN (`findstr /n /r . input`) DO (
  SET LINE=%%G
  >line.txt ECHO !LINE!
  CALL :process-line
  DEL line.txt
)
CALL :complete-record
ECHO valid passports: !valid!
RMDIR /S /Q tmp
GOTO :EOF

:process-line
  SET /P line=<line.txt
  FOR /F "DELIMS=:" %%G IN ("%line%") DO (
    SET /A currline=%%G
  )
  SET /A linenumber+=1
  IF NOT %currline% == %linenumber% (
    SET /A linenumber=%currline%
    CALL :complete-record
  )
  IF !currline! LSS 10 (
    SET tokens=%line:~2%
  ) ELSE IF !currline! LSS 100 (
    SET tokens=%line:~3%
  ) ELSE IF !currline! LSS 1000 (
    SET tokens=%line:~4%
  ) ELSE (
    SET tokens=%line:~5%
  )
  >tokens.txt ECHO %tokens%
  CALL :extract-values
  DEL tokens.txt
  GOTO :EOF

:extract-values
  SET /P tokens=<tokens.txt
  FOR %%G IN (%tokens%) DO (
    SET token=%%G
    SET key=!token:~0,3!
    SET val=!token:~4!
    >tmp/!key! ECHO !val!
  )
  GOTO :EOF

:complete-record
  SET found=
  FOR %%G IN (byr iyr eyr hgt hcl ecl pid) DO (
    IF EXIST tmp/%%G (
      set "found=!found!_%%G"
    )
  )
  IF !found! == _byr_iyr_eyr_hgt_hcl_ecl_pid (
    SET /A valid+=1
  )
  RMDIR /S /Q tmp
  MKDIR tmp
  GOTO :EOF