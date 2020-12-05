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
  FOR %%G IN (byr iyr eyr hgt hcl ecl pid) DO (
    IF NOT EXIST tmp/%%G (
      GOTO :done
    )
  )

  :: birth year
  SET /P byr=<tmp/byr
  IF !byr! LSS 1920 (
    GOTO :done
  )
  IF !byr! GTR 2002 (
    GOTO :done
  )

  :: issue year
  SET /P iyr=<tmp/iyr
  IF !iyr! LSS 2010 (
    GOTO :done
  )
  IF !iyr! GTR 2020 (
    GOTO :done
  )

  :: expiration year
  SET /P eyr=<tmp/eyr
  IF !eyr! LSS 2020 (
    GOTO :done
  )
  IF !eyr! GTR 2030 (
    GOTO :done
  )

  :: height
  SET /P hgt=<tmp/hgt
  IF !hgt:~3! == cm (
    SET hgtcm=!hgt:~0,3!
    IF !hgtcm! LSS 150 (
      GOTO :done
    )
    IF !hgtcm! GTR 193 (
      GOTO :done
    )
  ) ELSE IF !hgt:~2! == in (
    SET hgtin=!hgt:~0,2!
    IF !hgtin! LSS 59 (
      GOTO :done
    )
    IF !hgtin! GTR 76 (
      GOTO :done
    )
  ) ELSE (
    GOTO :done
  )

  :: hair color
  FOR /F "USEBACKQ" %%G IN (`findstr /x #[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f] tmp\\hcl`) DO (
    GOTO :goodhair
  )
  GOTO :done
:goodhair

  :: eye color
  SET /P ecl=<tmp/ecl
  FOR %%G IN (amb blu brn gry grn hzl oth) DO (
    IF %%G == !ecl! (
      GOTO :goodeyes
    )
  )
  GOTO :done
:goodeyes

  :: passport
  FOR /F "USEBACKQ" %%G IN (`findstr /x [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9] tmp\\pid`) DO (
    GOTO :goodpassport
  )
  GOTO :done
:goodpassport

  SET /A valid+=1
:done
  RMDIR /S /Q tmp
  MKDIR tmp
  GOTO :EOF

