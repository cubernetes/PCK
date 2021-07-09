::bat

:: PCK Package Manager for Windows ::
:: PCK is a minimalist package manager for Windows. It extremely quick to use and it is very easy to add new packges to the list decentral list. 

:: # Usage
:: pck package         # installs a package
:: pck i package       # shows info for a package
:: pck r package       # uninstalls (removes) a package
:: pck u package link  # updates a package's link
:: pck a package link  # adds a package to list
:: pck list            # lists all packages
:: pck list "regex"    # lists all packages that match the regex

@ECHO OFF
SETLOCAL EnableDelayedExpansion EnableExtensions
CALL :Init

CALL :Main %0 %*

ENDLOCAL & SET "PATH=%PATH%"
EXIT /B 0

REM ------------------------ Main ------------------------
:Main
SETLOCAL
	SET "Arg1=%~2"
	SET "Arg2=%~3"
	SET "Arg3=%~4"
	SET "Arg4=%~5"
	%$ToLower% Arg1
	%$ToLower% Arg2
	%$ToLower% Arg3
	%$ToLower% Arg4

	REM --------------------------- List packages ---------------------------
	IF "!Arg1!"=="help" (
		CALL :WarnAboutIgnoredArgs 2 %*
		CALL :ShowHelp
		GOTO :Finish
	)

	IF "!Arg1!"=="list" (
		CALL :WarnAboutIgnoredArgs 3 %*
		IF DEFINED Arg2 (
			IF "!Arg2:~0,1!"=="-" (
				CALL :ColorEcho ERROR def "Your regex starts with a minus ("-"). Please escape it using a backslash ("\")." 1 0
				CALL :ColorEcho INFO def "The reason for this error is unclear to me, but you know, it's batch." 1 0
				GOTO :Error
			)
			CALL :ColorEcho INFO def "Showing all available packages that match the regex `!Arg2!`." 1 1
			FOR /F "TOKENS=1,2 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!"  ^| !Findstr! /I /R "^^[!AlnumCharClass!]*!Arg2![!AlnumCharClass!]*;" ^| !Sort!') DO @(
				IF NOT EXIST "!BaseDir!\%%~A%%~B" (
					ECHO - %%~A
				) ELSE (
					ECHO %ESC%[32m- %%~A [already installed]%ESC%[0m
				)
			)
		) ELSE (
			CALL :ColorEcho INFO def "Showing all available packages." 1 1
			FOR /F "TOKENS=1,2 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| !Sort!') DO (
				IF NOT EXIST "!BaseDir!\%%~A%%~B" (
					ECHO - %%~A
				) ELSE (
					ECHO %ESC%[32m- %%~A [already installed]%ESC%[0m
				)
			)
		)
		GOTO :Finish
	)

	IF "!Arg1!"=="i" (
		CALL :WarnAboutIgnoredArgs 3 %*
		CALL :ShowInformation "%2"
		GOTO :Finish
	)

	IF "!Arg1!"=="r" (
		CALL :WarnAboutIgnoredArgs 3 %*
		CALL :ColorEcho INFO def "This functionality will uninstall a package." 1 1
		GOTO :Finish
	)

	IF "!Arg1!"=="u" (
		CALL :WarnAboutIgnoredArgs 4 %*
		CALL :ColorEcho INFO def "This functionality will update a package's link if possible." 1 1
		GOTO :Finish
	)

	IF "!Arg1!"=="a" (
		CALL :WarnAboutIgnoredArgs 4 %*
		CALL :ColorEcho INFO def "This functionality will add a package to the list if possible." 1 1
		GOTO :Finish
	)

	REM --------------------------- Show Usage ---------------------------
	IF NOT DEFINED Arg1 (
		CALL :ShowHelp
		GOTO :Finish
	)


	CALL :WarnAboutIgnoredArgs 1 %*
	REM --------------------------- Install package ---------------------------
	FOR /F "TOKENS=1,2,3,4,5 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| !Sort!') DO (

		IF "%%~A"=="!Arg1!" (

			SET "Name=%%~A"
			SET "RelPath=%%~B"
			SET "URL=%%~C"
			SET "URL32bit=%%~D"

			IF NOT EXIST "!BaseDir!\!Name!!RelPath!" (

				CALL :ColorEcho INFO def "Package "!Arg1!" found!" 1 1

				CALL :FollowURL URL LastRedirectURL
				CALL :FollowURL URL32bit LastRedirectURL32bit

				CALL :DetermineFileExtensionOfURL LastRedirectURL FileExtension
				CALL :DetermineFileExtensionOfURL LastRedirectURL32bit FileExtension32bit

				REM a => Architecture is 64 bit
				REM b => File type for 64 bit version is not unknown and not undetermined
				REM c => File type for 32 bit version is not unknown and not undetermined
				REM
				REM Truth table:
				REM a b c Error    Use b instead of c
				REM 0 0 0     1                     0
				REM 0 0 1     0                     0
				REM 0 1 0     1                     0
				REM 0 1 1     0                     0
				REM 1 0 0     1                     0
				REM 1 0 1     0                     0
				REM 1 1 0     0                     1
				REM 1 1 1     0                     1
				REM To get a formular for the Error variable, we can use a disjunctive normal form:
				REM Error = (¬a ∧ ¬b ∧ ¬c) ∨ (¬a ∧ b ∧ ¬c) ∨ (a ∧ ¬b ∧ ¬c) <=>
				REM Error = (¬a ∧ ¬c) ∨ (¬b ∧ ¬c)
				REM With:
				REM   ∧ = AND
				REM   ∨ = OR
				REM   ¬ = NOT
				REM In batch:
				REM SET /A "Error=(!a & !b) | (!b & !c)"
				REM
				REM Forumular for when to use the 64 bit version:
				REM SET /A "CanUse64Bit=a & b"

				SET "_ErrorString=unknown undetermined"

				SET a=0
				SET b=0
				SET c=0
				IF "!Architecture!"=="64" SET "a=1"
				CALL CALL SET "TmpString=%%%%_ErrorString:%%FileExtension%%=%%%%"
				CALL CALL SET "TmpString32bit=%%%%_ErrorString:%%FileExtension32bit%%=%%%%"
				IF "!TmpString!"=="!_ErrorString!" SET "b=1"
				IF "!TmpString32bit!"=="!_ErrorString!" SET "c=1"

				REM Escaping exclamation points because of DelayedExpansion
				SET /A "Error=(^!a & ^!c)|(^!b & ^!c)"

				IF "!Error!"=="1" (
					CALL :ColorEcho ERROR def "There was no compatible version found for your machine." 1 0
					CMD /C ^""!DifferentCmdLine!" i "!Name!"^"
					GOTO :Error
				)

				SET /A "CanUse64Bit=a & b"

				IF "!CanUse64Bit!"=="1" (
					CALL :ColorEcho INFO def "64 bit version of "!Arg1!" will be downloaded." 1 0
					CALL :Download LastRedirectURL "!TmpDir!\!Name!.!FileExtension!"
					SET "Extension=!FileExtension!"
				) ELSE (
					CALL :ColorEcho INFO def "32 bit version of "!Arg1!" will be downloaded." 1 0
					CALL :Download LastRedirectURL32bit "!TmpDir!\!Name!.!FileExtension32bit!"
					SET "Extension=!FileExtension32bit!"
				)

				CALL :CreateFolder "!BaseDir!\!Name!"

				IF "!Extension!"=="zip" (
					CALL :Unzip "!TmpDir!\!Name!.zip" "!BaseDir!\!Name!"
				)
				IF "!Extension!"=="7z" (
					CALL :Extract7z "!TmpDir!\!Name!.7z" "!BaseDir!\!Name!"
				)
				IF "!Extension!"=="exe" (
					CALL :CheckExeExtractable "!TmpDir!\!Name!.exe" isExtractable
					IF "!IsExtractable!"=="1" (
						CALL :ExtractExe "!TmpDir!\!Name!.zip" "!BaseDir!\!Name!"
					) ELSE (
						CALL :CheckExeInstaller "!TmpDir!\!Name!.exe" isInstaller
						IF NOT "!IsInstaller!"=="1" (
							CALL :InstallExe "!TmpDir!\!Name!.zip" "!BaseDir!\!Name!"
						) ELSE (
							MOVE "!TmpDir!\!Name!.exe" "!BaseDir!\!Name!"
						)
					)
				)
				IF "!Extension!"=="msi" (
					CALL :ExtractMsi "!TmpDir!\!Name!.msi" "!BaseDir!\!Name!"
					REM Since you can always extract any MSI with windows built-in msiexec-utility, there's no need
					REM to install any msi. Option to actually install it might be added later
					REM CALL :InstallMsi "!TmpDir!\!Name!.msi" "!BaseDir!\!Name!"
				)
				IF "!Extension!"=="bat" (
					MOVE "!TmpDir!\!Name!.bat" "!BaseDir!\!Name!"
				)
				IF "!Extension!"=="ahk" (
					MOVE "!TmpDir!\!Name!.ahk" "!BaseDir!\!Name!"
				)

				CALL :CreateShortcutWithVbs "!BaseDir!\!Name!!RelPath!" "!RedirectsDir!\!Name!.lnk"

				GOTO :Finish
			) ELSE (
				CALL :ColorEcho INFO def "!Name! is already installed in "!BaseDir!\!Name!". Use it with "!BaseDir!\!Name!!RelPath!"."
				GOTO :Finish
			)
		)
	)
	CALL :ColorEcho ERROR def "That package does not exist" 1 0
	GOTO :Error

	:Finish
	CALL :Cleanup "%~0"
ENDLOCAL
EXIT /B 0

REM ------------------------ Init ------------------------
:Init
	SET "System32=!SystemRoot!\System32"

	REM Source: https://stackoverflow.com/a/59874436
	REM Create escape character for ANSI escape sequences.
	REM Only line of code where the part after DO can not be in paranthesis.
	FOR /F "DELIMS=#" %%E IN ('"PROMPT #$E# & FOR %%E IN (1) DO REM"') DO (SET "ESC=%%E")

	CALL :GetProgramPath where "!System32!" Where
	CALL :GetProgramPath chcp "!System32!" Chcp
	CALL :GetProgramPath sort "!System32!" Sort
	CALL :GetProgramPath findstr "!System32!" Findstr
	CALL :GetProgramPath certutil "!System32!" Certutil
	CALL :GetProgramPath cscript "!System32!" Cscript

	FOR /F "TOKENS=3 DELIMS=. " %%A IN ('^"!Chcp!^"') DO (SET "OldCodePage=%%A")

	1>NUL CHCP 1252
	CALL :GetProgramPath powershell "!System32!\WindowsPowerShell\v1.0" Powershell
	1>NUL !Chcp! 65001

	REM Remove trailing backslash "\"
	REM Source: https://stackoverflow.com/a/60414485/13823467
	FOR %%A IN ("%~dp0\..") DO (SET "BaseDir=%%~fA")
	FOR %%A IN ("%~dp0\.") DO (SET "PckDir=%%~fA")

	SET "RedirectsDir=!BaseDir!\Redirects"
	SET "TmpDir=!BaseDir!\tmp"

	SET "PackagesFilePath=!PckDir!\packages.csv"

	SET "Space= "
	SET "DifferentCmdLine=!PckDir!\.\%~nx0"

	IF NOT EXIST "!TmpDir!\REPLVAR.BAT" (
		1>NUL !Certutil! -decode "%~f0" "!TmpDir!\REPLVAR.BAT"
	)

	SET "Verbose=1"

	CALL :UpdatePath
	CALL :UpdatePathExt

	IF DEFINED PROGRAMFILES(X86) (
	    SET "Architecture=64"
	) ELSE (
	    SET "Architecture=32"
	)

	REM Create carriage return character for generating live download info.
	SET "CR=" & IF NOT DEFINED CR FOR /F "SKIP=1" %%C IN ('ECHO(^|REPLACE ? . /W /U') DO (SET "CR=%%C")

SET LF=^



	REM Needed for live download info.
	SET "WhiteSpaceBuffer=                                      "

	REM For regex
	SET "AlnumCharClass=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

	CALL :CreateFolder "!RedirectsDir!"
	CALL :CreateFolder "!TmpDir!"

	ECHO START "" "!BaseDir!">"!RedirectsDir!\spck.cmd"

	REM Curl is needed for various tasks like fetching information for the packages,
	REM determining the file sizes or determining the file types/extensions.
	CALL :ProgramWorks "curl"
	IF NOT "!ERRORLEVEL!"=="0" CALL :DownloadDependency curl

	CALL :DefineMacros

	REM For future releases, might never be implemented, since downloading files with VBScript is way more bulletproof than with bitsadmin.
	REM Commented out for now because it takes long sometimes.
	REM 1>NUL 2>&1 bitsadmin.exe /complete downloadFile
EXIT /B 0

REM ------------------------ DefineMacros ------------------------
:DefineMacros
REM "  <- This construct is just for better syntax highlighting for Sublime Text
REM Must be enabled.
SETLOCAL EnableDelayedExpansion
SET LF=^


REM Above 2 lines required - do not remove
SET ^"\n=^^^%LF%%LF%^%LF%%LF%^^"
REM "

REM Modified to work with Delayed Expansion enabled or disabled. For macro definition, it must be enabled.
ENDLOCAL & FOR /F %%! IN ("! ! ^^^!") DO (REM "
REM --------------------- Split Macro ---------------------
SET $Split=FOR %%I IN ^(1 2^) DO IF %%I==2 ^(%\n%
	FOR /F "TOKENS=1,2" %%J IN ^("%%!MacroArgs%%!"^) DO ^(%\n%
		SET "String=%%!%%J%%!"%\n%
		SET "String=%%!String:%%K=^^^%%LF%%%%LF%%%%!"%\n%
		SET "i=0"%\n%
		SET ReturnString=ENDLOCAL%\n%
		FOR /F "DELIMS=" %%A IN ^(%\n%
			'ECHO %%!String%%!'^) DO ^(%\n%
			SET "Line=%%A"%\n%
			IF DEFINED Line ^(%\n%
				IF NOT DEFINED NotDelayed ^(%\n%
					SET "Line=%%!Line:^^^=^^^^%%!"%\n%
					SET "Line=%%!Line:"=""Q%%!^"%\n%
					CALL SET "Line=%%^^^Line:^^^!=""E^^^!%%" %%! %\n%
					SET "Line=%%!Line:""E=^^^!"%\n%
					SET "Line=%%!Line:""Q="%%!^"%\n%
				^)%\n%
				FOR /F ^^^^^"EOL^^^^^=^^^^^%LF%%LF%^%LF%%LF%^^^^ TOKENS^^^^=^^^^1*^^^^ DELIMS^^^^=\^^^^^" %%k IN ^("%%!i%%!\%%!Line%%!"^) DO ENDLOCAL^&ENDLOCAL^&SET "%%J[%%~k]=%%~l"%%!%\n%
)	ELSE ENDLOCAL^&ENDLOCAL^&SET "%%J[%%!i%%!]="%\n%
SET /A "i+=1"%\n%
SETLOCAL^&SETLOCAL EnableDelayedExpansion^)%\n%
	^)%\n%
) ELSE SETLOCAL^&SET "NotDelayed=%%!"^&SETLOCAL EnableDelayedExpansion^&SET MacroArgs=
REM "

REM --------------------- Trim Macro ---------------------
REM %$Trim% strVar [charVar] -- macro for trimming both left and right
REM Source: https://www.dostips.com/forum/viewtopic.php?t=2697
REM Is set to only strip 3 chars at max
SET $Trim=FOR %%I IN ^(1 2^) DO IF %%I==2 ^(%\n%
  SET "TrimChar= "%\n%
  FOR /F "TOKENS=1,2" %%J IN ^("%%!MacroArgs%%!"^) DO ^(%\n%
    SET "String=%%!%%J%%!"%\n%
    IF "%%~K" NEQ "" IF DEFINED %%~K SET "TrimChar=%%!%%K:~0,1%%!"%\n%
    FOR /L %%i IN ^(1 1 2^) DO SET "TrimChar=%%!TrimChar%%!%%!TrimChar%%!"%\n%
    SET /A "k=2"%\n%
    FOR /L %%j IN ^(1 1 2^) DO ^(%\n%
      IF DEFINED String FOR %%k IN ^(%%!k%%!^) DO ^(%\n%
        IF "%%!String:~-%%k%%!"=="%%!TrimChar:~-%%k%%!" SET "String=%%!String:~0,-%%k%%!"%\n%
        IF "%%!String:~0,%%k%%!"=="%%!TrimChar:~-%%k%%!" SET "String=%%!String:~%%k%%!"%\n%
        SET /A "k/=2"%\n%
      ^)%\n%
    ^)%\n%
    IF DEFINED String ^(%\n%
      IF NOT DEFINED NotDelayed ^(%\n%
        SET "String=%%!String:^^^=^^^^%%!"%\n%
        SET "String=%%!String:"=""Q%%!^"%\n%
        CALL SET "String=%%^^^String:^^^!=""E^^^!%%" %%! %\n%
        SET "String=%%!String:""E=^^^!"%\n%
        SET "String=%%!String:""Q="%%!^"%\n%
      ^)%\n%
      FOR /F ^^^^^"EOL^^^^^=^^^^^%LF%%LF%^%LF%%LF%^^^^ DELIMS^^^^=^^^^^" %%k IN ^("%%!String%%!"^) DO ENDLOCAL^&ENDLOCAL^&SET "%%J=%%k"%%!%\n%
)	ELSE ENDLOCAL^&ENDLOCAL^&SET "%%J="%\n%
  ^)%\n%
) ELSE SETLOCAL^&SET "NotDelayed=%%!"^&SETLOCAL EnableDelayedExpansion^&SET MacroArgs=
REM "

REM ----------------------- ToLower Macro -------------------------
REM Source: https://www.dostips.com/forum/viewtopic.php?p=8697#p8697
SET $ToLower=FOR %%I IN ^(1 2^) DO IF %%I==2 ^(%\n%
	SET "TrimChar= "%\n%
	FOR /F %%J IN ^("%%!MacroArgs%%!"^) DO ^(%\n%
		SET "String=%%!%%J%%! "%\n%
		FOR %%A IN ^(%\n%
			"A=a" "B=b" "C=c" "D=d" "E=e" "F=f" "G=g" "H=h" "I=i" "J=j"%\n%
			"K=k" "L=l" "M=m" "N=n" "O=o" "P=p" "Q=q" "R=r" "S=s" "T=t"%\n%
			"U=u" "V=v" "W=w" "X=x" "Y=y" "Z=z" "Ö=ö" "Ä=ä" "Ü=ü"%\n%
		^) DO ^(SET "String=%%!String:%%~A%%!"^)%\n%
		SET "String=%%!String:~,-1%%!"%\n%
		IF DEFINED String ^(%\n%
			IF NOT DEFINED NotDelayed ^(%\n%
				SET "String=%%!String:^^^=^^^^%%!"%\n%
				SET "String=%%!String:"=""Q%%!^"%\n%
				CALL SET "String=%%^^^String:^^^!=""E^^^!%%" %%! %\n%
				SET "String=%%!String:""E=^^^!"%\n%
				SET "String=%%!String:""Q="%%!^"%\n%
			^)%\n%
			FOR /F ^^^^^"EOL^^^^^=^^^^^%LF%%LF%^%LF%%LF%^^^^ DELIMS^^^^=^^^^^" %%k IN ^("%%!String%%!"^) DO ENDLOCAL^&ENDLOCAL^&SET "%%J=%%k"%%!%\n%
)	ELSE ENDLOCAL^&ENDLOCAL^&SET "%%J="%\n%
	^)%\n%
) ELSE SETLOCAL^&SET "NotDelayed=%%!"^&SETLOCAL EnableDelayedExpansion^&SET MacroArgs=
)
REM "
EXIT /B 0

REM ------------------------ ColorEcho ------------------------
:ColorEcho
SETLOCAL
	REM Needed to print excalamation points.
	SETLOCAL DisableDelayedExpansion
	SET "Type=[%~1] "
	SET "Clr=%~2"
	SET "Message=%~3 "
	SET "AddNewline=%~4"
	SET "ComplyWithVerboseSetting=%~5"


	1>NUL SET /A "Check=(!ComplyWithVerboseSetting) | Verbose"
	REM /!\ With logical operators: NOT verbose  OR  Verbose
	REM  !  Which is equivalent to: verbose -> Verbose
	REM  !  Which means the only way this function will NOT echo anything is, when "Verbose" is 0 [meaning verbose output will be suppressed]
	REM  !  and "verbose" is 1, meaning the following echo statements comply with the aforementioned "Verbose" setting. In any other case, it will be echoed.

	SETLOCAL EnableDelayedExpansion EnableExtensions
	SET "Style=0"
	REM ANSI escape codes, see https://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html
	IF "!Clr!"=="black" SET "Style=30"
	IF "!Clr!"=="red" SET "Style=31;1"
	IF "!Clr!"=="green" SET "Style=32"
	IF "!Clr!"=="yellow" SET "Style=33"
	IF "!Clr!"=="blue" SET "Style=34;1"
	IF "!Clr!"=="magenta" SET "Style=35"
	IF "!Clr!"=="white" SET "Style=37"
	IF "!Clr!"=="cyan" SET "Style=36"
	IF "!Clr!"=="orange" SET "Style=38;5;208"
	IF "!Clr!"=="def" (
		IF "!Type!"=="[ERROR] " SET "Style=31;1"
		IF "!Type!"=="[ACTION] " SET "Style=34;1"
		IF "!Type!"=="[INFO] " SET "Style=33"
		IF "!Type!"=="[WARNING] " SET "Style=38;5;208"
		IF "!Type!"=="[SUCCESS] " SET "Style=32"
		IF "!Type!"=="[FAIL] " SET "Style=31;1"
	)
	IF "!Type!"=="[] " SET "Type="

	SET "MakeUnsafe=0"
	FOR %%A IN ("\x20" "\x3b" "\x2c" "\x09" "\x3d" "\x22") DO (
		IF NOT "!Message:%%~A=!"=="!Message!" (
			SET "MakeUnsafe=1"
		)
	)
	SET "Message=!Message:~0,-1!"
	IF "!MakeUnsafe!"=="1" (
		CALL :MakeParameterUnsafe Message Message
	)

	IF "!Check!"=="1" (
		IF "!AddNewline!"=="1" (
			ECHO !ESC![!Style!m!Type!!Message!!ESC![0m
		) ELSE (
			<NUL SET /P=!ESC![!Style!m!Type!!Message!!ESC![0m
		)
	)
ENDLOCAL
EXIT /B 0

:ShowInformation
SETLOCAL
	SET "Package=%~1"

	FOR /F "TOKENS=1,2,3,4,5 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| !Sort!') DO (
		SETLOCAL
		IF "!Package!"=="all" (
			SET "Pkg=%%~A"
		) ELSE (
			SET "Pkg=!Package!"
		)
		IF "%%~A"=="!Pkg!" (
			SET "Name=%%~A"
			SET "RelPath=%%~B"
			SET "URL=%%~C"
			SET "URL32bit=%%~D"
			SET "WhereToFindURLs=%%~E"

			CALL :FollowURL URL LastRedirectURL
			CALL :FollowURL URL32bit LastRedirectURL32bit

			CALL :DetermineFileExtensionOfURL LastRedirectURL FileExtension
			CALL :DetermineFileExtensionOfURL LastRedirectURL32bit FileExtension32bit

			CALL :ColorEcho ACTION def "Showing information for "!Pkg!"." 1 1
			CALL :ColorEcho "" white "" 1 0
			CALL :ColorEcho "" cyan "### General Information ###" 1 0
			ECHO     Name/Runstring:                            !ESC![4m!Name!!ESC![0m
			ECHO     Relative path to binary or folder:         !ESC![4m!RelPath!!ESC![0m
			ECHO     Where to find new download links:          !ESC![4m!WhereToFindURLs!!ESC![0m
			CALL :ColorEcho "" cyan "### 64 bit ###" 1 0
			ECHO     Download link:                             !ESC![4m!URL!!ESC![0m
			ECHO     Direct download link ^(derived from above^): !ESC![32;4m!LastRedirectURL!!ESC![0m
			ECHO     File size:                                 !ESC![4m~!FileSizeMB! MB!ESC![0m ^(!FileSize! Bytes^)
			ECHO     File type:                                 !ESC![4m!FileExtension!!ESC![0m
			ECHO     Version:                                   !ESC![4mUnknown!ESC![0m
			CALL :ColorEcho "" cyan "### 32 bit ###" 1 0
			ECHO     Download link:                             !ESC![4m!URL32bit!!ESC![0m
			ECHO     Direct download link ^(derived from above^): !ESC![32;4m!LastRedirectURL32bit!!ESC![0m
			ECHO     File size:                                 !ESC![4m~!FileSizeMB32bit! MB!ESC![0m ^(!FileSize32bit! Bytes^)
			ECHO     File type:                                 !ESC![4m!FileExtension32bit!!ESC![0m
			ECHO     Version:                                   !ESC![4mUnknown!ESC![0m
			IF NOT "!Pkg!"=="all" (
				ENDLOCAL
				EXIT /B 0
			)
		)
		ENDLOCAL
	)
	IF NOT "!Package!"=="all" (
		CALL :ColorEcho ERROR def "The package "!Package!" could not be found in "!PackagesFilePath!"." 1 0
		GOTO :Error
	) ELSE (
		CALL :ColorEcho INFO def "Successfully showed information for all packages." 1 1
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ Download ------------------------
:Download
SETLOCAL
	SET "URLReference=%~1"
	SET "DestFile=%~2"
	SET "FileName=%~n2"
	REM DownloadWithVbs = 1 --> force download with vbs
	SET "DownloadWithVbs=%~3"

	CALL :ProgramWorks "curl"

	SET /A "Check=^!ERRORLEVEL & ^!DownloadWithVbs"

	IF "!Check!"=="1" (
		CALL :ColorEcho ACTION def "Downloading !FileName! with curl:" 1 0
		CALL :DownloadWithCurl "!URLReference!" "!DestFile!"
	) ELSE (
		CALL :ColorEcho ACTION def "Downloading !FileName! with VBScript:" 1 0
		CALL :DownloadWithVbs "!URLReference!" "!DestFile!"
	)
	IF EXIST "!DestFile!" (
		CALL :ColorEcho SUCCESS def "Successfully downloaded !FileName!^!" 1 0
	) ELSE (
		CALL :ColorEcho FAIL def "Failed^! Could not download !FileName!^!" 1 0
		CALL :ColorEcho ERROR def "!FileName! could not be downloaded from this URL: !%URLReference%!. Please check if the URL is active." 1 0
		GOTO :Error
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ DownloadWithCurl ------------------------
:DownloadWithCurl
SETLOCAL
	SET "URL=!%~1!"
	SET "DestFile=%~2"

	SET "TmpTIME=!TIME:~,-3!"
	SET "FinishedAt=!TmpTIME: =0!, !DATE!"
	1>NUL !Chcp! 1252
	!Powershell! -Command Remove-Item Alias:curl; curl --location """!URL!""" --output """!DestFile!""" 2^>^&1 ^| Select-String k ^| Foreach-Object {$data = (([string] $_).Trim() -Split '\s+')[0,1,3,9]; Write-Host -NoNewline """[PROGRESS] !ESC![37;1;4m$($data[0])%%!ESC![0m: $($data[2])B / $($data[1])B. Elapsed: $($data[3]).!WhiteSpaceBuffer!!CR!"""}; $FinishedAt=cmd.exe '/V /C SET "TmpTIME=^^^!TIME:~,-3^^^!"^&SET "FinishedAt=^^^!TmpTIME: =0^^^!, ^^^!DATE^^^!"^&ECHO ^^^!FinishedAt^^^!'; $FinishedAt=$FinishedAt.Trim().Replace('""""', ''); Write-Host """!ESC![32m[PROGRESS] !ESC![32;1;4m100%%!ESC![0m!ESC![32m: $($data[2])B / $($data[1])B. Elapsed: $($data[3]). Finished at $($FinishedAt).!ESC![0m"""
	1>NUL !Chcp! 65001
ENDLOCAL
EXIT /B 0

REM ------------------------ DownloadWithVbs ------------------------
:DownloadWithVbs
SETLOCAL
	REM Progress bar can hardly be implemented. Use this only to bootstrap to download curl or something similar.
	SET "URL=!%~1!"
	SET "DestFile=%~2"

	REM Source: https://stackoverflow.com/a/2973344
	ECHO DIM xHttp: Set xHttp=CreateObject("Microsoft.XMLHTTP") >"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp2=CreateObject("MSXML2.XMLHTTP.3.0") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp3=CreateObject("MSXML2.XMLHTTP.6.0") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp4=CreateObject("MSXML2.ServerXMLHTTP") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp5=CreateObject("MSXML2.ServerXMLHTTP.3.0") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp6=CreateObject("MSXML2.ServerXMLHTTP.6.0") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp7=CreateObject("MSXML2.XMLHTTP") >>"!TmpDir!\DownloadWithVbs.vbs"
	REM ECHO DIM xHttp: Set xHttp8=CreateObject("WinHttp.WinHttpRequest.5.1") >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO DIM bStrm: Set bStrm=CreateObject("ADODB.Stream") >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO xHttp.Open "GET", "!URL!", False >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO xHttp.Send >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO With bStrm >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO     .type=1 '//binary >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO     .open >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO     .write xHttp.responseBody >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO     .saveToFile "!DestFile!", 2 '//overwrite >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO End With >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO Set xHttp = Nothing >>"!TmpDir!\DownloadWithVbs.vbs"
	ECHO Set bStrm = Nothing >>"!TmpDir!\DownloadWithVbs.vbs"

	!Cscript! //NOLOGO "!TmpDir!\DownloadWithVbs.vbs"
ENDLOCAL
EXIT /B 0

REM ------------------------ DownloadDependency ------------------------
:DownloadDependency
SETLOCAL
	SET "Package=%~1"
	CALL :ColorEcho INFO def "!Package! will now be downloaded in a separate process." 1 1
	CALL :ColorEcho "" white "" 1 1
	CALL :ColorEcho "" white "[-----New process started: "!DifferentCmdLine!" !Package!-----]" 1 1
	CMD /C ^""!DifferentCmdLine!" !Package!^"
	CALL :ColorEcho "" white "[------New process ended------]" 1 1
	CALL :ColorEcho "" white "" 1 1
ENDLOCAL
EXIT /B 0

REM ------------------------ Unzip ------------------------
:Unzip
SETLOCAL
	SET "SrcFile=%~1"
	SET "FileName=%~n1"
	SET "DestFolder=%~2"

	CALL :ProgramWorks "7z"
	IF NOT "!ERRORLEVEL!"=="0" (
		CALL :ColorEcho WARNING def "7z was NOT found" 1 0
		CALL :DownloadDependency 7zip
	)
	CALL :ProgramWorks "7z"
	IF NOT "!ERRORLEVEL!"=="0" (
		CALL :ColorEcho WARNING def "7z still not found." 1 0
		CALL :ColorEcho ACTION def "Unzipping !FileName! with VBScript instead:" 1 0
		CALL :UnzipWithVbs "!SrcFile!" "!DestFolder!"
	) ELSE (
		CALL :ColorEcho ACTION def "Unzipping !FileName! with 7z:" 1 0
		7z x -bsp1 -o"!DestFolder!" "!SrcFile!"
	)

	CALL :RemoveNestedFolderStructure "!DestFolder!" "!DestFolder!"
	IF "!ERRORLEVEL!"=="0" (
		CALL :ColorEcho SUCCESS def "Successfully unzipped "!SrcFile!" to "!DestFolder!"^!" 1 1
	) ELSE (
		CALL :ColorEcho ERROR def "Could not unzip "!SrcFile!" to "!DestFolder!"." 1 1
		GOTO :Error		
	)

	ENDLOCAL
	EXIT /B 0

	SET "ItemCount=0"
	FOR /F %%A IN ('DIR "!DestFolder!" /A /B') DO (1>NUL SET /A "ItemCount+=1")
	IF !ItemCount! GEQ 2 (
		CALL :ColorEcho SUCCESS def "Successfully unzipped "!SrcFile!" to "!DestFolder!"^!" 1 1
	) ELSE (
		IF !ItemCount! EQU 1 (
			CALL :ColorEcho SUCCESS def "Successfully unzipped "!SrcFile!" to "!DestFolder!"^!" 1 1
			SET "Directory=-"
			FOR /F "DELIMS=" %%A IN ('DIR "!DestFolder!" /AD /B') DO (SET "Directory=%%A")
			IF NOT "!Directory!"=="-" (
				CALL :ColorEcho INFO def "The contents of the zip file need to be moved up one folder." 1 1
				CALL :ColorEcho ACTION def "Moving the contents of "!DestFolder!\!Directory!" to "!DestFolder!" with VBScript:" 1 1
				CALL :MoveFilesWithVbsAndDeleteDir "!DestFolder!\!Directory!" "!DestFolder!"
				2>NUL RD /S /Q "!DestFolder!\!Directory!"
				CALL :ColorEcho SUCCESS def "Successfully moved the contents one folder up." 1 1
			)
		) ELSE (
			CALL :ColorEcho ERROR def "Could not unzip "!SrcFile!" to "!DestFolder!"." 1 1
			GOTO :Error
		)
	)
	ENDLOCAL
EXIT /B 0

REM ------------------------ UnzipWithVbs ------------------------
:UnzipWithVbs
SETLOCAL
	SET "SrcFile=%~1"
	SET "DestFolder=%~2"

	REM Source: https://stackoverflow.com
	ECHO UnzipTo="!DestFolder!" >"!TmpDir!\Unzip.vbs"
	ECHO Set FSO=CreateObject("Scripting.FileSystemObject") >>"!TmpDir!\Unzip.vbs"
	ECHO If Not FSO.FolderExists(UnzipTo) Then >>"!TmpDir!\Unzip.vbs"
	ECHO    FSO.CreateFolder(UnzipTo) >>"!TmpDir!\Unzip.vbs"
	ECHO End If >>"!TmpDir!\Unzip.vbs"
	ECHO Set objShell=CreateObject("Shell.Application") >>"!TmpDir!\Unzip.vbs"
	ECHO Set filesInZip=objShell.NameSpace("!SrcFile!").Items >>"!TmpDir!\Unzip.vbs"
	ECHO objShell.NameSpace(UnzipTo).MoveHere(filesInZip) >>"!TmpDir!\Unzip.vbs"
	ECHO Set FSO=Nothing >>"!TmpDir!\Unzip.vbs"
	ECHO Set objShell=Nothing >>"!TmpDir!\Unzip.vbs"

	!Cscript! //NOLOGO "!TmpDir!\Unzip.vbs"
	REM TODO: Catch Unzipping error 
ENDLOCAL
EXIT /B 0

REM ------------------------ ExtractMsi ------------------------
:ExtractMsi
SETLOCAL
	SET "SrcFile=%~1"
	SET "DestFolder=%~2"

	2>NUL RD /S /Q "!DestFolder!"
	"!System32!\msiexec.exe" /a "!SrcFile!" /qn TargetDir="!DestFolder!"
	FOR /F "DELIMS=" %%A IN ('DIR "!DestFolder!" /A-D /B') DO (DEL "!DestFolder!\%%A")
	CALL :RemoveNestedFolderStructure "!DestFolder!" "!DestFolder!"
ENDLOCAL
EXIT /B 0

REM ------------------------ RemoveNestedFolderStructure ------------------------
:RemoveNestedFolderStructure
SETLOCAL
	REM TODO Rethink ~f choice
	SET "TargetFolder=%~f1"
	SET "DestFolder=%~f2"

	SET "ItemCount=0"
	FOR /F %%A IN ('DIR "!TargetFolder!" /A /B') DO (1>NUL SET /A "ItemCount+=1")

	IF !ItemCount! GEQ 2 (
		IF NOT "!TargetFolder!"=="!DestFolder!" (
			CALL :MoveFilesWithVbsAndDeleteDir "!TargetFolder!" "!DestFolder!"
		)
	) ELSE IF !ItemCount! EQU 1 (
		SET "Directory=NotADirectory"
		FOR /F "DELIMS=" %%A IN ('DIR "!TargetFolder!" /AD /B') DO (SET "Directory=%%A")
		IF NOT "!Directory!"=="NotADirectory" (
			CALL :RemoveNestedFolderStructure "!TargetFolder!\!Directory!" "!DestFolder!"
		) ELSE IF NOT "!TargetFolder!"=="!DestFolder!" (
			CALL :MoveFilesWithVbsAndDeleteDir "!TargetFolder!" "!DestFolder!"
		)
	) ELSE (
		ENDLOCAL
		EXIT /B 1
	)

ENDLOCAL
EXIT /B 0


REM ------------------------ MoveFilesWithVbsAndDeleteDir ------------------------
:MoveFilesWithVbsAndDeleteDir
SETLOCAL
	REM NO trailing backslash for SrcFolder.
	SET "SrcFolder=%~1"
	SET "DestFolder=%~2"

	REM Source: https://social.technet.microsoft.com/Forums/es-ES/8baf14c5-1888-489f-87ef-ed9516ed9f53/progress-bar-for-vbscript
	ECHO Set ShellApp = CreateObject("Shell.Application") >"!TmpDir!\MoveFilesWithVbsAndDeleteDir.vbs"
	ECHO Set Folder = ShellApp.Namespace("!DestFolder!") >>"!TmpDir!\MoveFilesWithVbsAndDeleteDir.vbs"
	ECHO Folder.MoveHere "!SrcFolder!\*" >>"!TmpDir!\MoveFilesWithVbsAndDeleteDir.vbs"

	!Cscript! //NOLOGO "!TmpDir!\MoveFilesWithVbsAndDeleteDir.vbs"
	2>NUL RD "!SrcFolder!"
ENDLOCAL
EXIT /B 0

REM ------------------------ CreateShortcutWithVbs ------------------------
:CreateShortcutWithVbs
SETLOCAL
	SET "ScrFileOrFolder=%~1"
	SET "DestShortcutFile=%~2"
	
	REM Source: https://docs.microsoft.com/de-de/troubleshoot/windows-client/admin-development/create-desktop-shortcut-with-wsh
	ECHO Dim WshShell: Set WshShell = CreateObject("Wscript.Shell") >"!TmpDir!\CreateShortcut.vbs"
	ECHO Set oMyShortcut = WshShell.CreateShortcut("!DestShortcutFile!") >>"!TmpDir!\CreateShortcut.vbs"
	ECHO oMyShortcut.WindowStyle = 4 >>"!TmpDir!\CreateShortcut.vbs"
	ECHO OMyShortcut.TargetPath = "!ScrFileOrFolder!" >>"!TmpDir!\CreateShortcut.vbs"
	ECHO oMyShortCut.Save >>"!TmpDir!\CreateShortcut.vbs"
	ECHO Set oMyShortCut = Nothing >>"!TmpDir!\CreateShortcut.vbs"
	ECHO Set WshShell = Nothing >>"!TmpDir!\CreateShortcut.vbs"

	!Cscript! //NOLOGO "!TmpDir!\CreateShortcut.vbs"
ENDLOCAL
EXIT /B 0

REM ------------------------ CreateFolder ------------------------
:CreateFolder
SETLOCAL
	SET "DestFolder=%~1"
	IF NOT EXIST "!DestFolder!" (
		CALL :ColorEcho ACTION def "Creating directory "!DestFolder!"..." 0 1
		2>NUL MD "!DestFolder!"
		IF NOT EXIST "!DestFolder!" (
			CALL :ColorEcho "" red " Failed^!" 1 1
			CALL :ColorEcho ERROR def "Directory "!DestFolder!" could not be created. Run as admin or in a different location." 1 0
			GOTO :Error
		)
		CALL :ColorEcho "" green " Success^!" 1 1
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ DetermineFileExtensionOfURL ------------------------
:DetermineFileExtensionOfURL
SETLOCAL
	SET "URL=!%~1!"
	SET "ReturnVar1=%~2"

	SET "TmpFile=!TmpDir!\determineFileExtension.extension"

	2>NUL DEL "!TmpFile!"
	1>NUL !Chcp! 1252
	!Powershell! -Command Remove-Item Alias:curl; curl --location --range 0-32 """!URL!""" --output """!TmpFile!""" 2^>^&1 ^| Select-String k ^| Foreach-Object {$data = ^(^([string] $_^).Trim^(^) -Split '\s+'^)[0,1,3,9]; If ^( ^(Invoke-Expression^($data[2].replace^('k','*1000'^).replace^('M','*1000000'^)^)^) -gt 26 ^){ exit }}
	1>NUL !Chcp! 65001
	IF NOT EXIST "!TmpFile!" (
		IF NOT "!LastRedirectURL!"=="" CALL :ColorEcho WARNING def "File type for URL could not be determined, because there probably was some problem with curl." 1 0
		SET "FileExtension=undetermined"
	) ELSE (
		CALL :DetermineFileExtension "!TmpFile!" FileExtension
	)
ENDLOCAL & SET "%ReturnVar1%=%FileExtension%"
EXIT /B 0

REM ------------------------ DetermineFileExtension ------------------------
:DetermineFileExtension
SETLOCAL
	SET "ScrFile=%~1"
	SET "ReturnVar1=%~2"

	REM Get first 26 Bytes
	1>NUL !Chcp! 1252
	FOR /F "DELIMS=" %%A IN ('!Powershell! -Command ^(Get-Content '!ScrFile!' -Encoding byte -TotalCount 26^) -Join ' '') DO (SET "First26Bytes=%%A")
	1>NUL !Chcp! 65001

	REM Check signatures/magic bytes
	REM Current managable file types: zip, 7z, exe, msi, bat/cmd
	IF "!First26Bytes:~,5!"=="77 90" (
		SET "FileExtension=exe"
		GOTO :Finish2
	)
	IF "!First26Bytes:~,9!"=="80 75 3 4" (
		SET "FileExtension=zip"
		GOTO :Finish2
	)
	IF "!First26Bytes:~,15!"=="58 58 98 97 116" (
		REM Because bat/cmd files are plain text, the only way to determine the file type is to
		REM manually add the first bytes. Therefore, every batch file that is supposed to be
		REM recognized by this package manager must start with "::bat" without the qoutes.
		SET "FileExtension=bat"
		GOTO :Finish2
	)
	IF "!First26Bytes:~,13!"=="59 97 104 107" (
		REM Cf. IF statement for bat extension, this time with ;ahk
		SET "FileExtension=ahk"
		GOTO :Finish2
	)
	IF "!First26Bytes:~,20!"=="55 122 188 175 39 28" (
		SET "FileExtension=7z"
		GOTO :Finish2
	)
	IF "!First26Bytes!"=="208 207 17 224 161 177 26 225 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 62 0" (
		SET "FileExtension=msi"
		GOTO :Finish2
	)
	SET "FileExtension=unknown"

	:Finish2
ENDLOCAL & SET "%ReturnVar1%=%FileExtension%"
EXIT /B 0

REM ------------------------ FollowURL ------------------------
:FollowURL
SETLOCAL
	REM URL can't be passed literally, but must be passed as the variable name that contains the URL.
	REM Must be done this way to prevent percent doubling.
	SET "URL=!%~1!"
	SET "ReturnVar1=%~2"

	SET "LastRedirectURL="
	FOR /F "TOKENS=2 DELIMS= " %%A IN ('2^>NUL curl --silent --location --head -X GET "!URL!" ^| !Findstr! "Location: "') DO (SET "LastRedirectURL=%%A")
	IF NOT DEFINED LastRedirectURL SET "LastRedirectURL=!URL!"

ENDLOCAL & SET "%ReturnVar1%=%LastRedirectURL%"
EXIT /B 0

REM ------------------------ UpdatePath ------------------------
:UpdatePath
SETLOCAL
	CALL :GetPaths "PATH" "OldPATH" "OldSystemPATH"

	REM Update this local PATH.
	IF NOT "!PATH:~-1!"==";" SET "PATH=!PATH!;"
	SET "PATH=!PATH!!RedirectsDir!;!PckDir!"

	IF "!OldPATH:%RedirectsDir%=!;!OldSystemPATH:%RedirectsDir%=!"=="!OldPATH!;!OldSystemPATH!" (
		CALL :ColorEcho INFO def "Redirects and PCK directory are not in PATH" 1 1
		CALL :ColorEcho ACTION def "Adding Redirects and PCK directory permanently to PATH with SETX..." 0 1

		1>NUL 2>&1 SETX PATH "!OldPATH!!RedirectsDir!;!PckDir!"

		CALL :GetPaths "PATH" "OldPATH" "OldSystemPATH"
		IF "!OldPATH:%RedirectsDir%=!;!OldSystemPATH:%RedirectsDir%=!"=="!OldPATH!;!OldSystemPATH!" (
			CALL :ColorEcho "" red " Failed^!" 1 1
			CALL :ColorEcho WARNING def "For some unknown reason the Redirects and PCK directories could not be added to PATH." 1 0
			CALL :Anomaly
		) ELSE (
			CALL :ColorEcho "" green " Success^!" 1 1
		)
	)
ENDLOCAL & SET "PATH=%PATH%"
EXIT /B 0

REM ------------------------ UpdatePathExt ------------------------
:UpdatePathExt
SETLOCAL
	CALL :GetPaths "PATHEXT" "OldPATHEXT" "OldSystemPATHEXT"

	REM Update this local PATHEXT
	IF NOT "!PATHEXT:~-1!"==";" SET "PATHEXT=!PATHEXT!;"
	SET "PATHEXT=!PATHEXT!.LNK"

	IF "!OldPATHEXT:.LNK=!;!OldSystemPATHEXT:.LNK=!"=="!OldPATHEXT!;!OldSystemPATHEXT!" (
		CALL :ColorEcho INFO def "Extension ".LNK" is not in "PATHEXT"." 1 1
		CALL :ColorEcho ACTION def "Adding extension ".LNK" permanently to "PATHEXT" with SETX..." 0 1

		1>NUL 2>&1 SETX PATHEXT "!OldPATHEXT!.LNK"

		CALL :GetPaths "PATHEXT" "OldPATHEXT" "OldSystemPATHEXT"
		IF "!OldPATHEXT:.LNK=!;!OldSystemPATHEXT:.LNK=!"=="!OldPATHEXT!;!OldSystemPATHEXT!" (
			CALL :ColorEcho "" red " Failed^!" 1 1
			CALL :ColorEcho WARNING def "For some unknown reason, the ".LNK" extension could not be added to the "PATHEXT" environment variable." 1 0
			CALL :Anomaly
		)
		CALL :ColorEcho "" green " Success^!" 1 1
	)
ENDLOCAL & SET "PATHEXT=%PATHEXT%"
EXIT /B 0

REM ------------------------ GetPaths ------------------------
:GetPaths
SETLOCAL
	SET "EnvVar=%~1"
	SET "ReturnVar1=%~2"
	SET "ReturnVar2=%~3"

	FOR /F "TOKENS=* SKIP=2" %%A IN ('2^>NUL "!System32!\reg.exe" QUERY "HKEY_CURRENT_USER\Environment" /V "!EnvVar!"') DO (SET "OldPATH=%%A;")
	IF NOT DEFINED OldPATH SET "OldPATH=PATH REG_SZ ;"
	SET "OldPATH=!OldPATH:REG_SZ=|!"
	FOR /F "TOKENS=2 DELIMS=^|" %%A IN ("!OldPATH!") DO (SET "OldPATH=%%A")
	CALL :Trim OldPATH OldPATH
	SET "OldPATH=!OldPATH:;;=;!"

	FOR /F "TOKENS=* SKIP=2" %%A IN ('2^>NUL "!System32!\reg.exe" QUERY "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /V "!EnvVar!"') DO (SET "OldSystemPATH=%%A;")
	IF NOT DEFINED OldSystemPATH SET "OldSystemPATH=PATH REG_SZ ;"
	SET "OldSystemPATH=!OldSystemPATH:REG_SZ=|!"
	FOR /F "TOKENS=2 DELIMS=^|" %%A IN ("!OldSystemPATH!") DO (SET "OldSystemPATH=%%A")
	CALL :Trim OldSystemPATH OldSystemPATH
	SET "OldSystemPATH=!OldSystemPATH:;;=;!"
ENDLOCAL & SET "%ReturnVar1%=%OldPATH%" & SET "%ReturnVar2%=%OldSystemPATH%"
EXIT /B 0

REM ------------------------ TrimFast ------------------------
:TrimFast
SETLOCAL
	SET "String=!%~1!"
	SET "ReturnVar1=%~2"
	SET String=!String:"='!
	CALL :_TrimFast TrimmedString !String!
	SET TrimmedString=!TrimmedString:'="!
ENDLOCAL & SET "%ReturnVar1%=%TrimmedString%"
EXIT /B 0

:_TrimFast
SETLOCAL
	FOR /F "TOKENS=1*" %%A IN ("%*") DO (ENDLOCAL & SET "%1=%%B")
EXIT /B 0


REM ------------------------ Trim ------------------------
REM Source: https://stackoverflow.com/a/48050848/13823467
:Trim
SETLOCAL
	REM Must be defined here.
	(SET LF=^
%= BLANK LINE REQUIRED =%
)
	REM Must be defined here.
	SET "CR=" & IF NOT DEFINED CR FOR /F "SKIP=1" %%C IN ('ECHO(^|REPLACE ? . /W /U') DO (SET "CR=%%C")

	SET "ddx=!"
	SETLOCAL EnableDelayedExpansion
	SET "Die="
	IF NOT DEFINED %1 (
	    CALL :ColorEcho WARNING def "Trimming failed, because variable "%1" is not defined." 1 0
	    SET "Die=1"
	) ELSE (
		SET "Str=!%1!"
	)

	IF NOT DEFINED Die FOR %%L IN ("!LF!") DO (
		IF "!Str!" NEQ "!Str:%%~L=!" (
		    CALL :ColorEcho WARNING def "Trimming failed, because variable "%1" contains linefeeds." 1 0
		    SET "Die=1"
		)
	)

	IF NOT DEFINED Die FOR %%C IN ("!CR!") DO (
		IF "!Str!" NEQ "!Str:%%~C=!" (
		    CALL :ColorEcho WARNING def "Trimming failed, because variable "%1" contains carriage returns." 1 0
		    SET "Die=1"
		)
	)

	IF DEFINED Die GOTO :Die

	(FOR /F EOL^= %%A IN ("!Str!") DO REM NOP
	) || (
	    CALL :ColorEcho WARNING def "Trimming failed, because variable "%1" consists entirely of whitespace." 1 0
	    ENDLOCAL & ENDLOCAL & SET "%2=" & EXIT /B 0
	)

	SET "Str=!Str:^=^^^^!"
	SET "Str=!Str:"=""!"
	SET "Str=%Str:!=^^^!%" !

	CALL :_TRIM "%%Str%%

	IF NOT DEFINED ddx SET "Str=!Str:^=^^^^!"
	IF NOT DEFINED ddx SET "Str=%Str:!=^^^!%" !
	SET "Str=!Str:""="!"

	FOR /F TOKENS^=*^ EOL^= %%A IN ("!Str!") DO (
	    ENDLOCAL & ENDLOCAL & SET "%2=%%A" !
	)
EXIT /B 0

:Die
ENDLOCAL & ENDLOCAL & SET "%2=" & EXIT /B 1

:_TRIM
	SET "Str=%~1" !
EXIT /B 0
REM "
REM ------------------------ StripQuotes ------------------------
:StripQuotes
SETLOCAL
	SET "String=!%~1!"
	SET "ReturnVar1=%~2"
	FOR /F "DELIMS=" %%A IN ("!String!") DO (SET "Stripped=%%~A")
ENDLOCAL & SET "%ReturnVar1%=%Stripped%"
EXIT /B 0

REM ------------------------ CheckExeExtractable ------------------------
:CheckExeExtractable
SETLOCAL
	SET "PathToExe=%~1"
	SET "ReturnVar1=%~2"

	CALL :ProgramWorks "7z"
	IF "!ERRORLEVEL!"=="0" (
		ECHO 7z DOES exist
	) ELSE (
		ECHO 7z does not not exist
	)
ENDLOCAL & SET "%ReturnVar1%=ReturnValue"
EXIT /B 0

REM ----------------------- GetProgramPath -----------------------
:GetProgramPath
SETLOCAL
	SET "FileName=%~1"
	SET "DefaultLocation=%~2"
	SET "ReturnVar1=%~3"

	IF EXIST "!System32!\where.exe" (
		SET "Where="!System32!\where.exe""
	) ELSE (
		FOR /F "DELIMS=" %%A IN (
			'2^>NUL where where'
		) DO (
			SET "Where="%%~A""
			GOTO :Continue
		)
	)

	:Continue
	SET "ProgramPath="
	FOR /F "DELIMS=" %%A IN (
		'2^>NUL !Where! "!DefaultLocation!":"!FileName!"'
	) DO (
		SET "ProgramPath=%%~A"
		GOTO :Found
	)
	FOR /F "DELIMS=" %%A IN (
		'2^>NUL !Where! "!FileName!"'
	) DO (
		SET "ProgramPath=%%~A"
		GOTO :Found
	)

	CALL :ColorEcho WARNING def "!FileName! was not found." 1 0
	ENDLOCAL & SET "%ReturnVar1%=%FileName%"
	EXIT /B 1

	:Found
ENDLOCAL & SET "%ReturnVar1%="%ProgramPath%""
EXIT /B 0

REM ------------------------ ProgramWorks ------------------------
:ProgramWorks
SETLOCAL
	SET "FileName=%~1"
	REM "!ReturnVar1!" MUST NOT be "ERRORLEVEL"
	SET "ReturnVar1=%~2"

	"!System32!\where.exe" /Q "!FileName!"

	IF "!ERRORLEVEL!"=="9009" (
		CALL :ColorEcho ERROR def "where-utility does not exist. Is your path variable broken?" 1 0
		GOTO :Error
	)
	IF "!ERRORLEVEL!"=="0" (
		ENDLOCAL
		EXIT /B 0
	)
ENDLOCAL
EXIT /B 1

REM ------------------------ WarnAboutIgnoredArgs ------------------------
:WarnAboutIgnoredArgs
SETLOCAL
	SET "MaxNumberOfArgs=%~1"
	SET "ExtraArgs= "
	SET "Command=%~2"
	SET "CommandWithArgs=!Command!"
	SHIFT
	SHIFT
	SET "NumberOfArgs=1"
	FOR /L %%A IN (1,1,50) DO (
		CALL SET "ShiftedArg=%%1"
		IF "!ShiftedArg!"=="" (
			GOTO :NoArgsAnymore
		)
		SET /A "NumberOfArgs+=1"
		SHIFT
		CALL CALL SET "CommandWithArgs=!CommandWithArgs! %%%%0"
		IF !NumberOfArgs! GTR !MaxNumberOfArgs! (
			CALL CALL SET "ExtraArgs=!ExtraArgs! %%%%0"
		)
	)
	CALL :ColorEcho WARNING def "You specified 50 or more args to the command !Command!." 1 0

	:NoArgsAnymore
	SET "ExtraArgs=!ExtraArgs:~2!"
	CALL :StripQuotes CommandWithArgs CommandWithArgs
	CALL :MakeParameterSafe ExtraArgs SafeExtraArgs
	CALL :MakeParameterSafe CommandWithArgs SafeCommandWithArgs
	%$ToLower% SafeExtraArgs
	%$ToLower% SafeCommandWithArgs
	IF !NumberOfArgs! GTR !MaxNumberOfArgs! (
		SET /A "Difference=NumberOfArgs - MaxNumberOfArgs"
		IF "!Difference!"=="1" (
			CALL :ColorEcho WARNING def "The last argument (`!SafeExtraArgs!`) in the command `!SafeCommandWithArgs!` was ignored." 1 0
		) ELSE (
			CALL :ColorEcho WARNING def "The last !Difference! arguments (`!SafeExtraArgs!`) in the command `!SafeCommandWithArgs!` were ignored." 1 0
		)
		ENDLOCAL
		EXIT /B 1
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ MakeParameterUnsafe ------------------------
:MakeParameterUnsafe
SETLOCAL
	SET "String=!%~1!"
	SET "ReturnVar1=%~2"

	CALL "!TmpDir!\REPLVAR.BAT" String String "\x3d" "=" L
	SET String=!String:\x22="!
	SET String=!String:\x20= !
	SET String=!String:\x3b=;!
	SET String=!String:\x2c=,!
	SET UnsafeParameter=!String:\x09=	!
ENDLOCAL & SET "%ReturnVar1%=%UnsafeParameter%"
EXIT /B

REM ------------------------ MakeParameterSafe ------------------------
:MakeParameterSafe
SETLOCAL
	SET "String=!%~1!"
	SET "ReturnVar1=%~2"

	SET String=!String: =\x20!
	SET String=!String:;=\x3b!
	SET String=!String:,=\x2c!
	SET String=!String:	=\x09!
	SET String=!String:"=\x22!
	CALL "!TmpDir!\REPLVAR.BAT" String SafeParameter "=" "\x3d" L
ENDLOCAL & SET "%ReturnVar1%=%SafeParameter%"
EXIT /B

REM ------------------------ ShowHelp ------------------------
:ShowHelp
SETLOCAL
	CALL :ColorEcho INFO def "Usage:" 1 0
	CALL :ColorEcho "" yellow "    pck package                      # install a package" 1 0
	CALL :ColorEcho "" yellow "    pck i {package | all}            # show info for {a package | all packages}" 1 0
	CALL :ColorEcho "" yellow "    pck i {package | all} [offline]  # as above, but don't fetch further info from URLs" 1 0
	CALL :ColorEcho "" yellow "    pck r package                    # uninstall (removes) a package" 1 0
	CALL :ColorEcho "" yellow "    pck u package link               # update a package's link" 1 0
	CALL :ColorEcho "" yellow "    pck a package link               # add a package to list" 1 0
	CALL :ColorEcho "" yellow "    pck list [regex]                 # list all packages [that match the regex]" 1 0
	REM                                     /!\   /!\ For some cool reason, you don't need/can't escape these qoutes,
	REM                                      !     !  maybe because there are no spaces. If you escape them, it won't work as expected.
	CALL :ColorEcho "" yellow "    pck [help]                       # show this help" 1 0
	ENDLOCAL
	EXIT /B 0


REM ------------------------ ParsePackageConfigFile ------------------------
:ParsePackageConfigFile
SET SingleQuote="
REM "
SETLOCAL EnableDelayedExpansion
	SET "SrcFile=%~1"

	SET "BEGIN="
	SET "PackageList="
	SET j=0
	SET i=0
	FOR /F "EOL=# DELIMS=" %%A IN ('TYPE "!SrcFile!"') DO (
		SET "Line=%%~A"
		REM Macros for speed
		%$Trim% Line
		%$Trim% Line SingleQuote
		IF "!Line!"=="-BEGIN-" (
			SET "BEGIN=1"
		) ELSE IF "!Line!"=="-END-" (
			SET "PackageList[!j!][len]=!i!"
			GOTO :DoneParsing
		) ELSE IF "!Line!"=="-" (
			SET "PackageList[!j!][len]=!i!"
			SET "i=0"
		) ELSE IF DEFINED BEGIN (
			IF "!i!"=="0" (
				SET /A "j += 1"
				SET "PackageList[!j!][name]=!Line!"
			) ELSE IF "!i!"=="1" (
				SET "PackageList[!j!][type]=!Line!"
			) ELSE (
				CALL CALL SET "Type=%%%%PackageList[%%j%%][type]%%%%"
				IF "!Type!"=="direct" (
					IF "!i!"=="2" (
						SET "PackageList[!j!][whereToFindURLs]=!Line!"
					) ELSE IF "!i!"=="3" (
						SET "PackageList[!j!][64BitUrl]=!Line!"
					) ELSE IF "!i!"=="4" (
						SET "PackageList[!j!][32BitUrl]=!Line!"
					) ELSE (
						SET /A "FileCounter=i - 4"
						FOR /F "TOKENS=1,2 DELIMS=;" %%B IN ("!Line!") DO (
							SET "PackageList[!j!][File!FileCounter!][relPath]=%%~B"
							IF "%%~C"=="d" (
								IF NOT "%%~nB"=="" (
									SET "PackageList[!j!][File!FileCounter!][name]=%%~nB"
								) ELSE (
									CALL CALL :ColorEcho WARNING def "Error parsing package "%%%%PackageList[%%j%%][name]%%%%", default identifier used although relative path points to a folder." 1 0
									SET "PackageList[!j!][File!FileCounter!][name]=%%~C"
									)
							) ELSE (
								SET "PackageList[!j!][File!FileCounter!][name]=%%~C"
							)
						)
					)
				) ELSE IF "!Type!"=="github" (
					ECHO GITHUB
				) ELSE IF "!Type!"=="sourceforge" (
					ECHO SOURCEFORGE
				) ELSE (
					ECHO !Type!
					CALL CALL :ColorEcho WARNING def "Type !Type! for package "%%%%PackageList[%%j%%][name]%%%%" does not exist." 1 0
				)
			)
			SET /A "i+=1"
		)
	)

	:DoneParsing
	SET "PackageList[!j!][len]=!i!"
	SET "PackageList[len]=!j!"

	FOR /L %%A IN (1,1,!PackageList[len]!) DO (
		ECHO PackageList[%%A][len]:"!PackageList[%%A][len]!"
		ECHO PackageList[%%A][name]:"!PackageList[%%A][name]!"
		ECHO PackageList[%%A][type]:"!PackageList[%%A][type]!"
		ECHO PackageList[%%A][whereToFindURLs]:"!PackageList[%%A][whereToFindURLs]!"
		ECHO PackageList[%%A][64BitUrl]:"!PackageList[%%A][64BitUrl]!"
		ECHO PackageList[%%A][32BitUrl:"!PackageList[%%A][32BitUrl]!"

		SET /A "FileListLen=PackageList[%%A][len] - 5"
		FOR /L %%B IN (1,1,!FileListLen!) DO (
			ECHO PackageList[%%A][File%%B][relPath]:"!PackageList[%%A][File%%B][relPath]!"
			ECHO PackageList[%%A][File%%B][name]:"!PackageList[%%A][File%%B][name]!"
		)
		ECHO(
	)
ENDLOCAL
EXIT /B

REM ------------------------ Cleanup ------------------------
:Cleanup
SETLOCAL
	SET "Orig0thParam=%~1"
	IF NOT "!Orig0thParam!"=="!DifferentCmdLine!" (
		2>NUL RD /S /Q "!TmpDird!"
	)
	1>NUL !Chcp! !OldCodePage!
	POPD
ENDLOCAL
EXIT /B 0

REM ------------------------ Anomaly ------------------------
:Anomaly
SETLOCAL
	CALL :ColorEcho INFO def "Please report this error on " 0 0
	<NUL SET/P=!ESC![33;4;1mhttps://www.github.com/CubersHub/PCK_WindowsPackageManager/issues!ESC![0m
	CALL :ColorEcho "" yellow ". This error should never happen." 1 0
	CALL :ColorEcho "" yellow "       Please provide some information about your permissions, the directory this file is in," 1 0
	CALL :ColorEcho "" yellow "       whether you changed the source code or not and other potentially useful information." 1 0
ENDLOCAL
EXIT /B 0

REM ------------------------ Error ------------------------
REM Source: https://stackoverflow.com/a/61782349
:Error - Cleanly exit batch processing, regardless how many CALLs
	CALL :ColorEcho "" white "" 1 0
	CALL :Cleanup "%0"
	CALL :ColorEcho INFO def "Exiting." 1 0
	IF NOT EXIST "!TEMP!\ExitBatchYes.txt" CALL :BuildYes
	1>NUL 2>&1 <"!TEMP!\ExitBatchYes.txt" CALL :CtrlC

:CtrlC
CMD /C EXIT -1073741510

:BuildYes - Establish a Yes file for the language used by the OS
SETLOCAL
	SET "Yes="
	1>NUL COPY NUL "!TmpDir!\ExitBatchYes.txt"
	FOR /F "DELIMS=(/ TOKENS=2" %%Y IN ('^<NUL COPY /-Y NUL "!TmpDir!\ExitBatchYes.txt"') DO (
		IF NOT DEFINED Yes (
			SET "Yes=%%Y"
		)
	)
	>"!TmpDir!\ExitBatchYes.txt" ECHO !Yes!
ENDLOCAL
EXIT /B 0

REM This portion will be decoded with certutil.exe from System32 and saved as REPLVAR.BAT
REM REPLVAR.BAT is a utility to do Find and Replace in Batch variables, see https://www.dostips.com/forum/viewtopic.php?f=3&t=5492
REM The only time it is needed is where equals signs potentially need to be replaced [see MakeParameterSafe function].
-----BEGIN CERTIFICATE-----
QGlmIChAWCk9PShAWSkgQGVuZCAvKiBIYXJtbGVzcyBoeWJyaWQgbGluZSB0aGF0
IGJlZ2lucyBhIEpTY3JpcHQgY29tbWVudA0KDQo6OioqKioqKioqKioqKiBEb2N1
bWVudGF0aW9uICoqKioqKioqKioqDQo6OlJFUExWQVIuQkFUIHZlcnNpb24gMS40
DQo6OjoNCjo6OlJFUExWQVIgIEluVmFyICBPdXRWYXIgIFNlYXJjaCAgUmVwbGFj
ZSAgW09wdGlvbnNdDQo6OjpSRVBMVkFSICAvP1tSRUdFWHxSRVBMQUNFXQ0KOjo6
UkVQTFZBUiAgL1YNCjo6Og0KOjo6ICBQZXJmb3JtcyBhIGdsb2JhbCByZWd1bGFy
IGV4cHJlc3Npb24gc2VhcmNoIGFuZCByZXBsYWNlIG9uIHRoZSBjb250ZW50cyBv
Zg0KOjo6ICB2YXJpYWJsZSBJblZhciBhbmQgd3JpdGVzIHRoZSByZXN1bHQgdG8g
dmFyaWFibGUgT3V0VmFyLg0KOjo6DQo6OjogIFJFUExWQVIuQkFUIHdvcmtzIHBy
b3Blcmx5IHdpdGggZGVsYXllZCBleHBhbnNpb24gZW5hYmxlZCBvciBkaXNhYmxl
ZC4NCjo6Og0KOjo6ICBSRVBMVkFSLkJBVCB0cmVhdHMgdGhlIHNvdXJjZSB2YXJp
YWJsZSB2YWx1ZSBhcyBleHRlbmRlZCBBU0NJSS4gVGhlIHZhbHVlDQo6OjogIHNo
b3VsZCBtYXAgcHJvcGVybHkgdG8gdGhlIGFjdGl2ZSBjb2RlIHBhZ2UuIFVuaWNv
ZGUgc291cmNlIHZhbHVlcyB0aGF0DQo6OjogIGRvIG5vdCBtYXAgdG8gdGhlIGFj
dGl2ZSBjb2RlIHBhZ2Ugd2lsbCBiZSBzaWxlbnRseSB0cmFuc2Zvcm1lZCB0byBh
IG5ldw0KOjo6ICB2YWx1ZSB0aGF0IGRvZXMgbWFwIHRvIHRoZSBhY3RpdmUgY29k
ZSBwYWdlLiBUaGUgcmVzdWx0IG9mIHRoZSBzZWFyY2ggYW5kDQo6OjogIHJlcGxh
Y2UgbXVzdCBiZSBjb21wYXRpYmxlIHdpdGggdGhlIGFjdGl2ZSBjb2RlIHBhZ2Us
IG90aGVyd2lzZSBhbiBlcnJvcg0KOjo6ICBpcyByYWlzZWQuDQo6OjoNCjo6OiAg
VGhlIG1heGltdW0gc3VwcG9ydGVkIG91dHB1dCBzdHJpbmcgbGVuZ3RoIHVzdWFs
bHkgYXBwcm9hY2hlcyB0aGUgODE5MQ0KOjo6ICBtYXhpbXVtIGZvciBtb3N0IHN0
cmluZ3MuIEJ1dCBpdCBjb3VsZCBiZSBzaWduaWZpY2FudGx5IGxlc3MgaWYgdGhl
IG91dHB1dA0KOjo6ICBzdHJpbmcgY29udGFpbnMgbWFueSAlICIgXHIgb3IgXG4g
Y2hhcmFjdGVycywgYXMgdGhleSBtdXN0IGJlIHRlbXBvcmFyaWx5DQo6OjogIGV4
cGFuZGVkIGludG8gMiBvciAzIGJ5dGVzLiBBbHNvLCBeIGFuZCAhIGNoYXJhY3Rl
cnMgYXJlIHRlbXBvcmFyaWx5DQo6OjogIGV4cGFuZGVkIGludG8gMiBieXRlcyBp
ZiBkZWxheWVkIGV4cGFuc2lvbiBpcyBlbmFibGVkLg0KOjo6DQo6OjogIFJFUExW
QVIuQkFUIHJldHVybnMgd2l0aCBFUlJPUkxFVkVMIDAgdXBvbiBzdWNjZXNzLCBh
bmQgRVJST1JMRVZFTCAxDQo6OjogIHVwb24gZXJyb3IuIElmIHRoZSBBIG9wdGlv
biBpcyB1c2VkIGFuZCB0aGUgaW5wdXQgd2FzIG5vdCBhbHRlcmVkIHRoZW4NCjo6
OiAgT3V0VmFyIGlzIHVuZGVmaW5lZCBhbmQgRVJST1JMRVZFTCBzZXQgdG8gMi4N
Cjo6Og0KOjo6ICBFYWNoIHBhcmFtZXRlciBtYXkgYmUgb3B0aW9uYWxseSBlbmNs
b3NlZCBieSBkb3VibGUgcXVvdGVzLiBUaGUgZG91YmxlDQo6OjogIHF1b3RlcyBh
cmUgbm90IGNvbnNpZGVyZWQgcGFydCBvZiB0aGUgYXJndW1lbnQuIFRoZSBxdW90
ZXMgYXJlIHJlcXVpcmVkDQo6OjogIGlmIHRoZSBwYXJhbWV0ZXIgY29udGFpbnMg
YSBiYXRjaCB0b2tlbiBkZWxpbWl0ZXIgbGlrZSBzcGFjZSwgdGFiLCBjb21tYSwN
Cjo6OiAgc2VtaWNvbG9uLiBUaGUgcXVvdGVzIHNob3VsZCBhbHNvIGJlIHVzZWQg
aWYgdGhlIGFyZ3VtZW50IGNvbnRhaW5zIGENCjo6OiAgYmF0Y2ggc3BlY2lhbCBj
aGFyYWN0ZXIgbGlrZSAmLCB8LCBldGMuIHNvIHRoYXQgdGhlIHNwZWNpYWwgY2hh
cmFjdGVyDQo6OjogIGRvZXMgbm90IG5lZWQgdG8gYmUgZXNjYXBlZCB3aXRoIF4u
DQo6OjoNCjo6OiAgSWYgY2FsbGVkIHdpdGggYSBzaW5nbGUgYXJndW1lbnQgb2Yg
Lz8sIHRoZW4gcHJpbnRzIGhlbHAgZG9jdW1lbnRhdGlvbg0KOjo6ICB0byBzdGRv
dXQuIElmIGEgc2luZ2xlIGFyZ3VtZW50IG9mIC8/UkVHRVgsIHRoZW4gb3BlbnMg
dXAgTWljcm9zb2Z0J3MNCjo6OiAgSlNjcmlwdCByZWd1bGFyIGV4cHJlc3Npb24g
ZG9jdW1lbnRhdGlvbiB3aXRoaW4geW91ciBicm93c2VyLiBJZiBhIHNpbmdsZQ0K
Ojo6ICBhcmd1bWVudCBvZiAvP1JFUExBQ0UsIHRoZW4gb3BlbnMgdXAgTWljcm9z
b2Z0J3MgSlNjcmlwdCBSRVBMQUNFDQo6OjogIGRvY3VtZW50YXRpb24gd2l0aGlu
IHlvdXIgYnJvd3Nlci4NCjo6Og0KOjo6ICBJZiBjYWxsZWQgd2l0aCBhIHNpbmds
ZSBhcmd1bWVudCBvZiAvViwgY2FzZSBpbnNlbnNpdGl2ZSwgdGhlbiBwcmludHMN
Cjo6OiAgdGhlIHZlcnNpb24gb2YgUkVQTFZBUi5CQVQuDQo6OjoNCjo6OiAgSW5W
YXIgICAtIFRoZSBuYW1lIG9mIGEgdmFyaWFibGUgY29udGFpbmluZyB0aGUgc291
cmNlIHN0cmluZy4NCjo6Og0KOjo6ICBPdXRWYXIgIC0gVGhlIG5hbWUgb2YgYSB2
YXJpYWJsZSB3aGVyZSB0aGUgcmVzdWx0IHNob3VsZCBiZSBzdG9yZWQuDQo6OjoN
Cjo6OiAgU2VhcmNoICAtIEJ5IGRlZmF1bHQsIHRoaXMgaXMgYSBjYXNlIHNlbnNp
dGl2ZSBKU2NyaXB0IChFQ01BKSByZWd1bGFyDQo6OjogICAgICAgICAgICBleHBy
ZXNzaW9uIGV4cHJlc3NlZCBhcyBhIHN0cmluZy4NCjo6Og0KOjo6ICAgICAgICAg
ICAgVGhlIHNlYXJjaCBpcyBjb25kdWN0ZWQgdXNpbmcgdGhlIHJlZ3VsYXIgZXhw
cmVzc2lvbiBnIChnbG9iYWwpDQo6OjogICAgICAgICAgICBhbmQgbSAobXVsdGls
bGluZSkgZmxhZ3MuDQo6OjoNCjo6OiAgICAgICAgICAgIEpTY3JpcHQgcmVnZXgg
c3ludGF4IGRvY3VtZW50YXRpb24gaXMgYXZhaWxhYmxlIGF0DQo6OjogICAgICAg
ICAgICBodHRwOi8vbXNkbi5taWNyb3NvZnQuY29tL2VuLXVzL2xpYnJhcnkvYWU1
YmY1NDEodj12cy44MCkuYXNweA0KOjo6DQo6OjogIFJlcGxhY2UgLSBCeSBkZWZh
dWx0LCB0aGlzIGlzIHRoZSBzdHJpbmcgdG8gYmUgdXNlZCBhcyBhIHJlcGxhY2Vt
ZW50IGZvcg0KOjo6ICAgICAgICAgICAgZWFjaCBmb3VuZCBzZWFyY2ggZXhwcmVz
c2lvbi4gRnVsbCBzdXBwb3J0IGlzIHByb3ZpZGVkIGZvcg0KOjo6ICAgICAgICAg
ICAgc3Vic3RpdHVpb24gcGF0dGVybnMgYXZhaWxhYmxlIHRvIHRoZSBKU2NyaXB0
IHJlcGxhY2UgbWV0aG9kLg0KOjo6DQo6OjogICAgICAgICAgICBGb3IgZXhhbXBs
ZSwgJCYgcmVwcmVzZW50cyB0aGUgcG9ydGlvbiBvZiB0aGUgc291cmNlIHRoYXQg
bWF0Y2hlZA0KOjo6ICAgICAgICAgICAgdGhlIGVudGlyZSBzZWFyY2ggcGF0dGVy
biwgJDEgcmVwcmVzZW50cyB0aGUgZmlyc3QgY2FwdHVyZWQNCjo6OiAgICAgICAg
ICAgIHN1Ym1hdGNoLCAkMiB0aGUgc2Vjb25kIGNhcHR1cmVkIHN1Ym1hdGNoLCBl
dGMuIEEgJCBsaXRlcmFsDQo6OjogICAgICAgICAgICBjYW4gYmUgZXNjYXBlZCBh
cyAkJC4NCjo6Og0KOjo6ICAgICAgICAgICAgQW4gZW1wdHkgcmVwbGFjZW1lbnQg
c3RyaW5nIG11c3QgYmUgcmVwcmVzZW50ZWQgYXMgIiIuDQo6OjoNCjo6OiAgICAg
ICAgICAgIFJlcGxhY2Ugc3Vic3RpdHV0aW9uIHBhdHRlcm4gc3ludGF4IGlzIGZ1
bGx5IGRvY3VtZW50ZWQgYXQNCjo6OiAgICAgICAgICAgIGh0dHA6Ly9tc2RuLm1p
Y3Jvc29mdC5jb20vZW4tVVMvbGlicmFyeS9lZnk2czNlNih2PXZzLjgwKS5hc3B4
DQo6OjoNCjo6OiAgT3B0aW9ucyAtIEFuIG9wdGlvbmFsIHN0cmluZyBvZiBjaGFy
YWN0ZXJzIHVzZWQgdG8gYWx0ZXIgdGhlIGJlaGF2aW9yDQo6OjogICAgICAgICAg
ICBvZiBSRVBMVkFSLiBUaGUgb3B0aW9uIGNoYXJhY3RlcnMgYXJlIGNhc2UgaW5z
ZW5zaXRpdmUsIGFuZCBtYXkNCjo6OiAgICAgICAgICAgIGFwcGVhciBpbiBhbnkg
b3JkZXIuDQo6OjoNCjo6OiAgICAgICAgICAgIEkgLSBNYWtlcyB0aGUgc2VhcmNo
IGNhc2UtaW5zZW5zaXRpdmUuDQo6OjoNCjo6OiAgICAgICAgICAgIEwgLSBUaGUg
U2VhcmNoIGlzIHRyZWF0ZWQgYXMgYSBzdHJpbmcgbGl0ZXJhbCBpbnN0ZWFkIG9m
IGENCjo6OiAgICAgICAgICAgICAgICByZWd1bGFyIGV4cHJlc3Npb24uIEFsc28s
IGFsbCAkIGZvdW5kIGluIFJlcGxhY2UgYXJlDQo6OjogICAgICAgICAgICAgICAg
dHJlYXRlZCBhcyAkIGxpdGVyYWxzLg0KOjo6DQo6OjogICAgICAgICAgICBCIC0g
VGhlIFNlYXJjaCBtdXN0IG1hdGNoIHRoZSBiZWdpbm5pbmcgb2YgYSBsaW5lLg0K
Ojo6ICAgICAgICAgICAgICAgIE1vc3RseSB1c2VkIHdpdGggbGl0ZXJhbCBzZWFy
Y2hlcy4NCjo6Og0KOjo6ICAgICAgICAgICAgRSAtIFRoZSBTZWFyY2ggbXVzdCBt
YXRjaCB0aGUgZW5kIG9mIGEgbGluZS4NCjo6OiAgICAgICAgICAgICAgICBNb3N0
bHkgdXNlZCB3aXRoIGxpdGVyYWwgc2VhcmNoZXMuDQo6OjoNCjo6OiAgICAgICAg
ICAgIEEgLSBPbmx5IHJldHVybiBhIHZhbHVlIGlmIHRoZSBpbnB1dCB3YXMgYWx0
ZXJlZC4gSWYgbm90IGFsdGVyZWQsDQo6OjogICAgICAgICAgICAgICAgdGhlbiBF
UlJPUkxFVkVMIGlzIHNldCB0byAyLg0KOjo6DQo6OjogICAgICAgICAgICBYIC0g
RW5hYmxlcyBleHRlbmRlZCBzdWJzdGl0dXRpb24gcGF0dGVybiBzeW50YXggd2l0
aCBzdXBwb3J0DQo6OjogICAgICAgICAgICAgICAgZm9yIHRoZSBmb2xsb3dpbmcg
ZXNjYXBlIHNlcXVlbmNlcyB3aXRoaW4gdGhlIFJlcGxhY2Ugc3RyaW5nOg0KOjo6
DQo6OjogICAgICAgICAgICAgICAgXFwgICAgIC0gIEJhY2tzbGFzaA0KOjo6ICAg
ICAgICAgICAgICAgIFxiICAgICAtICBCYWNrc3BhY2UNCjo6OiAgICAgICAgICAg
ICAgICBcZiAgICAgLSAgRm9ybWZlZWQNCjo6OiAgICAgICAgICAgICAgICBcbiAg
ICAgLSAgTmV3bGluZQ0KOjo6ICAgICAgICAgICAgICAgIFxxICAgICAtICBRdW90
ZQ0KOjo6ICAgICAgICAgICAgICAgIFxyICAgICAtICBDYXJyaWFnZSBSZXR1cm4N
Cjo6OiAgICAgICAgICAgICAgICBcdCAgICAgLSAgSG9yaXpvbnRhbCBUYWINCjo6
OiAgICAgICAgICAgICAgICBcdiAgICAgLSAgVmVydGljYWwgVGFiDQo6OjogICAg
ICAgICAgICAgICAgXHhubiAgIC0gIEV4dGVuZGVkIEFTQ0lJIGJ5dGUgY29kZSBl
eHByZXNzZWQgYXMgMiBoZXggZGlnaXRzDQo6OjogICAgICAgICAgICAgICAgXHVu
bm5uIC0gIFVuaWNvZGUgY2hhcmFjdGVyIGV4cHJlc3NlZCBhcyA0IGhleCBkaWdp
dHMNCjo6Og0KOjo6ICAgICAgICAgICAgICAgIEFsc28gZW5hYmxlcyB0aGUgXHEg
ZXNjYXBlIHNlcXVlbmNlIGZvciB0aGUgU2VhcmNoIHN0cmluZy4NCjo6OiAgICAg
ICAgICAgICAgICBUaGUgb3RoZXIgZXNjYXBlIHNlcXVlbmNlcyBhcmUgYWxyZWFk
eSBzdGFuZGFyZCBmb3IgYSByZWd1bGFyDQo6OjogICAgICAgICAgICAgICAgZXhw
cmVzc2lvbiBTZWFyY2ggc3RyaW5nLg0KOjo6DQo6OjogICAgICAgICAgICAgICAg
QWxzbyBtb2RpZmllcyB0aGUgYmVoYXZpb3Igb2YgXHhubiBpbiB0aGUgU2VhcmNo
IHN0cmluZyB0byB3b3JrDQo6OjogICAgICAgICAgICAgICAgcHJvcGVybHkgd2l0
aCBleHRlbmRlZCBBU0NJSSBieXRlIGNvZGVzLg0KOjo6DQo6OjogICAgICAgICAg
ICAgICAgRXh0ZW5kZWQgZXNjYXBlIHNlcXVlbmNlcyBhcmUgc3VwcG9ydGVkIGV2
ZW4gd2hlbiB0aGUgTCBvcHRpb24NCjo6OiAgICAgICAgICAgICAgICBpcyB1c2Vk
LiBCb3RoIFNlYXJjaCBhbmQgUmVwbGFjZSBzdXBwb3J0IGFsbCBvZiB0aGUgZXh0
ZW5kZWQNCjo6OiAgICAgICAgICAgICAgICBlc2NhcGUgc2VxdWVuY2VzIGlmIGJv
dGggdGhlIFggYW5kIEwgb3Bpb25zIGFyZSBjb21iaW5lZC4NCjo6Og0KOjo6IFJF
UExWQVIuQkFUIHdhcyB3cml0dGVuIGJ5IERhdmUgQmVuaGFtLCB3aXRoIGFzc2lz
dGFuY2UgZnJvbSBEb3NUaXBzIHVzZXJzDQo6OjogQWFjaW5pIGFuZCBMaXZpdSBy
ZWdhcmRpbmcgY29tcGxpY2F0aW9ucyBkdWUgdG8gSlNjcmlwdCdzIHVzZSBvZiB1
bmljb2RlIHZzLg0KOjo6IGNtZC5leGUncyB1c2Ugb2YgZXh0ZW5kZWQgQVNDSUku
IFJFUExWQVIuQkFUIGFsc28gdXNlcyBhIG1vZGlmZWQgZm9ybSBvZiB0aGUNCjo6
OiBzYWZlIHJldHVybiB0ZWNobmlxdWUgZGV2ZWxvcGVkIGJ5IERvc1RpcHMgdXNl
ciBqZWIuIFVwZGF0ZXMgdG8gUkVQTFZBUi5CQVQNCjo6OiB3aWxsIGJlIHBvc3Rl
ZCB0byB0aGUgb3JpZ2luYWwgcG9zdGluZyBzaXRlOg0KOjo6IGh0dHA6Ly93d3cu
ZG9zdGlwcy5jb20vZm9ydW0vdmlld3RvcGljLnBocD9mPTMmdD01NDkyDQo6OjoN
Cg0KOjoqKioqKioqKioqKiogQmF0Y2ggcG9ydGlvbiAqKioqKioqKioqKg0KDQpA
ZWNobyBvZmYNCmlmIC4lNCBlcXUgLiAoDQogIGlmICIlfjEiIGVxdSAiLz8iICgN
CiAgICBmb3IgL2YgImRlbGltcz06IHRva2Vucz0xKiIgJSVBIGluICgnZmluZHN0
ciAvbiAiXjo6OiIgIiV+ZjAiJykgZG8gZWNobyglJUINCiAgICBleGl0IC9iIDAN
CiAgKSBlbHNlIGlmIC9pICIlfjEiIGVxdSAiLz9SRUdFWCIgKA0KICAgIHN0YXJ0
ICIiICJodHRwOi8vbXNkbi5taWNyb3NvZnQuY29tL2VuLXVzL2xpYnJhcnkvYWU1
YmY1NDEodj12cy44MCkuYXNweCINCiAgICBleGl0IC9iIDANCiAgKSBlbHNlIGlm
IC9pICIlfjEiIGVxdSAiLz9SRVBMQUNFIiAoDQogICAgc3RhcnQgIiIgImh0dHA6
Ly9tc2RuLm1pY3Jvc29mdC5jb20vZW4tVVMvbGlicmFyeS9lZnk2czNlNih2PXZz
LjgwKS5hc3B4Ig0KICAgIGV4aXQgL2IgMA0KICApIGVsc2UgaWYgL2kgIiV+MSIg
ZXF1ICIvViIgKA0KICAgIGZvciAvZiAiZGVsaW1zPTogdG9rZW5zPTEqIiAlJUEg
aW4gKCdmaW5kc3RyIC9uYmxjOiI6OlJFUExWQVIuQkFUIHZlcnNpb24iICIlfmYw
IicpIGRvIGVjaG8oJSVCDQogICAgZXhpdCAvYiAwDQogICkgZWxzZSAoDQogICAg
Y2FsbCA6ZXJyICJJbnN1ZmZpY2llbnQgYXJndW1lbnRzIg0KICAgIGV4aXQgL2Ig
MQ0KICApDQopDQplY2hvKCV+NXxmaW5kc3RyIC9pICJbXklMRUJYQV0iID5udWwg
JiYgKA0KICBjYWxsIDplcnIgIkludmFsaWQgb3B0aW9uKHMpIg0KICBleGl0IC9i
IDENCikNCg0Kc2V0bG9jYWwNCnNldCAiJHJlcGxWYXIubm90RGVsYXllZD0hISIN
CnNldGxvY2FsIGVuYWJsZURlbGF5ZWRFeHBhbnNpb24NCmZvciAvZiAiZGVsaW1z
PT0iICUlViBpbiAoJ3NldCB+IDJePm51bCcpIGRvIHNldCAiJSVWPSINCnNldCAi
fj0hJX4xISINCnNldGxvY2FsIGRpc2FibGVEZWxheWVkRXhwYW5zaW9uDQpzZXQg
InJ0bj0iDQpmb3IgL2YgZGVsaW1zXj1eIGVvbF49ICUlQSBpbiAoDQogICdzZXQg
fiAyXj5udWxefGNzY3JpcHQgLy9FOkpTY3JpcHQgLy9ub2xvZ28gIiV+ZjAiICIl
JHJlcGxWYXIubm90RGVsYXllZCUiICUzICU0ICU1Jw0KKSBkbyBzZXQgInJ0bj0l
JUEiDQppZiBkZWZpbmVkIHJ0biAoDQogIHNldCAiZXJyPSVydG46fjAsMSUiDQog
IHNldCAicnRuPSVydG46fjElIg0KKSBlbHNlIHNldCAiZXJyPTIiDQppZiAlZXJy
JSBlcXUgMSAoZWNobyBFUlJPUjogUmVzdWx0IG5vdCBjb21wYXRpYmxlIHdpdGgg
YWN0aXZlIGNvZGUgcGFnZSkgPiYyDQppZiAlZXJyJSBlcXUgMiAoZWNobyBJbnB1
dCBub3QgYWx0ZXJlZCkgPiYyDQpzZXRsb2NhbCBlbmFibGVEZWxheWVkRXhwYW5z
aW9uDQpzZXQgXiJMRj1eDQoNCl4iDQpmb3IgL2YgJSVBIGluICgnY29weSAveiAi
JX5kcGYwIiBudWwnKSBkbyBzZXQgIkNSPSUlQSINCnNldCAicmVwbGFjZT0lJSAi
IiIgIUNSISFDUiEiDQpmb3IgL2YgInRva2Vucz0xLDIsMyIgJSVKIGluICgiIXJl
cGxhY2UhIikgZG8gZm9yICUlTSBpbiAoIiFMRiEiKSBkbyAoDQogIGVuZGxvY2Fs
DQogIGVuZGxvY2FsDQogIGVuZGxvY2FsDQogIGVuZGxvY2FsDQogIHNldCAiJX4y
PSVydG4lIiAhDQogIGV4aXQgL2IgJWVyciUNCikNCg0KOmVycg0KPiYyIGVjaG8g
RVJST1I6ICV+MS4gVXNlIHJlcGxWYXIgLz8gdG8gZ2V0IGhlbHAuDQpleGl0IC9i
DQoNCg0KKioqKioqKioqKioqKiBKU2NyaXB0IHBvcnRpb24gKioqKioqKioqKi8N
CnZhciBlbnY9V1NjcmlwdC5DcmVhdGVPYmplY3QoIldTY3JpcHQuU2hlbGwiKS5F
bnZpcm9ubWVudCgiUHJvY2VzcyIpOw0KdmFyIGFyZ3M9V1NjcmlwdC5Bcmd1bWVu
dHM7DQp2YXIgc2VhcmNoPWFyZ3MuSXRlbSgxKTsNCnZhciByZXBsYWNlPWFyZ3Mu
SXRlbSgyKTsNCnZhciBvcHRpb25zPSJnbSI7DQppZiAoYXJncy5sZW5ndGg+Mykg
b3B0aW9ucys9YXJncy5JdGVtKDMpLnRvTG93ZXJDYXNlKCk7DQp2YXIgYWx0ZXJh
dGlvbnM9KG9wdGlvbnMuaW5kZXhPZigiYSIpPj0wKTsNCmlmIChhbHRlcmF0aW9u
cykgb3B0aW9ucz1vcHRpb25zLnJlcGxhY2UoL2EvZywiIik7DQppZiAob3B0aW9u
cy5pbmRleE9mKCJ4Iik+PTApIHsNCiAgb3B0aW9ucz1vcHRpb25zLnJlcGxhY2Uo
L3gvZywiIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXFxcL2csIlxc
QiIpOw0KICByZXBsYWNlPXJlcGxhY2UucmVwbGFjZSgvXFxxL2csIlwiIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4MC9nLCJcXHUyMEFDIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4Mi9nLCJcXHUyMDFBIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4My9nLCJcXHUwMTkyIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4NC9nLCJcXHUyMDFFIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4NS9nLCJcXHUyMDI2Iik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4Ni9nLCJcXHUyMDIwIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4Ny9nLCJcXHUyMDIxIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4OC9nLCJcXHUwMkM2Iik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4OS9nLCJcXHUyMDMwIik7DQog
IHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4W2FBXS9nLCJcXHUwMTYwIik7
DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4W2JCXS9nLCJcXHUyMDM5
Iik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4W2NDXS9nLCJcXHUw
MTUyIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg4W2VFXS9nLCJc
XHUwMTdEIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5MS9nLCJc
XHUyMDE4Iik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5Mi9nLCJc
XHUyMDE5Iik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5My9nLCJc
XHUyMDFDIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5NC9nLCJc
XHUyMDFEIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5NS9nLCJc
XHUyMDIyIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5Ni9nLCJc
XHUyMDEzIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5Ny9nLCJc
XHUyMDE0Iik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5OC9nLCJc
XHUwMkRDIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5OS9nLCJc
XHUyMTIyIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5W2FBXS9n
LCJcXHUwMTYxIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5W2JC
XS9nLCJcXHUyMDNBIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHg5
W2NDXS9nLCJcXHUwMTUzIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9c
XHg5W2REXS9nLCJcXHUwMDlEIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNl
KC9cXHg5W2VFXS9nLCJcXHUwMTdFIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBs
YWNlKC9cXHg5W2ZGXS9nLCJcXHUwMTc4Iik7DQogIHJlcGxhY2U9cmVwbGFjZS5y
ZXBsYWNlKC9cXGIvZywiXGIiKTsNCiAgcmVwbGFjZT1yZXBsYWNlLnJlcGxhY2Uo
L1xcZi9nLCJcZiIpOw0KICByZXBsYWNlPXJlcGxhY2UucmVwbGFjZSgvXFxuL2cs
IlxuIik7DQogIHJlcGxhY2U9cmVwbGFjZS5yZXBsYWNlKC9cXHIvZywiXHIiKTsN
CiAgcmVwbGFjZT1yZXBsYWNlLnJlcGxhY2UoL1xcdC9nLCJcdCIpOw0KICByZXBs
YWNlPXJlcGxhY2UucmVwbGFjZSgvXFx2L2csIlx2Iik7DQogIHJlcGxhY2U9cmVw
bGFjZS5yZXBsYWNlKC9cXHhbMC05YS1mQS1GXXsyfXxcXHVbMC05YS1mQS1GXXs0
fS9nLA0KICAgIGZ1bmN0aW9uKCQwLCQxLCQyKXsNCiAgICAgIHJldHVybiBTdHJp
bmcuZnJvbUNoYXJDb2RlKHBhcnNlSW50KCIweCIrJDAuc3Vic3RyaW5nKDIpKSk7
DQogICAgfQ0KICApOw0KICByZXBsYWNlPXJlcGxhY2UucmVwbGFjZSgvXFxCL2cs
IlxcIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFxcXC9nLCJcXEIiKTsN
CiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9cXHEvZywiXCIiKTsNCiAgc2VhcmNo
PXNlYXJjaC5yZXBsYWNlKC9cXHg4MC9nLCJcXHUyMEFDIik7DQogIHNlYXJjaD1z
ZWFyY2gucmVwbGFjZSgvXFx4ODIvZywiXFx1MjAxQSIpOw0KICBzZWFyY2g9c2Vh
cmNoLnJlcGxhY2UoL1xceDgzL2csIlxcdTAxOTIiKTsNCiAgc2VhcmNoPXNlYXJj
aC5yZXBsYWNlKC9cXHg4NC9nLCJcXHUyMDFFIik7DQogIHNlYXJjaD1zZWFyY2gu
cmVwbGFjZSgvXFx4ODUvZywiXFx1MjAyNiIpOw0KICBzZWFyY2g9c2VhcmNoLnJl
cGxhY2UoL1xceDg2L2csIlxcdTIwMjAiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBs
YWNlKC9cXHg4Ny9nLCJcXHUyMDIxIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFj
ZSgvXFx4ODgvZywiXFx1MDJDNiIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2Uo
L1xceDg5L2csIlxcdTIwMzAiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9c
XHg4W2FBXS9nLCJcXHUwMTYwIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgv
XFx4OFtiQl0vZywiXFx1MjAzOSIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2Uo
L1xceDhbY0NdL2csIlxcdTAxNTIiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNl
KC9cXHg4W2VFXS9nLCJcXHUwMTdEIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFj
ZSgvXFx4OTEvZywiXFx1MjAxOCIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2Uo
L1xceDkyL2csIlxcdTIwMTkiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9c
XHg5My9nLCJcXHUyMDFDIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFx4
OTQvZywiXFx1MjAxRCIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2UoL1xceDk1
L2csIlxcdTIwMjIiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9cXHg5Ni9n
LCJcXHUyMDEzIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFx4OTcvZywi
XFx1MjAxNCIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2UoL1xceDk4L2csIlxc
dTAyREMiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9cXHg5OS9nLCJcXHUy
MTIyIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFx4OVthQV0vZywiXFx1
MDE2MSIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2UoL1xceDlbYkJdL2csIlxc
dTIwM0EiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9cXHg5W2NDXS9nLCJc
XHUwMTUzIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFx4OVtkRF0vZywi
XFx1MDA5RCIpOw0KICBzZWFyY2g9c2VhcmNoLnJlcGxhY2UoL1xceDlbZUVdL2cs
IlxcdTAxN0UiKTsNCiAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9cXHg5W2ZGXS9n
LCJcXHUwMTc4Iik7DQogIGlmIChvcHRpb25zLmluZGV4T2YoImwiKT49MCkgew0K
ICAgIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFxiL2csIlxiIik7DQogICAgc2Vh
cmNoPXNlYXJjaC5yZXBsYWNlKC9cXGYvZywiXGYiKTsNCiAgICBzZWFyY2g9c2Vh
cmNoLnJlcGxhY2UoL1xcbi9nLCJcbiIpOw0KICAgIHNlYXJjaD1zZWFyY2gucmVw
bGFjZSgvXFxyL2csIlxyIik7DQogICAgc2VhcmNoPXNlYXJjaC5yZXBsYWNlKC9c
XHQvZywiXHQiKTsNCiAgICBzZWFyY2g9c2VhcmNoLnJlcGxhY2UoL1xcdi9nLCJc
diIpOw0KICAgIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvXFx4WzAtOWEtZkEtRl17
Mn18XFx1WzAtOWEtZkEtRl17NH0vZywNCiAgICAgIGZ1bmN0aW9uKCQwLCQxLCQy
KXsNCiAgICAgICAgcmV0dXJuIFN0cmluZy5mcm9tQ2hhckNvZGUocGFyc2VJbnQo
IjB4IiskMC5zdWJzdHJpbmcoMikpKTsNCiAgICAgIH0NCiAgICApOw0KICAgIHNl
YXJjaD1zZWFyY2gucmVwbGFjZSgvXFxCL2csIlxcIik7DQogIH0gZWxzZSBzZWFy
Y2g9c2VhcmNoLnJlcGxhY2UoL1xcQi9nLCJcXFxcIik7DQp9DQppZiAob3B0aW9u
cy5pbmRleE9mKCJsIik+PTApIHsNCiAgb3B0aW9ucz1vcHRpb25zLnJlcGxhY2Uo
L2wvZywiIik7DQogIHNlYXJjaD1zZWFyY2gucmVwbGFjZSgvKFsuXiQqKz8oKVt7
XFx8XSkvZywiXFwkMSIpOw0KICByZXBsYWNlPXJlcGxhY2UucmVwbGFjZSgvXCQv
ZywiJCQkJCIpOw0KfQ0KaWYgKG9wdGlvbnMuaW5kZXhPZigiYiIpPj0wKSB7DQog
IG9wdGlvbnM9b3B0aW9ucy5yZXBsYWNlKC9iL2csIiIpOw0KICBzZWFyY2g9Il4i
K3NlYXJjaA0KfQ0KaWYgKG9wdGlvbnMuaW5kZXhPZigiZSIpPj0wKSB7DQogIG9w
dGlvbnM9b3B0aW9ucy5yZXBsYWNlKC9lL2csIiIpOw0KICBzZWFyY2g9c2VhcmNo
KyIkIg0KfQ0KdmFyIHNlYXJjaD1uZXcgUmVnRXhwKHNlYXJjaCxvcHRpb25zKTsN
Cg0KdmFyIHN0cjEsIHN0cjIsIGRlbGF5Ow0KZGVsYXk9YXJncy5JdGVtKDApOw0K
DQppZiAoIVdTY3JpcHQuU3RkSW4uQXRFbmRPZlN0cmVhbSkgc3RyMT1XU2NyaXB0
LlN0ZEluLlJlYWRBbGwoKTsgZWxzZSBzdHIxPSIiOw0Kc3RyMT1zdHIxLnN1YnN0
cigyLHN0cjEubGVuZ3RoLTQpOw0Kc3RyMj1zdHIxLnJlcGxhY2Uoc2VhcmNoLHJl
cGxhY2UpOw0KaWYgKCFhbHRlcmF0aW9ucyB8fCBzdHIxIT1zdHIyKSB7DQogIHN0
cjI9c3RyMi5yZXBsYWNlKC8lL2csIiVKIik7DQogIHN0cjI9c3RyMi5yZXBsYWNl
KC9cIi9nLCIlfksiKTsNCiAgc3RyMj1zdHIyLnJlcGxhY2UoL1xyL2csIiVMIik7
DQogIHN0cjI9c3RyMi5yZXBsYWNlKC9cbi9nLCIlfk0iKTsNCiAgaWYgKGRlbGF5
PT0iIikgew0KICAgIHN0cjI9c3RyMi5yZXBsYWNlKC9cXi9nLCJeXiIpOw0KICAg
IHN0cjI9c3RyMi5yZXBsYWNlKC8hL2csIl4hIik7DQogIH0NCiAgdHJ5IHsNCiAg
ICBXU2NyaXB0LlN0ZG91dC5Xcml0ZSgiMCIrc3RyMik7DQogIH0gY2F0Y2ggKGUp
IHsNCiAgICBXU2NyaXB0LlN0ZG91dC5Xcml0ZSgiMSIpOw0KICB9DQp9
-----END CERTIFICATE-----

REM Ideas/TODOs:
REM Get latest Github Releases: (FOR /F "!SKIP! TOKENS=1* DELIMS=: " %A IN ('curl --silent --location "https://api.github.com/repos/telegramdesktop/tdesktop/releases/latest" ^| !Findstr! /R "browser_download_url.*zip"') DO @(IF NOT DEFINED URL SET "URL=%~B")) & ECHO !URL! & SET URL=
REM Save last direct download link in installation folder to check for new (different) version
REM Sourceforge API /best_release.json
REM Find programs instead of programworks
REM Get rid of REPLVAR.BAT
