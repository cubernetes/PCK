::bat
:: PCK Package Manager for Windows
:: https://github.com/cubernetes/PCK-Package-Manager

@ECHO OFF
SETLOCAL EnableDelayedExpansion EnableExtensions

CALL :Init "%~1"

CALL :Main "%~0" %*

ENDLOCAL & SET "PATH=%PATH%" & SET "PATHEXT=%PATHEXT%"
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

	IF "!Arg1!"=="help" (
		CALL :WarnAboutIgnoredArgs 2 %*
		CALL :ShowHelp
		GOTO :Finish
	)

	REM --------------------------- List packages ---------------------------
	IF "!Arg1!"=="list" (
		CALL :WarnAboutIgnoredArgs 3 %*
		IF DEFINED Arg2 (
			IF "!Arg2:~0,1!"=="-" (
				CALL :ColorEcho ERROR def 1 0 "Your regex starts with a minus ("-"). Please escape it using a backslash ("\")."
				CALL :ColorEcho INFO def 1 0 "The reason for this error is unclear to me, but you know, it's batch."
				GOTO :Error
			)
			CALL :ColorEcho ACTION def 1 1 "Showing all available packages that match the regex `!Arg2!`."
			FOR /F "TOKENS=1,2 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!"  ^| "!Findstr!" /I /R "^^[!AlnumCharClass!]*!Arg2![!AlnumCharClass!]*;" ^| "!Sort!"') DO @(
				IF NOT EXIST "!BaseDir!\%%~A%%~B" (
					ECHO - %%~A
				) ELSE (
					ECHO %ESC%[32m- %%~A [already installed]%ESC%[0m
				)
			)
		) ELSE (
			CALL :ColorEcho ACTION def 1 1 "Showing all available packages."
			FOR /F "TOKENS=1,2 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| "!Sort!"') DO (
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
		CALL :WarnAboutIgnoredArgs 4 %*
		CALL :ShowInformation "!Arg2!" "!Arg3!"
		GOTO :Finish
	)

	IF "!Arg1!"=="r" (
		CALL :WarnAboutIgnoredArgs 3 %*
		CALL :ColorEcho INFO def 1 1 "This functionality will uninstall a package."
		GOTO :Finish
	)

	IF "!Arg1!"=="u" (
		CALL :WarnAboutIgnoredArgs 4 %*
		CALL :ColorEcho INFO def 1 1 "This functionality will update a package's link if possible."
		GOTO :Finish
	)

	IF "!Arg1!"=="a" (
		CALL :WarnAboutIgnoredArgs 4 %*
		CALL :ColorEcho INFO def 1 1 "This functionality will add a package to the list if possible."
		GOTO :Finish
	)

	REM --------------------------- Show Usage ---------------------------
	IF NOT DEFINED Arg1 (
		CALL :ShowHelp
		GOTO :Finish
	)

	CALL :WarnAboutIgnoredArgs 1 %*
	REM --------------------------- Install package ---------------------------
	FOR /F "TOKENS=1,2,3,4,5 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| "!Sort!"') DO (

		IF "%%~A"=="!Arg1!" (

			SET "Name=%%~A"
			SET "RelPath=%%~B"
			SET "URL=%%~C"
			SET "URL32bit=%%~D"

			IF NOT EXIST "!BaseDir!\!Name!!RelPath!" (

				CALL :ColorEcho INFO def 1 1 "Package "!Arg1!" found!"

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
					CALL :ColorEcho ERROR def 1 0 "There was no compatible version found for your machine."
					CMD /C ^""!DifferentCmdLine!" i "!Name!"^"
					GOTO :Error
				)

				SET /A "CanUse64Bit=a & b"

				IF "!CanUse64Bit!"=="1" (
					CALL :ColorEcho ACTION def 1 0 "Getting 64 bit version of "!Arg1!"."
					CALL :Download LastRedirectURL "!TmpDir!\!Name!.!FileExtension!"
					SET "Extension=!FileExtension!"
				) ELSE (
					CALL :ColorEcho ACTION def 1 0 "Getting 32 bit version of "!Arg1!"."
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
				CALL :ColorEcho INFO def 1 1 "!Name! is already installed in "!BaseDir!\!Name!". Use it with "!BaseDir!\!Name!!RelPath!"."
				GOTO :Finish
			)
		)
	)
	CALL :ColorEcho ERROR def 1 0 "That package does not exist"
	GOTO :Error

	:Finish
	CALL :Cleanup "%~0"
ENDLOCAL
EXIT /B 0

REM ------------------------ Init ------------------------
:Init
	SET "Arg1=%~1"

	SET "System32=!SystemRoot!\System32"

	REM Source: https://stackoverflow.com/a/59874436
	REM Create escape character for ANSI escape sequences.
	REM Only line of code where the part after DO can not be in paranthesis.
	FOR /F "DELIMS=#" %%E IN ('"PROMPT #$E# & FOR %%E IN (1) DO REM"') DO (SET "ESC=%%E")

	CALL :GetProgramPath where "!System32!" Where
	CALL :GetProgramPath chcp "!System32!" Chcp
	CALL :GetProgramPath sort "!System32!" Sort
	CALL :GetProgramPath findstr "!System32!" Findstr
	CALL :GetProgramPath cscript "!System32!" Cscript
	CALL :GetProgramPath replace "!System32!" Replace

	FOR /F "TOKENS=3 DELIMS=. " %%A IN ('^""!Chcp!"^"') DO (SET "OldCodePage=%%A")

	1>NUL "!Chcp!" 1252
	CALL :GetProgramPath powershell "!System32!\WindowsPowerShell\v1.0" Powershell
	1>NUL "!Chcp!" 65001

	REM Remove trailing backslash "\"
	REM Source: https://stackoverflow.com/a/60414485/13823467
	FOR %%A IN ("%~dp0\..") DO (SET "BaseDir=%%~fA")
	FOR %%A IN ("%~dp0\.") DO (SET "PckDir=%%~fA")

	SET "RedirectsDir=!BaseDir!\Redirects"
	SET "TmpDir=!BaseDir!\tmp"

	CALL :CreateFolder "!RedirectsDir!"
	CALL :CreateFolder "!TmpDir!"

	SET "PackagesFilePath=!PckDir!\packages.csv"

	SET SingleQuote="
	REM "
	SET "DifferentCmdLine=!PckDir!\.\%~nx0"

	SET "Verbose=0"

	CALL :DefineMacros

	CALL :UpdatePath
	CALL :UpdatePathExt

	IF DEFINED PROGRAMFILES(X86) (
	    SET "Architecture=64"
	) ELSE (
	    SET "Architecture=32"
	)

	REM Create carriage return character for generating live download info.
	SET "CR=" & IF NOT DEFINED CR FOR /F "SKIP=1" %%C IN ('ECHO(^|!Replace! ? . /W /U') DO (SET "CR=%%C")

	REM Needed for live download info.
	SET "WhiteSpaceBuffer=                                      "

	REM For regex
	SET "AlnumCharClass=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

	CALL :CreateShortcutWithVbs "!BaseDir!" "!RedirectsDir!\spck.lnk"

	REM Curl is needed for various tasks like fetching information for the packages,
	REM determining the file sizes or determining the file types/extensions.

	%$ToLower% Arg1
	IF NOT "!Arg1!"=="curl" (
		IF NOT "!Arg1!"=="7zip" (
			CALL :GetProgramPath curl "!System32!" Curl
			IF "!Curl!"=="curl" (
				CALL :DownloadDependency curl
			)
		)
	)

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
SET "$Split="
SET "$Trim="
SET "$ToLower="
ENDLOCAL & FOR /F %%! IN ("! ! ^^^!") DO (REM "
REM --------------------- Split Macro ---------------------
REM Multiple delimiters are ignored
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
ENDLOCAL^&ENDLOCAL^&SET "%%J[len]=%%!i%%!"^)%\n%
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
) & SET ^"LF=^%LF%%LF%"
REM "
EXIT /B 0

REM ------------------------ ColorEcho ------------------------
:ColorEcho
SETLOCAL
	REM Needed to print excalamation points.
	SETLOCAL DisableDelayedExpansion
	SET "Type=[%~1] "
	SET "Clr=%~2"
	SET "AddNewline=%~3"
	SET "ComplyWithVerboseSetting=%~4"
	FOR /F "TOKENS=4*" %%A IN ('ECHO %*') DO (SET "Message=%%~B")

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
	SET "Offline=%~2"

	IF DEFINED Offline (
		IF NOT "!Offline!"=="offline" (
			CALL :ColorEcho WARNING def 1 0 "Ignoring unknown argument `!Offline!`."
			SET "Offline="
		)
	)

	FOR /F "TOKENS=1,2,3,4,5 EOL=# DELIMS=; " %%A IN ('TYPE "!PackagesFilePath!" ^| "!Sort!"') DO (
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

			IF NOT DEFINED Offline (
				CALL :FollowURL URL LastRedirectURL
				CALL :FollowURL URL32bit LastRedirectURL32bit

				CALL :GetFileSize LastRedirectURL FileSize FileSizeMB
				CALL :GetFileSize LastRedirectURL32bit FileSize32bit FileSizeMB32bit

				SET "FileSize=~!FileSizeMB! MB (!FileSize! Bytes)"
				SET "FileSize32bit=~!FileSizeMB32bit! MB (!FileSize32bit! Bytes)"

				CALL :DetermineFileExtensionOfURL LastRedirectURL FileExtension
				CALL :DetermineFileExtensionOfURL LastRedirectURL32bit FileExtension32bit
			)

			CALL :ColorEcho ACTION def 1 1 "Showing information for "!Pkg!"."
			CALL :ColorEcho "" white 1 0 ""
			CALL :ColorEcho "" cyan 1 0 "### General Information ###"
			ECHO     Name/Runstring:                            !ESC![4m!Name!!ESC![0m
			ECHO     Relative path to binary or folder:         !ESC![4m!RelPath!!ESC![0m
			ECHO     Where to find new download links:          !ESC![4m!WhereToFindURLs!!ESC![0m
			CALL :ColorEcho "" cyan 1 0 "### 64 bit ###"
			ECHO     Download link:                             !ESC![4m!URL!!ESC![0m
			IF NOT DEFINED Offline (
				ECHO     Direct download link ^(derived from above^): !ESC![32;4m!LastRedirectURL!!ESC![0m
				ECHO     File size:                                 !ESC![4m!FileSize!!ESC![0m
				ECHO     File type:                                 !ESC![4m!FileExtension!!ESC![0m
			)
			CALL :ColorEcho "" cyan 1 0 "### 32 bit ###"
			ECHO     Download link:                             !ESC![4m!URL32bit!!ESC![0m
			IF NOT DEFINED Offline (
				ECHO     Direct download link ^(derived from above^): !ESC![32;4m!LastRedirectURL32bit!!ESC![0m
				ECHO     File size:                                 !ESC![4m!FileSize32bit!!ESC![0m
				ECHO     File type:                                 !ESC![4m!FileExtension32bit!!ESC![0m
			)

			IF DEFINED Offline (
				CALL :ColorEcho "" white 1 1 ""
				CALL :ColorEcho INFO def 1 1 "There was information left out because of the offline argument."
			)

			IF NOT "!Pkg!"=="all" (
				ENDLOCAL
				EXIT /B 0
			)
		)
		ENDLOCAL
	)
	IF NOT "!Package!"=="all" (
		CALL :ColorEcho ERROR def 1 0 "The package "!Package!" could not be found in "!PackagesFilePath!"."
		GOTO :Error
	) ELSE (
		CALL :ColorEcho INFO def 1 1 "Successfully showed information for all packages."
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
		CALL :ColorEcho ACTION def 1 0 "Downloading !FileName! with curl:"
		CALL :DownloadWithCurl "!URLReference!" "!DestFile!"
	) ELSE (
		CALL :ColorEcho ACTION def 1 0 "Downloading !FileName! with VBScript:"
		CALL :DownloadWithVbs "!URLReference!" "!DestFile!"
	)
	IF EXIST "!DestFile!" (
		CALL :ColorEcho SUCCESS def 1 0 "Successfully downloaded !FileName!^!"
	) ELSE (
		CALL :ColorEcho FAIL def 1 0 "Failed^! Could not download !FileName!^!"
		CALL :ColorEcho ERROR def 1 0 "!FileName! could not be downloaded from this URL: !%URLReference%!. Please check if the URL is active."
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
	1>NUL "!Chcp!" 1252
	"!Powershell!" -Command Remove-Item Alias:curl; curl --location """!URL!""" --output """!DestFile!""" 2^>^&1 ^| Select-String k ^| Foreach-Object {$data = (([string] $_).Trim() -Split '\s+')[0,1,3,9]; Write-Host -NoNewline """[PROGRESS] !ESC![37;1;4m$($data[0])%%!ESC![0m: $($data[2])B / $($data[1])B. Elapsed: $($data[3]).!WhiteSpaceBuffer!!CR!"""}; $FinishedAt=cmd.exe '/V /C SET "TmpTIME=^^^!TIME:~,-3^^^!"^&SET "FinishedAt=^^^!TmpTIME: =0^^^!, ^^^!DATE^^^!"^&ECHO ^^^!FinishedAt^^^!'; $FinishedAt=$FinishedAt.Trim().Replace('""""', ''); Write-Host """!ESC![32m[PROGRESS] !ESC![32;1;4m100%%!ESC![0m!ESC![32m: $($data[2])B / $($data[1])B. Elapsed: $($data[3]). Finished at $($FinishedAt).!ESC![0m"""
	1>NUL "!Chcp!" 65001
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

	"!Cscript!" //NOLOGO "!TmpDir!\DownloadWithVbs.vbs"
ENDLOCAL
EXIT /B 0

REM ------------------------ DownloadDependency ------------------------
:DownloadDependency
SETLOCAL
	SET "Package=%~1"
	CALL :ColorEcho ACTION def 1 1 "!Package! will now be downloaded in a separate process."
	CALL :ColorEcho "" white 1 1 ""
	CALL :ColorEcho "" white 1 1 "[-----Installing dependency: !Package!-----]"
	CMD /C ^""!DifferentCmdLine!" !Package!^"
	CALL :ColorEcho "" white 1 1 "[------Done------]"
	CALL :ColorEcho "" white 1 1 ""
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
		CALL :ColorEcho WARNING def 1 0 "7z was NOT found"
		CALL :DownloadDependency 7zip
	)
	CALL :ProgramWorks "7z"
	IF NOT "!ERRORLEVEL!"=="0" (
		CALL :ColorEcho WARNING def 1 0 "7z still not found."
		CALL :ColorEcho ACTION def 1 0 "Unzipping !FileName! with VBScript instead:"
		CALL :UnzipWithVbs "!SrcFile!" "!DestFolder!"
	) ELSE (
		CALL :ColorEcho ACTION def 1 0 "Unzipping !FileName! with 7z:"
		7z x -bsp1 -o"!DestFolder!" "!SrcFile!"
	)

	SET "ItemCount=0"
	FOR /F %%A IN ('DIR "!DestFolder!" /A /B') DO (1>NUL SET /A "ItemCount+=1")

	IF NOT "!ItemCount!"=="0" (
		CALL :ColorEcho SUCCESS def 1 1 "Successfully unzipped "!SrcFile!" to "!DestFolder!"^!"
	) ELSE (
		CALL :ColorEcho ERROR def 1 1 "Could not unzip "!SrcFile!" to "!DestFolder!"."
		GOTO :Error		
	)
	CALL :RemoveNestedFolderStructure "!DestFolder!" "!DestFolder!"
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

	"!Cscript!" //NOLOGO "!TmpDir!\Unzip.vbs"
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
	SET "TargetFolder=%~f1"
	SET "DestFolder=%~f2"

	SET "ItemCount=0"
	FOR /F %%A IN ('DIR "!TargetFolder!" /A /B') DO (1>NUL SET /A "ItemCount+=1")

	IF !ItemCount! GEQ 2 (
		IF NOT "!TargetFolder!"=="!DestFolder!" (
			CALL :MoveFilesWithVbsAndDeleteDirWrapper "!TargetFolder!" "!DestFolder!"
		)
	) ELSE IF !ItemCount! EQU 1 (
		SET "Directory=NotADirectory"
		FOR /F "DELIMS=" %%A IN ('DIR "!TargetFolder!" /AD /B') DO (SET "Directory=%%A")
		IF NOT "!Directory!"=="NotADirectory" (
			CALL :RemoveNestedFolderStructure "!TargetFolder!\!Directory!" "!DestFolder!"
		) ELSE IF NOT "!TargetFolder!"=="!DestFolder!" (
			CALL :MoveFilesWithVbsAndDeleteDirWrapper "!TargetFolder!" "!DestFolder!"
		)
	) ELSE (
		ENDLOCAL
		EXIT /B 1
	)

ENDLOCAL
EXIT /B 0

REM ------------------------ MoveFilesWithVbsAndDeleteDirWrapper ------------------------
:MoveFilesWithVbsAndDeleteDirWrapper
SETLOCAL
	SET "SrcFolder=%~1"
	SET "DestFolder=%~2"

	CALL :ColorEcho INFO def 1 1 "The contents of the zip file need to be moved up one folder."
	CALL :ColorEcho ACTION def 1 1 "Moving the contents of "!SrcFolder!" to "!DestFolder!" with VBScript:"
	CALL :MoveFilesWithVbsAndDeleteDir "!SrcFolder!" "!DestFolder!"
	CALL :ColorEcho SUCCESS def 1 1 "Successfully moved the contents one folder up^!"
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

	"!Cscript!" //NOLOGO "!TmpDir!\MoveFilesWithVbsAndDeleteDir.vbs"
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

	"!Cscript!" //NOLOGO "!TmpDir!\CreateShortcut.vbs"
ENDLOCAL
EXIT /B 0

REM ------------------------ CreateFolder ------------------------
:CreateFolder
SETLOCAL
	SET "DestFolder=%~1"
	IF NOT EXIST "!DestFolder!" (
		CALL :ColorEcho ACTION def 0 1 "Creating directory "!DestFolder!"..."
		2>NUL MD "!DestFolder!"
		IF NOT EXIST "!DestFolder!" (
			CALL :ColorEcho "" red 1 1 " Failed^!"
			CALL :ColorEcho ERROR def 1 0 "Directory "!DestFolder!" could not be created. Run as admin or in a different location."
			GOTO :Error
		)
		CALL :ColorEcho "" green 1 1 " Success^!"
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ GetFileSize ------------------------
:GetFileSize
SETLOCAL
	SET "URL=!%~1!"
	SET "ReturnVar1=%~2"
	SET "ReturnVar2=%~3"

	FOR /F "TOKENS=2 DELIMS= " %%A IN ('2^>NUL "!Curl!" --head --silent --location "!URL!" ^| "!Findstr!" "Content-Length: "') DO (SET "FileSize=%%A")
	>NUL SET /A "FileSize=FileSize / 1"
	>NUL SET /A "FileSizeMB=FileSize / 1000000"
ENDLOCAL & SET "%ReturnVar1%=%FileSize%" & SET "%ReturnVar2%=%FileSizeMB%"
EXIT /B 0


REM ------------------------ DetermineFileExtensionOfURL ------------------------
:DetermineFileExtensionOfURL
SETLOCAL
	SET "URL=!%~1!"
	SET "ReturnVar1=%~2"

	SET "TmpFile=!TmpDir!\determineFileExtension.extension"

	2>NUL DEL "!TmpFile!"
	1>NUL "!Chcp!" 1252
	"!Powershell!" -Command Remove-Item Alias:curl; curl --location --range 0-32 """!URL!""" --output """!TmpFile!""" 2^>^&1 ^| Select-String k ^| Foreach-Object {$data = ^(^([string] $_^).Trim^(^) -Split '\s+'^)[0,1,3,9]; If ^( ^(Invoke-Expression^($data[2].replace^('k','*1000'^).replace^('M','*1000000'^)^)^) -gt 26 ^){ exit }}
	1>NUL "!Chcp!" 65001
	IF NOT EXIST "!TmpFile!" (
		IF NOT "!LastRedirectURL!"=="" CALL :ColorEcho WARNING def 1 0 "File type for URL could not be determined, because there probably was some problem with curl."
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
	1>NUL "!Chcp!" 1252
	FOR /F "DELIMS=" %%A IN ('"!Powershell!" -Command ^(Get-Content '!ScrFile!' -Encoding byte -TotalCount 26^) -Join ' '') DO (SET "First26Bytes=%%A")
	1>NUL "!Chcp!" 65001

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
	FOR /F "TOKENS=2 DELIMS= " %%A IN ('2^>NUL curl --silent --location --head -X GET "!URL!" ^| "!Findstr!" "Location: "') DO (SET "LastRedirectURL=%%A")
	IF NOT DEFINED LastRedirectURL SET "LastRedirectURL=!URL!"

ENDLOCAL & SET "%ReturnVar1%=%LastRedirectURL%"
EXIT /B 0

REM ------------------------ UpdatePath ------------------------
:UpdatePath
SETLOCAL
	CALL :GetPaths "PATH" "OldPATH" "OldSystemPATH"

	REM Update this local PATH.
	IF "!PATH:%RedirectsDir%=!"=="!PATH!" (
		IF NOT "!PATH:~-1!"==";" SET "PATH=!PATH!;"
		SET "PATH=!PATH!!RedirectsDir!;!PckDir!"
	)

	IF "!OldPATH:%RedirectsDir%=!;!OldSystemPATH:%RedirectsDir%=!"=="!OldPATH!;!OldSystemPATH!" (
		CALL :ColorEcho INFO def 1 1 "Redirects and PCK directory are not in PATH"
		CALL :ColorEcho ACTION def 0 1 "Adding Redirects and PCK directory permanently to PATH with SETX..."

		1>NUL 2>&1 SETX PATH "!OldPATH!!RedirectsDir!;!PckDir!"

		CALL :GetPaths "PATH" "OldPATH" "OldSystemPATH"
		IF "!OldPATH:%RedirectsDir%=!;!OldSystemPATH:%RedirectsDir%=!"=="!OldPATH!;!OldSystemPATH!" (
			CALL :ColorEcho "" red 1 1 " Failed^!"
			CALL :ColorEcho WARNING def 1 0 "For some unknown reason the Redirects and PCK directories could not be added to PATH."
			CALL :Anomaly
		) ELSE (
			CALL :ColorEcho "" green 1 1 " Success^!"
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
	IF "!PATHEXT:.LNK=!"=="!PATHEXT!" (
		SET "PATHEXT=!PATHEXT!.LNK"
	)

	IF "!OldPATHEXT:.LNK=!;!OldSystemPATHEXT:.LNK=!"=="!OldPATHEXT!;!OldSystemPATHEXT!" (
		CALL :ColorEcho INFO def 1 1 "Extension ".LNK" is not in "PATHEXT"."
		CALL :ColorEcho ACTION def 0 1 "Adding extension ".LNK" permanently to "PATHEXT" with SETX..."

		:: %$Split% PATHEXT ;
		:: SET PATHEXT

		IF NOT "!OldPATHEXT!"==";" (
			1>NUL 2>&1 SETX PATHEXT "!OldPATHEXT!.LNK"
		) ELSE (
			IF "!PATHEXT:.LNK=!"=="!PATHEXT!" (
				1>NUL 2>&1 SETX PATHEXT "!PATHEXT!.LNK"
			) ELSE (
				1>NUL 2>&1 SETX PATHEXT "!PATHEXT!"
			)
		)

		CALL :GetPaths "PATHEXT" "OldPATHEXT" "OldSystemPATHEXT"
		IF "!OldPATHEXT:.LNK=!;!OldSystemPATHEXT:.LNK=!"=="!OldPATHEXT!;!OldSystemPATHEXT!" (
			CALL :ColorEcho "" red 1 1 " Failed^!"
			CALL :ColorEcho WARNING def 1 0 "For some unknown reason, the ".LNK" extension could not be added to the "PATHEXT" environment variable."
			CALL :Anomaly
		)
		CALL :ColorEcho "" green 1 1 " Success^!"
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
	SET "CR=" & IF NOT DEFINED CR FOR /F "SKIP=1" %%C IN ('ECHO(^|"!Replace!" ? . /W /U') DO (SET "CR=%%C")

	SET "ddx=!"
	SETLOCAL EnableDelayedExpansion
	SET "Die="
	IF NOT DEFINED %1 (
	    CALL :ColorEcho WARNING def 1 0 "Trimming failed, because variable "%1" is not defined."
	    SET "Die=1"
	) ELSE (
		SET "Str=!%1!"
	)

	IF NOT DEFINED Die FOR %%L IN ("!LF!") DO (
		IF "!Str!" NEQ "!Str:%%~L=!" (
		    CALL :ColorEcho WARNING def 1 0 "Trimming failed, because variable "%1" contains linefeeds."
		    SET "Die=1"
		)
	)

	IF NOT DEFINED Die FOR %%C IN ("!CR!") DO (
		IF "!Str!" NEQ "!Str:%%~C=!" (
		    CALL :ColorEcho WARNING def 1 0 "Trimming failed, because variable "%1" contains carriage returns."
		    SET "Die=1"
		)
	)

	IF DEFINED Die GOTO :Die

	(FOR /F EOL^= %%A IN ("!Str!") DO REM NOP
	) || (
	    CALL :ColorEcho WARNING def 1 0 "Trimming failed, because variable "%1" consists entirely of whitespace."
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
		'2^>NUL "!Where!" "!DefaultLocation!":"!FileName!"'
	) DO (
		SET "ProgramPath=%%~A"
		GOTO :Found
	)
	FOR /F "DELIMS=" %%A IN (
		'2^>NUL "!Where!" "!FileName!"'
	) DO (
		SET "ProgramPath=%%~A"
		GOTO :Found
	)

	CALL :ColorEcho WARNING def 1 0 "!FileName! was not found."
	ENDLOCAL & SET "%ReturnVar1%=%FileName%"
	EXIT /B 1

	:Found
ENDLOCAL & SET "%ReturnVar1%=%ProgramPath%"
EXIT /B 0

REM ------------------------ ProgramWorks ------------------------
:ProgramWorks
SETLOCAL
	SET "FileName=%~1"
	REM "!ReturnVar1!" MUST NOT be "ERRORLEVEL"
	SET "ReturnVar1=%~2"

	"!System32!\where.exe" /Q "!FileName!"

	IF "!ERRORLEVEL!"=="9009" (
		CALL :ColorEcho ERROR def 1 0 "where-utility does not exist. Is your path variable broken?"
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
	SET "NumberOfArgs=0"
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
	CALL :ColorEcho WARNING def 1 0 "You specified 50 or more args to the command !Command!."

	:NoArgsAnymore
	SET "ExtraArgs=!ExtraArgs:~2!"
	%$Trim% CommandWithArgs SingleQuote
	%$ToLower% ExtraArgs
	%$ToLower% CommandWithArgs
	IF !NumberOfArgs! GTR !MaxNumberOfArgs! (
		SET /A "Difference=NumberOfArgs - MaxNumberOfArgs"
		IF "!Difference!"=="1" (
			CALL :ColorEcho WARNING def 1 0 "The last argument (`!ExtraArgs!`) in the command `!CommandWithArgs!` was ignored."
		) ELSE (
			CALL :ColorEcho WARNING def 1 0 "The last !Difference! arguments (`!ExtraArgs!`) in the command `!CommandWithArgs!` were ignored."
		)
		ENDLOCAL
		EXIT /B 1
	)
ENDLOCAL
EXIT /B 0

REM ------------------------ ShowHelp ------------------------
:ShowHelp
SETLOCAL
	CALL :ColorEcho INFO def 1 0 "Usage:"
	CALL :ColorEcho "" yellow 1 0 "    pck package                      # install a package"
	CALL :ColorEcho "" yellow 1 0 "    pck i {package | all}            # show info for {a package | all packages}"
	CALL :ColorEcho "" yellow 1 0 "    pck i {package | all} [offline]  # as above, but don't fetch further info from URLs"
	CALL :ColorEcho "" yellow 1 0 "    pck r package                    # uninstall (removes) a package"
	CALL :ColorEcho "" yellow 1 0 "    pck u package link               # update a package's link"
	CALL :ColorEcho "" yellow 1 0 "    pck a package link               # add a package to list"
	CALL :ColorEcho "" yellow 1 0 "    pck list [regex]                 # list all packages [that match the regex]"
	REM                                        /!\   /!\ For some cool reason, you don't need/can't escape these qoutes,
	REM                                         !     !  maybe because there are no spaces. If you escape them, it won't work as expected.
	CALL :ColorEcho "" yellow 1 0 "    pck [help]                       # show this help"
	ENDLOCAL
	EXIT /B 0


REM ------------------------ ParsePackageConfigFile ------------------------
:ParsePackageConfigFile
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
									CALL CALL :ColorEcho WARNING def 1 0 "Error parsing package "%%%%PackageList[%%j%%][name]%%%%", default identifier used although relative path points to a folder."
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
					CALL CALL :ColorEcho WARNING def 1 0 "Type !Type! for package "%%%%PackageList[%%j%%][name]%%%%" does not exist."
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
		REM TODO
		REM 2>NUL RD /S /Q "!TmpDir!"
	)
	1>NUL "!Chcp!" !OldCodePage!
	POPD
ENDLOCAL
EXIT /B 0

REM ------------------------ Anomaly ------------------------
:Anomaly
SETLOCAL
	CALL :ColorEcho INFO def 0 0 "Please report this error on "
	<NUL SET/P=!ESC![33;4;1mhttps://www.github.com/CubersHub/PCK_WindowsPackageManager/issues!ESC![0m
	CALL :ColorEcho "" yellow 1 0 ". This error should never happen."
	CALL :ColorEcho "" yellow 1 0 "       Please provide some information about your permissions, the directory this file is in,"
	CALL :ColorEcho "" yellow 1 0 "       whether you changed the source code or not and other potentially useful information."
ENDLOCAL
EXIT /B 0

REM ------------------------ Error ------------------------
REM Source: https://stackoverflow.com/a/61782349
:Error - Cleanly exit batch processing, regardless how many CALLs
	CALL :ColorEcho "" white 1 0 ""
	CALL :Cleanup "%0"
	CALL :ColorEcho ACTION def 1 0 "Exiting."
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


REM Ideas/TODOs:
REM Get latest Github Releases: (FOR /F "!SKIP! TOKENS=1* DELIMS=: " %A IN ('curl --silent --location "https://api.github.com/repos/telegramdesktop/tdesktop/releases/latest" ^| "!Findstr!" /R "browser_download_url.*zip"') DO @(IF NOT DEFINED URL SET "URL=%~B")) & ECHO !URL! & SET URL=
REM Save last direct download link in installation folder to check for new (different) version
REM Sourceforge API /best_release.json
REM Find programs instead of programworks