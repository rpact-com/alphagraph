@echo off
REM ===========================================
REM  Direct gMCP Startup (isolated R session)
REM  Avoids StatET/rj JVM conflicts
REM ===========================================

REM ---- Set R installation path ----
set R_HOME=C:\R\R-4.5.2
set R_EXE="%R_HOME%\bin\Rscript.exe"

REM ---- Check if Rscript exists ----
if not exist %R_EXE% (
    echo ERROR: Rscript.exe not found at %R_EXE%
    pause
    exit /b 1
)

echo Starting gMCP GUI...
cmd /k %R_EXE% --vanilla -e "library(gMCP); graphGUI()"