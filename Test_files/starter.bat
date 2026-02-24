this is still and example and needs to be fixed with proper libraries and paths!!!


@echo off
setlocal
set R_HOME=%~dp0R-Portable
set PATH=%R_HOME%\bin\x64;%PATH%

REM Install missing packages into a local library on first run
"%R_HOME%\bin\Rscript.exe" -e "local_lib <- file.path(getwd(), 'library'); if (!dir.exists(local_lib)) dir.create(local_lib); .libPaths(c(local_lib, .libPaths())); pkgs <- c('shiny'); needs <- pkgs[!pkgs %in% rownames(installed.packages())]; if (length(needs)) install.packages(needs, repos='https://cloud.r-project.org'); shiny::runApp('app', launch.browser=TRUE, host='127.0.0.1', port=8888)"
endlocal
