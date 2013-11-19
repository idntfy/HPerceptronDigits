set BIN_PATH=../bin
set SRC_PATH=../src

ghc -o "%BIN_PATH%/HPerceptronDigits" -odir "%BIN_PATH%" -hidir "%BIN_PATH%" -i"%SRC_PATH%" "%SRC_PATH%/main.hs"
pause