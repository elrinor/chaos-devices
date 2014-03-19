@echo off
java -cp oberon.jar;antlr.jar ru.msu.cmc.sp.oberon.Oberon %*
echo.
echo errorlevel = %errorlevel%