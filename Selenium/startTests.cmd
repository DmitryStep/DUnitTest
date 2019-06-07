@echo off
SET MVN_PATH=c:\maven\bin\

call c:\maven\bin\mvn -X clean test > Build.txt
call c:\maven\bin\mvn -X verify -DskipTests > html.txt