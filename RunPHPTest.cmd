SET PHP_INI_DIR="D:\wamp\bin\php\php7.0.10"
SET PHP_EXE="D:\wamp\bin\php\php7.0.10\php.exe"

xcopy "%~dp0sess_2m7ee1t6374qpol7bk3e1geb11.*" "D:\wamp\tmp\sess_2m7ee1t6374qpol7bk3e1geb11.*" /Y

%PHP_EXE% %~dp0TestScript.php 