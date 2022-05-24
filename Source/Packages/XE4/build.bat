setlocal
pushd %~dp0
call rsvars.bat

dcc32.exe ^
--no-config ^
-B ^
-LE..\..\..\ ^
-LN..\..\..\ ^
-NSSystem.Win;Vcl;Vcl.Samples;System;Soap;Winapi; ^
-U"%BDS%\lib\Win32\release" ^
GR32_R.dpk GR32_D.dpk

popd
