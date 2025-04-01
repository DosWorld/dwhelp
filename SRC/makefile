all: dwhc.exe dwhdc.exe dwhelp.exe dwhadd.exe dwhsub.exe dos_api.hlp amb.hlp

install: all
        copy /y /b *.exe ..\bin\
        copy /y /b *.hlp ..\bin\

dwhc.exe: dwh.tpu dwhc.pas dwhutil.tpu
        tpc dwhc.pas
        upx -9 --8086 dwhc.exe

dwhdc.exe: dwhdc.pas dwh.tpu dwhutil.tpu
        tpc dwhdc.pas
        upx -9 --8086 dwhdc.exe

dwhelp.exe: dwhelp.pas dwhelp.hlp scrui.tpu dwh.tpu dwhview.tpu dwhadd.exe dwhutil.tpu
        tpc dwhelp.pas
        upx -9 --8086 dwhelp.exe
        dwhadd dwhelp.exe dwhelp.hlp
        del dwhelp.hlp

dwhadd.exe: dwhadd.pas dwh.tpu dwhutil.tpu
        tpc dwhadd.pas
        upx -9 --8086 dwhadd.exe

dwhsub.exe: dwhsub.pas dwh.tpu dwhutil.tpu
        tpc dwhsub.pas
        upx -9 --8086 dwhsub.exe

dwh.tpu: dwh.pas
        tpc dwh.pas

dwhutil.tpu: dwhutil.pas
        tpc dwhutil.pas

scr.tpu: scr.pas
        tpc scr.pas

event.tpu: event.pas
        tpc event.pas

scrui.tpu: scrui.pas scr.tpu event.tpu
        tpc scrui.pas

dwhview.tpu: scr.tpu dwhutil.tpu dwhview.pas
        tpc dwhview.pas

dos_api.hlp: dwhc.exe dos_api.txt
        dwhc dos_api.txt

amb.hlp: dwhc.exe amb.txt
        dwhc amb.txt

dwhelp.hlp: dwhc.exe dwhelp.txt
        dwhc dwhelp.txt

test: dwhelp.exe dos_api.hlp
	dwhelp dos_api

clean:
        del *.bak
        del *.tpu
        del *.exe
        del *.hlp
