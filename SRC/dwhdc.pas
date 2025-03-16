{ MIT License

Copyright (c) 2025 Viacheslav Komenda

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. }
{$M 16000, 64000, 650000}
{$I+}
PROGRAM DWHDC;

USES System2, str, DWH, DWHUTIL;

TYPE
PWORD = ^WORD;
VAR err : BOOLEAN;

PROCEDURE DeCompile(src_name, idname : STRING);
VAR
        INF     : DWH_FILE;
        ofs     : LONGINT;
        s       : STRING;
        acount  : WORD;
        abody   : PCHAR;
        aptr    : WORD;
        i, link : WORD;
        asize, alines, l : WORD;
        atype  : BYTE;
        c      : CHAR;
BEGIN
        Assign(INF, src_name);
        {$I-}
        Reset(INF);
        i := IOResult;
        {$I+}
        err := i <> 0;
        IF err THEN BEGIN
                System.Writeln('ERROR: Could not read ', src_name);
                EXIT;
        END;
        upstr(idname);
        GetMem(abody, 65535);
        ofs := dwh_GetHdrOfs(INF, ofs);
        IF ofs <> DWH_HEADER_NOT_FOUND THEN BEGIN
                acount := dwh_GetArtCount(INF, ofs);
                i := 0;
                WHILE i < acount DO BEGIN
                        Seek(INF, dwh_GetArtOfs(INF, ofs, i));
                        dwh_read(INF, atype, 1);
                        dwh_read(INF, asize, SizeOf(WORD));
                        dwh_read(INF, alines, SizeOf(WORD));
                        dwh_read(INF, abody^, asize);
                        l := 0; aptr := 0;
                        System.WriteLn(CDOC, ' ', idname, '_', i);
                        WHILE l < alines DO BEGIN
                                c := abody[aptr];
                                INC(aptr);
                                CASE c OF
                                CHR(DWH_LT_LINE): System.WriteLn('---');
                                CHR(DWH_LT_TEXT): BEGIN
                                        Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                                        INC(aptr, ORD(abody[aptr]) + 1);
                                        System.WriteLn(dwh_rle_unpack(s));
                                END;
                                CHR(DWH_LT_LINK): BEGIN
                                        link := PWORD(abody[aptr])^;
                                        System.Write(CLINK, ' ', idname, '_', link, ' ');
                                        INC(aptr, SizeOf(WORD));
                                        Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                                        INC(aptr, ORD(abody[aptr]) + 1);
                                        System.WriteLn(dwh_rle_unpack(s));
                                END;
                                END;
                                INC(l);
                        END;
                        INC(i);
                END;
        END;
        FreeMem(abody, 65535);
        Close(INF);
END;

VAR iname : STRING;

BEGIN
        IF ParamCount <> 1 THEN BEGIN
                System.WriteLn('Usage:');
                System.WriteLn(#9, basename(ParamStr(0)), ' filename');
                System.WriteLn;
                System.WriteLn('For more help see DWHELP DWHELP 5');
                Halt(1);
        END;
        iname := ParamStr(1);
        DeCompile(iname, change_ext(BaseName(iname), ''));
        IF err THEN Halt(1);
        Halt(0);
END.
