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
{$M 64000, 256000, 256000}
{$I+}
PROGRAM DWHADD;

USES SYSTEM2, STR, DWH, DWHUTIL;

VAR     fname    : STRING;
        hname    : STRING;
        f, h, f2 : BFILE;
        i        : INTEGER;
        ofs, len : LONGINT;

BEGIN
        fname := ParamStr(0);
        IF ParamCount <> 2 THEN BEGIN
                fname := BaseName(fname);
                System.WriteLn('Usage:');
                System.WriteLn(#9, fname, ' exefile hlpfile');
                System.WriteLn;
                System.WriteLn('For more help see DWHELP DWHELP 2');
                Halt(1);
        END;
        fname := ParamStr(1);
        hname := ParamStr(2);
        IF NOT FileExists(fname) THEN BEGIN
                System.WriteLn(fname, ' does not exists.');
                Halt(1);
        END;
        IF NOT FileExists(hname) THEN BEGIN
                System.WriteLn(hname, ' does not exists.');
                Halt(1);
        END;
        Assign(f, fname);
        {$I-}
        Reset(f);
        i := IOResult;
        {$I+}
        IF i <> 0 THEN BEGIN
                System.WriteLn('Could not read ', fname);
                Halt(1);
        END;
        Assign(h, hname);
        {$I-}
        Reset(h);
        i := IOResult;
        {$I+}
        IF i <> 0 THEN BEGIN
                System.WriteLn('Could not read ', hname);
                Close(f);
                Halt(1);
        END;
        ofs := dwh_GetHdrOfs(f, len);
        IF ofs <> DWH_HEADER_NOT_FOUND THEN BEGIN
                System.Writeln('Remove old HLP');
                Seek(f, ofs);
                IF ofs+len < FileSize(f) THEN BEGIN
                        ReWriteTemp(f2);
                        Seek(f, ofs + len);
                        BlockCopy(f, f2, FileSize(f) - FilePos(f));
                        Seek(f2, 0);
                        Seek(f, ofs);
                        BlockCopy(f2, f, FileSize(f2));
                        Close(f2);
                END;
                Truncate(f);
        END;
        Seek(f, FileSize(f));
        ofs := dwh_GetHdrOfs(h, len);
        IF ofs <> DWH_HEADER_NOT_FOUND THEN BEGIN
                System.Writeln('Add new HLP');
                Seek(h, ofs);
                BlockCopy(h, f, len);
        END;
        Close(f);
        Close(h);
END.
