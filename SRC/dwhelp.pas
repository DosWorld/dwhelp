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
{$M 64000, 256000, 650000}
{$I+}
PROGRAM DWHELP;

USES SYSTEM2, STR, DWHVIEW, DWHUTIL;

VAR     id    : WORD;
        pc    : INTEGER;
        ctx   : TDWH_VIEW_CTX;
        fname : STRING;
BEGIN
        pc := ParamCount;
        IF (pc <> 1) AND (pc <> 2) THEN BEGIN
                System.WriteLn('Usage:');
                System.WriteLn(#9, BaseName(ParamStr(0)), ' help-file [articleId]');
                System.WriteLn;
                System.WriteLn('For more help see DWHELP DWHELP');
                Halt(1);
        END;
        IF pc = 2 THEN id := atow(ParamStr(2), 0) ELSE id := 0;
        fname := ParamStr(1);
        IF dwh_lookup(fname) THEN BEGIN
                DWHVIEW.Init(ctx, id, fname);
                Halt(dwh_view(ctx));
        END ELSE Halt(1);
END.
