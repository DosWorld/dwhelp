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
PROGRAM DWHC;
USES System2, str, DWH, DWHUTIL;

TYPE
PDWH_ART = ^TDWH_ART;
TDWH_ART = RECORD
        id    : WORD;
        lines : WORD;
        size  : WORD;
        ofs   : LONGINT;
        has_ref : BOOLEAN;
        link  : STRING;
        next  : PDWH_ART;
        prev  : PDWH_ART;
END;

PDWH_LINE = ^TDWH_LINE;
TDWH_LINE = RECORD
        t       : BYTE;
        linkid  : WORD;
        content : STRING;
        next    : PDWH_LINE;
END;

VAR err : BOOLEAN;

FUNCTION GetFirstWord(VAR str : STRING) : STRING;
VAR
        r : STRING;
        i : INTEGER;
BEGIN
        r := '';
        i := 1;
        WHILE i <= ORD(str[0]) DO BEGIN
                IF str[i] <= ' ' THEN BREAK;
                INC(i);
        END;
        r := Copy(str, 1, i - 1);
        str := Copy(str, i + 1, ORD(str[0]) - i);
        r := trim(r);
        str := trim(str);
        GetFirstWord := r;
END;

PROCEDURE next_line(VAR t : BFILE; VAR str : STRING; VAR stop : BOOLEAN);
VAR
        i : INTEGER;
BEGIN
        str := '';
        IF EOF(t) THEN stop := TRUE ELSE ReadLn(t, str);
        FOR i := 1 TO ORD(str[0]) DO IF str[i] < ' ' THEN str[i] := ' ';
END;

FUNCTION lookup(root : PDWH_ART; VAR s : STRING) : PDWH_ART;
BEGIN
        WHILE root <> NIL DO BEGIN
                IF root^.link = s THEN BREAK;
                root := root^.next;
        END;
        lookup := root;
END;

PROCEDURE AddLine(abody : PCHAR; VAR aptr : WORD);
BEGIN
        abody[aptr] := CHR(DWH_LT_LINE);
        INC(aptr);
END;

PROCEDURE AddText(abody : PCHAR; VAR aptr : WORD; s : STRING);
BEGIN
        abody[aptr] := CHR(DWH_LT_TEXT);
        INC(aptr);
        s := dwh_rle_pack(s);
        Move(s[0], abody[aptr], ORD(s[0]) + 1);
        INC(aptr, ORD(s[0]) + 1);
END;

PROCEDURE AddLink(abody : PCHAR; VAR aptr : WORD; id : WORD; s : STRING);
BEGIN
        abody[aptr] := CHR(DWH_LT_LINK);
        INC(aptr);
        abody[aptr] := CHR(id AND $FF);
        INC(aptr);
        abody[aptr] := CHR((id SHR 8) AND $FF);
        INC(aptr);
        s := dwh_rle_pack(s);
        Move(s[0], abody[aptr], ORD(s[0]) + 1);
        INC(aptr, ORD(s[0]) + 1);
END;

PROCEDURE CompileArt(VAR INF : BFILE; root, cur : PDWH_ART;
abody : PCHAR; VAR aptr : WORD; VAR stop : BOOLEAN; VAR s : STRING);
VAR
        l      : PDWH_ART;
        i      : INTEGER;
        link   : STRING;
        rlines : WORD;
        elines : WORD;
BEGIN
        IF (root = NIL) OR (cur = NIL) THEN EXIT;
        i := 0;
        rlines := 0;
        elines := 0;
        WHILE (NOT stop) AND (i < cur^.lines) DO BEGIN
                IF starts_with(s, CLINK) THEN BEGIN
                        GetFirstWord(s);
                        link := GetFirstWord(s);
                        upstr(link);
                        l := lookup(root, link);
                        IF l = NIL THEN BEGIN
                                System.WriteLn('ERROR: Link ', cur^.link, ' -> ', link, ' does not exists.');
                                stop := TRUE;
                                err := TRUE;
                                EXIT;
                        END ELSE System.WriteLn('->':9, l^.id, ':', link);
                        l^.has_ref := TRUE;
                        AddLink(abody, aptr, l^.id, s);
                        INC(rlines);
                        elines := 0;
                END ELSE BEGIN
                        IF ORD(s[0]) = 0 THEN INC(elines) ELSE elines := 0;
                        IF elines < 2 THEN BEGIN
                                IF s = '---' THEN AddLine(abody, aptr)
                                ELSE AddText(abody, aptr, s);
                                INC(rlines);
                        END;
                END;
                next_line(INF, s, stop);
                INC(i);
        END;
        cur^.lines := rlines;
END;

PROCEDURE Compile(src_name, dst_name : STRING);
VAR
        INF    : DWH_FILE;
        OUTF   : DWH_FILE;
        root, cur, prev : PDWH_ART;
        s      : STRING;
        stop   : BOOLEAN;
        acount : WORD;
        abody  : PCHAR;
        aptr   : WORD;
        b      : BYTE;
        link   : STRING;
        i      : INTEGER;
        len    : LONGINT;
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
        root := NIL;
        cur := NIL;
        prev := NIL;
        acount := 0;
        stop := FALSE;
        next_line(INF, s, stop);
        System.WriteLn('Compile ', BaseName(src_name));
        WHILE NOT stop DO BEGIN
                IF starts_with(s, CDOC) THEN BEGIN
                        INC(acount);
                        GetMem(cur, SizeOf(TDWH_ART));
                        FillChar(cur^, SizeOf(TDWH_ART), #0);
                        IF prev <> NIL THEN prev^.next := cur;
                        IF prev <> NIL THEN cur^.id := prev^.id + 1;
                        IF root = NIL THEN root := cur;
                        GetFirstWord(s);
                        upstr(s);
                        IF lookup(root, s) <> NIL THEN BEGIN
                                System.Writeln('ERROR: Article with duplicate id ', s);
                                err := TRUE;
                                EXIT;
                        END;
                        cur^.link := s;
                        cur^.prev := prev;
                        prev := cur;
                END ELSE IF cur <> NIL THEN INC(cur^.lines);
                next_line(INF, s, stop);
        END;
        Assign(OUTF, dst_name);
        {$I-}
        ReWrite(OUTF);
        i := IOResult;
        {$I+}
        err := err OR (i <> 0);
        IF err THEN BEGIN
                err := TRUE;
                Close(INF);
                System.Writeln('ERROR: Could not write ', dst_name);
                EXIT;
        END;
        BlockWrite(OUTF, DWH_MAGIC, SizeOf(DWH_MAGIC));
        dwh_write(OUTF, acount, SizeOf(WORD));
        cur := root;
        WHILE cur <> NIL DO BEGIN
                dwh_write(OUTF, cur^.ofs, SizeOf(LONGINT));        
                cur := cur^.next;
        END;
        stop := err;
        GetMem(abody, 65535);
        Seek(INF, 0);
        next_line(INF, s, stop);
        WHILE NOT stop DO BEGIN
                IF starts_with(s, CDOC) THEN BEGIN
                        GetFirstWord(s);
                        upstr(s);
                        link := s;
                        aptr := 0;
                        next_line(INF, s, stop);
                        cur := lookup(root, link);
                        System.WriteLn(cur^.id:5, ':', link);
                        CompileArt(INF, root, cur, abody, aptr, stop, s);
                        WHILE (aptr AND 1) <> 0 DO BEGIN
                                abody[aptr] := #0;
                                INC(aptr);
                        END;
                        IF NOT err THEN BEGIN
                                b := DWH_ET_ART;
                                cur^.ofs := FilePos(OUTF);
                                cur^.size := aptr;
                                dwh_write(OUTF, b, SizeOf(BYTE));
                                dwh_write(OUTF, cur^.size, SizeOf(WORD));
                                dwh_write(OUTF, cur^.lines, SizeOf(WORD));
                                dwh_write(OUTF, abody^, cur^.size);
                        END;
                END ELSE BEGIN
                        next_line(INF, s, stop);
                END;
        END;
        Close(INF);
        FreeMem(abody, 65535);

        IF NOT err THEN BEGIN
                IF root <> NIL THEN root^.has_ref := TRUE;
                cur := root;
                WHILE cur <> NIL DO BEGIN
                        IF NOT cur^.has_ref THEN BEGIN
                                System.Writeln('ERROR: Have no links to ', cur^.link);
                                err := TRUE;
                        END;
                        cur := cur^.next;
                END;

                Seek(OUTF, SizeOf(DWH_MAGIC) + SizeOf(WORD));
                cur := root;
                WHILE cur <> NIL DO BEGIN
                        dwh_write(OUTF, cur^.ofs, SizeOf(LONGINT));
                        cur := cur^.next;
                END;
                len := FileSize(OUTF);
                Seek(OUTF, len);
                INC(len, SizeOf(LONGINT));
                BlockWrite(OUTF, len, SizeOf(LONGINT));
        END;
        IF IsOpen(OUTF) THEN SetDeleteOnClose(OUTF, err);
        Close(OUTF);
END;

VAR iname, oname : STRING;

BEGIN
        IF ParamCount <> 1 THEN BEGIN
                System.WriteLn('Usage:');
                System.WriteLn(#9, basename(ParamStr(0)), ' filename');
                System.WriteLn;
                System.WriteLn('For more help see DWHELP DWHELP 4');
                Halt(1);
        END;
        iname := ParamStr(1);
        oname := change_ext(iname, '.hlp');
        Compile(iname, oname);
        IF err THEN Halt(1);
        Halt(0);
END.
