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

{$DEFINE DWH_CRYPT1}

{$I+,A-}
UNIT DWHUTIL;

INTERFACE

USES System2, dwh;

CONST
CDOC = '!doc';
CLINK  = '!link';

PROCEDURE dwh_write(VAR f : DWH_FILE; VAR buf; size : WORD);
PROCEDURE dwh_read(VAR f : DWH_FILE; VAR buf; size : WORD);

FUNCTION dwh_rle_pack(VAR s : STRING) : STRING;
FUNCTION dwh_rle_unpack(VAR s : STRING) : STRING;

FUNCTION dwh_lookup(VAR fname : STRING) : BOOLEAN;

FUNCTION dwh_GetHdrOfs(VAR f : DWH_FILE; VAR len : LONGINT) : LONGINT;
FUNCTION dwh_GetArtCount(VAR f : DWH_FILE; ofs : LONGINT) : WORD;
FUNCTION dwh_GetArtOfs(VAR f : DWH_FILE; ofs : LONGINT; id : WORD) : LONGINT;

IMPLEMENTATION

USES str, dos;

FUNCTION dwh_rle_pack(VAR s : STRING) : STRING;
VAR
        r        : STRING;
        i, len, c : INTEGER;
BEGIN
        r[0] := #0;
        i := 1;
        len := ORD(s[0]);
        WHILE i <= len DO BEGIN
                c := 1;
                IF i < len THEN
                        IF s[i] = s[i + 1] THEN BEGIN
                                WHILE (s[i] = s[i + c]) AND (i + c <= len) AND (c < 15) DO INC(c);
                                IF c > 2 THEN BEGIN
                                        INC(r[0]);
                                        r[ORD(r[0])] := CHR(c);
                                END ELSE c := 1;
                        END;
                INC(r[0]);
                r[ORD(r[0])] := s[i];
                INC(i, c);
        END;
        dwh_rle_pack := r;
END;

FUNCTION dwh_rle_unpack(VAR s : STRING) : STRING;
VAR
        r      : STRING;
        i, len : INTEGER;
        c      : CHAR;
BEGIN
        r[0] := #0;
        i := 1;
        len := ORD(s[0]);
        WHILE i <= len DO BEGIN
                c := s[i];
                IF ORD(c) IN [3..15] THEN BEGIN
                        FillChar(r[ORD(r[0]) + 1], ORD(c), s[i + 1]);
                        INC(r[0], ORD(c));
                        INC(i, 2);
                END ELSE BEGIN
                        INC(r[0]);
                        r[ORD(r[0])] := c;
                        INC(i);
                END;
        END;
        dwh_rle_unpack := r;
END;

{$IFDEF DWH_CRYPT}
PROCEDURE dwh_crypt(VAR b; size : WORD);
VAR
    p : PCHAR;
    w : WORD;
BEGIN
    p := @b;
    w := 26;
    WHILE size > 0 DO BEGIN
        p^ := CHR(ORD(p^) XOR (w AND $FF));
        INC(p);
        DEC(size);
        w := w + 13;
    END;
END;
{$ENDIF}

PROCEDURE dwh_write(VAR f : DWH_FILE; VAR buf; size : WORD);
BEGIN
{$IFDEF DWH_CRYPT}
        dwh_crypt(buf, size);
{$ENDIF}
        BlockWrite(f, buf, size);
{$IFDEF DWH_CRYPT}
        dwh_crypt(buf, size);
{$ENDIF}
END;

PROCEDURE dwh_read(VAR f : DWH_FILE; VAR buf; size : WORD);
BEGIN
        BlockRead(f, buf, size);
{$IFDEF DWH_CRYPT}
        dwh_crypt(buf, size);
{$ENDIF}
END;

FUNCTION dwh_GetArtCount(VAR f : DWH_FILE; ofs : LONGINT) : WORD;
VAR     count : WORD;
BEGIN
        Seek(f, ofs + SizeOf(TDWH_MAGIC));
        dwh_read(f, count, SizeOf(WORD));
        dwh_GetArtCount := count;
END;

FUNCTION dwh_GetArtOfs(VAR f : DWH_FILE; ofs : LONGINT; id : WORD) : LONGINT;
VAR     r     : LONGINT;
BEGIN
        Seek(f, ofs + SizeOf(TDWH_MAGIC) + SizeOf(WORD) + (id SHL 2));
        dwh_read(f, r, SizeOf(LONGINT));
        dwh_GetArtOfs := ofs + r;
END;

FUNCTION dwh_GetHdrOfs(VAR f : DWH_FILE; VAR len : LONGINT) : LONGINT;
VAR
        n, r : LONGINT;
        m : TDWH_MAGIC;
BEGIN
        r := DWH_HEADER_NOT_FOUND;
        len := 0;
        Seek(f, FileSize(f) - SizeOf(LONGINT));
        BlockRead(f, n, SizeOf(LONGINT));
        WHILE TRUE DO BEGIN
                IF n < 0 THEN BREAK;
                IF n > FilePos(f) THEN BREAK;
                Seek(f, FilePos(f) - n);
                BlockRead(f, m, SizeOf(TDWH_MAGIC));
                Seek(f, FilePos(f) - SizeOf(TDWH_MAGIC));
                IF m = DWH_MAGIC THEN BEGIN
                        r := FilePos(f);
                        len := n;
                        BREAK;
                END;
                Seek(f, FilePos(f) - SizeOf(LONGINT));
                BlockRead(f, n, SizeOf(LONGINT));
        END;
        dwh_GetHdrOfs := r;
END;

FUNCTION check_file(fname : STRING; VAR outfname : STRING) : BOOLEAN;
VAR
        f : DWH_FILE;
        r : BOOLEAN;
        l : LONGINT;
BEGIN
        r := FALSE;
        IF FileExists(fname) THEN BEGIN
                Assign(f, fname);
                Reset(f);
                IF IsOpen(f) THEN r := dwh_GetHdrOfs(f, l) <> DWH_HEADER_NOT_FOUND;
                Close(f);
        END;
        IF r THEN outfname := fname;
        check_file := r;
END;

PROCEDURE normPath(VAR name : STRING);
BEGIN
        IF (name[0] <> #0) AND (name[ORD(name[0])] <> '\') THEN name := name + '\';
END;

FUNCTION dwh_lookup(VAR fname : STRING) : BOOLEAN;
VAR     r     : BOOLEAN;
        tname : STRING;
        mpath : STRING;
        epath : STRING;
        i, e  : INTEGER;
        lang  : STRING;
        help  : STRING;
        bname : STRING;
        bname_exe : STRING;
        bname_hlp : STRING;
        lname_hlp : STRING;
BEGIN
        lang := change_ext(basename(GetEnv('LANG')), '');
        help := GetEnv('HELPPATH');
        epath := GetEnv('PATH');

        tname := fname;
        mpath := getpathname(ParamStr(0));
        bname :=basename(fname);
        bname_hlp := change_ext(bname, '.HLP');
        bname_exe := change_ext(bname, '.EXE');
        lname_hlp := '';
        IF lang[0] <> #0 THEN lname_hlp := change_ext(bname, '') + '_'+lang + '.HLP';
        r := FALSE;

        { direct name }
        IF NOT r THEN r := check_file(fname, fname);

        { check HELPPATH}
        IF help[0] <> #0 THEN BEGIN
                normPath(help);
                IF (NOT r) AND (ORD(lname_hlp[0])<>0) THEN r := check_file(help + lname_hlp, fname);
                IF NOT r THEN r := check_file(help + bname_hlp, fname);
        END;

        { check cur dir}
        IF (NOT r) AND (lname_hlp[0] <> #0) THEN r := check_file(help + lname_hlp, fname);
        IF NOT r THEN r := check_file(bname_hlp, fname);
        IF NOT r THEN r := check_file(bname_exe, fname);

        { check .exe dir}
        IF (NOT r) AND (lname_hlp[0] <> #0) THEN r := check_file(mpath + lname_hlp, fname);
        IF NOT r THEN r := check_file(mpath + bname_hlp, fname);
        IF NOT r THEN r := check_file(mpath + bname_exe, fname);

        { check PATH}
        IF NOT r THEN BEGIN
                i := 0;
                WHILE (NOT r) AND (Length(epath) <> 0) DO BEGIN
                        e := Pos(';', epath);
                        IF e >= 0 THEN BEGIN
                                mpath := Copy(epath, 1, e - 1);
                                epath := Copy(epath, e + 1, Length(epath));
                        END ELSE BEGIN
                                mpath := epath;
                                epath := '';
                        END;
                        IF Length(mpath) = 0 THEN BREAK;
                        normPath(mpath);
                        IF (NOT r) AND (lname_hlp[0] <> #0) THEN r := check_file(mpath + lname_hlp, fname);
                        IF NOT r THEN r := check_file(mpath + bname_hlp, fname);
                        IF NOT r THEN r := check_file(mpath + bname_exe, fname);
                END;
        END;
        dwh_lookup := r;
END;

END.
