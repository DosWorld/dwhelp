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
{$I+,A-}
UNIT DWHVIEW;

INTERFACE

CONST
DWH_ERR_OK          = 0;
DWH_ERR_OPEN        = 1;
DWH_ERR_INVALID_FMT = 2;

CONST
HISTORY_SIZE = 63;
TYPE
TDWH_VIEW_CTX = RECORD
        fname   : STRING;
        id      : WORD;
        history : ARRAY[0..HISTORY_SIZE] OF WORD;
        hptr    : INTEGER;
END;

PROCEDURE init(VAR ctx : TDWH_VIEW_CTX; id : WORD; fname : STRING);
FUNCTION dwh_view(VAR ctx : TDWH_VIEW_CTX) : INTEGER;

IMPLEMENTATION

USES System2, DWH, DWHUTIL, scr, KMInput, str;

{$IFNDEF MS_COLOR_SCHEME}
{$IFNDEF BORLAND_COLOR_SCHEME}
{$DEFINE BORLAND_COLOR_SCHEME}
{$ENDIF}
{$ENDIF}


CONST
HELP_STR : STRING
= ' ~'#$11+#$D9+'~ Enter topic ³ ~'+#$18+#$19+#$1B+#$1A+'~ Move ³ ~BS~ Back ³ ~F5~ Index  ³';

{$IFDEF MS_COLOR_SCHEME}
CLR_TEXT     = $17;
CLR_TEXT2    = $1F;
CLR_LINK     = $1E;
CLR_LINE     = $1F;
CLR_HITEXT   = $1B;
CLR_HILINK   = $70;
CLR_TITLE    = $1F;
{$ENDIF}
{$IFDEF BORLAND_COLOR_SCHEME}
CLR_TEXT     = $30;
CLR_TEXT2    = $38;
CLR_LINK     = $3E;
CLR_LINE     = $30;
CLR_HITEXT   = $3F;
CLR_HILINK   = $70;
CLR_TITLE    = $3E;
{$ENDIF}

LINE_CHAR    = #$C4;
LINK_CHAR    = #$10;

PROCEDURE init(VAR ctx : TDWH_VIEW_CTX; id : WORD; fname : STRING);
BEGIN
        FillChar(ctx, SizeOf(TDWH_VIEW_CTX), #0);
        ctx.id := id;
        ctx.fname := fname;
END;

FUNCTION LocateLine(p : PCHAR; lnum : WORD) : WORD;
VAR
        k, i : WORD;
BEGIN
        i := 0;
        k := lnum;
        WHILE lnum <> 0 DO BEGIN
                IF p[i] = CHR(DWH_LT_LINK) THEN BEGIN
                        INC(i, 3);
                        INC(i, ORD(p[i]) + 1);
                END ELSE IF p[i] = CHR(DWH_LT_TEXT) THEN BEGIN
                        INC(i);
                        INC(i, ORD(p[i]) + 1);
                END ELSE IF p[i] = CHR(DWH_LT_LINE) THEN BEGIN
                        INC(i);
                END ELSE BREAK;
                DEC(lnum);
        END;
        LocateLine := i;
END;

PROCEDURE high_light(VAR hl_chars : STRING);
VAR
        len    : WORD;
        p      : PCHAR;
BEGIN
        len := scr.getheight * scr.getwidth;
        p := scr.screen;
        WHILE len <> 0 DO BEGIN
                IF p^ <> ' ' THEN IF p[1] = CHR(CLR_TEXT) THEN BEGIN
                        p[1] := hl_chars[ORD(p^) + 1];
                END;
                INC(p, 2);
                DEC(len);
        END;
END;

PROCEDURE PrintLine(y, width : INTEGER; abody : PCHAR; id : WORD; VAR aptr, nidx : WORD; istitle, isselrow : BOOLEAN);
VAR
        s   : STRING;
        c   : BYTE;
        idx : ^WORD;
BEGIN
        CASE abody[aptr] OF
        CHR(DWH_LT_TEXT): BEGIN
                INC(aptr);
                Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                INC(aptr, ORD(abody[aptr]) + 1);
                s := dwh_rle_unpack(s);
                c := CLR_TEXT;
                IF istitle THEN BEGIN
                        c := CLR_TITLE;
                        s := wtoa(id) + ': ' + s;
                        upstr(s);
                END;
                scr.printhl(1, y, c, CLR_HITEXT, s);
        END;
        CHR(DWH_LT_LINK): BEGIN
                INC(aptr);
                idx := @abody[aptr];
                nidx := idx^;
                INC(aptr, 2);
                Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                INC(aptr, ORD(abody[aptr]) + 1);
                s := dwh_rle_unpack(s);
                s := LINK_CHAR + ' ' + s;
                IF isselrow THEN c := CLR_HILINK ELSE c := CLR_LINK;
                scr.print(1, y, c, s);
        END;
        CHR(DWH_LT_LINE): BEGIN
                INC(aptr);
                scr.hprint(1, y, CLR_LINE, LINE_CHAR, width - 2);
        END;
        END;
END;

PROCEDURE VIEW(VAR ctx : TDWH_VIEW_CTX; VAR f : BFILE; fofs : LONGINT);
VAR
        ofsy    : WORD;
        maxx    : WORD;
        len     : WORD;
        ofsfile : LONGINT;
        i, acount, size, lines, rlines : WORD;
        t       : BYTE;
        abody   : PCHAR;
        aptr    : WORD;
        s       : STRING;
        idx     : ^WORD;
        nidx    : WORD;
        key     : WORD;
        needredraw : BOOLEAN;
        x, y    : INTEGER;
        prev_x, prev_y : INTEGER;
        prev_ofsy : WORD;
        width, height : INTEGER;
        c       : BYTE;
        stop    : BOOLEAN;
        hlchars : STRING;
BEGIN
        scr.cursor_big;
        kbd_reset;
        height := scr.getheight - 1;
        width := scr.getwidth;
        stop := FALSE;
        acount := dwh_GetArtCount(f, fofs);
        s := '';
        FOR i := $B3 TO $DA DO s := s + CHR(i);
        s := '`^#@*_.,?+*/\-()!:;"=<>(){}[]~|' + s + #$27;
        FillChar(hlchars[1], 255, CHR(CLR_TEXT));
        hlchars[0] := #$FF;
        FOR i := 1 TO ORD(s[0]) DO hlchars[ORD(s[i]) + 1] := CHR(CLR_TEXT2);

        GetMem(abody, 65535);
        WHILE (NOT stop) AND (ctx.id < acount) DO BEGIN
                Seek(f, dwh_GetArtOfs(f, fofs, ctx.id));
                dwh_read(f, t, SizeOf(BYTE));
                dwh_read(f, size, SizeOf(WORD));
                dwh_read(f, lines, SizeOf(WORD));
                dwh_read(f, abody^, size);
                rlines := lines;
                ofsy := 0; x := 1; y := 2;
                prev_x := -100;
                prev_y := -100;
                prev_ofsy := 65535;
                needredraw := TRUE;
                WHILE TRUE DO BEGIN
                        IF (prev_x <> x) OR (prev_y <> y) OR (prev_ofsy <> ofsy) THEN BEGIN
                                scr.locate(x, y);
                                prev_x := x;
                                prev_y := y;
                                scr.cls(CLR_TEXT);
                                i := 0;
                                aptr := LocateLine(abody, ofsy);
                                WHILE (ofsy + i < lines) AND (i < height) DO BEGIN
                                        PrintLine(i, width, abody, ctx.id, aptr, nidx, (ofsy + i) = 0, i = y);
                                        INC(i);
                                END;
                                high_light(hlchars);
                                scr.hprint(0, height, $70, ' ', width);
                                scr.printhl(0, height, $70, $74, HELP_STR);
                                scr.show;
                                prev_ofsy := ofsy;
                        END;
                        key := kbd_getkey;
                        CASE hi(key) OF
                        SCAN_ESC: BEGIN
                                stop := TRUE;
                                BREAK;
                        END;
                        SCAN_UP: BEGIN
                                IF y > 0 THEN BEGIN
                                        DEC(y);
                                END ELSE IF (y = 0) AND (ofsy > 0) THEN BEGIN
                                        DEC(ofsy);
                                END;
                        END;
                        SCAN_DOWN: BEGIN
                                IF (y < height - 1) AND (y + ofsy + 1< lines) THEN BEGIN
                                        INC(y);
                                END ELSE IF (y = height - 1) AND (ofsy + height < lines) THEN BEGIN
                                        INC(ofsy);
                                END;
                        END;
                        SCAN_LEFT: BEGIN
                                IF x > 0 THEN BEGIN
                                        DEC(x);
                                END;
                        END;
                        SCAN_RIGHT: BEGIN
                                INC(x);
                        END;
                        SCAN_HOME: x := 0;
                        SCAN_END: x := width - 1;
                        SCAN_BS: BEGIN
                                IF ctx.hptr <> 0 THEN BEGIN
                                        DEC(ctx.hptr);
                                        ctx.id := ctx.history[ctx.hptr];
                                        BREAK;
                                END;
                        END;
                        SCAN_F5: BEGIN 
                                IF ctx.hptr > HISTORY_SIZE THEN BEGIN
                                        Move(ctx.history[1], ctx.history[0], HISTORY_SIZE*2);
                                END;
                                ctx.history[ctx.hptr] := ctx.id;
                                IF ctx.hptr <= HISTORY_SIZE THEN INC(ctx.hptr);
                                ctx.id := 0;
                                BREAK;
                        END;
                        SCAN_PGUP: BEGIN
                                IF y <> 0 THEN BEGIN
                                        y := 0; x := 1;
                                END ELSE IF ofsy > height THEN BEGIN
                                        DEC(ofsy, height - 1);
                                END ELSE BEGIN
                                        ofsy := 0; x := 1; y := 0;
                                END;
                        END;
                        SCAN_PGDN: BEGIN
                                IF y < height - 1 THEN BEGIN
                                        IF ofsy + height - 1 < lines THEN y := height - 1
                                        ELSE y := lines - ofsy - 1;
                                END ELSE IF ofsy + height < lines THEN BEGIN
                                        INC(ofsy, height);
                                        IF ofsy + y >= lines THEN y := lines - ofsy - 1;
                                END ELSE y := lines - ofsy - 1;
                        END;
                        SCAN_ENTER: BEGIN
                                aptr := LocateLine(abody, ofsy + y);
                                IF abody[aptr] = CHR(DWH_LT_LINK) THEN BEGIN
                                        INC(aptr);
                                        idx := @abody[aptr];
                                        nidx := idx^;
                                        IF nidx < acount THEN BEGIN
                                                IF ctx.hptr > HISTORY_SIZE THEN BEGIN
                                                        Move(ctx.history[1], ctx.history[0], HISTORY_SIZE*2);
                                                END;
                                                ctx.history[ctx.hptr] := ctx.id;
                                                IF ctx.hptr <= HISTORY_SIZE THEN INC(ctx.hptr);
                                                ctx.id := nidx;
                                                BREAK;
                                        END;
                                END;
                        END;
                        END;
                END;
        END;
        FreeMem(abody, 65535);
END;

FUNCTION dwh_view(VAR ctx : TDWH_VIEW_CTX) : INTEGER;
VAR     f    : BFILE;
        r, i : INTEGER;
        fofs : LONGINT;
BEGIN
        Assign(f, ctx.fname);
        {$I-}
        Reset(f);
        i := IOResult;
        {$I+}
        IF i <> 0 THEN r := DWH_ERR_OPEN
        ELSE BEGIN
                fofs := dwh_GetHdrOfs(f, fofs);
                IF fofs <> DWH_HEADER_NOT_FOUND THEN BEGIN
                        scr.push;
                        set_blink(FALSE);
                        VIEW(ctx, f, fofs);
                        scr.pop;
                        scr.show;
                        scr.set_cursor(scr.cursor);
                        r := DWH_ERR_OK;
                END ELSE r := DWH_ERR_INVALID_FMT;
        END;
        Close(f);
        dwh_view := r;
END;

END.
