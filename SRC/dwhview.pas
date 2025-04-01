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
TSTATE = RECORD
        id   : WORD;
        ofsy : WORD;
        x, y : WORD;
END;

TDWH_VIEW_CTX = RECORD
        fname     : STRING;
        id        : WORD;
        history   : ARRAY[0..HISTORY_SIZE] OF WORD;
        hptr      : INTEGER;
        issel     : BOOLEAN;
        selection : TSTATE;
        cur, prev : TSTATE;
END;

PROCEDURE init(VAR ctx : TDWH_VIEW_CTX; id : WORD; fname : STRING);
FUNCTION dwh_view(VAR ctx : TDWH_VIEW_CTX) : INTEGER;

IMPLEMENTATION

USES System2, DWH, DWHUTIL, scr, KMInput, scrui, Event, str, WinCB;

{$IFNDEF MS_COLOR_SCHEME}
{$IFNDEF BORLAND_COLOR_SCHEME}
{$DEFINE BORLAND_COLOR_SCHEME}
{$ENDIF}
{$ENDIF}


CONST
HELP_STR : STRING
= ' ~F3~ Load  ~F5~ Index  ~'#$11+#$D9+'~ Enter topic  ~'+#$18+#$19+'~ Move  ~BS~ Back  ~Ctrl~+~C~ Copy';

{$IFDEF MS_COLOR_SCHEME}
CLR_TEXT     = $17;
CLR_TEXT2    = $1F;
CLR_LINK     = $1E;
CLR_LINE     = $1F;
CLR_HITEXT   = $1B;
CLR_HILINK   = $70;
CLR_TITLE    = $1F;
CLR_SEL_TEXT = $70;
{$ENDIF}
{$IFDEF BORLAND_COLOR_SCHEME}
CLR_TEXT     = $30;
CLR_TEXT2    = $38;
CLR_LINK     = $3E;
CLR_LINE     = $38;
CLR_HITEXT   = $3F;
CLR_HILINK   = $70;
CLR_TITLE    = $3E;
CLR_SEL_TEXT = $07;
{$ENDIF}

LINE_CHAR    = #$C4;
LINK_CHAR    = #$10;

FUNCTION Min(w1, w2 : WORD) : WORD;
BEGIN
        IF w1 < w2 THEN Min := w1 ELSE Min := w2;
END;

FUNCTION Max(w1, w2 : WORD) : WORD;
BEGIN
        IF w1 > w2 THEN Max := w1 ELSE Max := w2;
END;

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

PROCEDURE high_light(y, width : WORD; VAR hl_chars : STRING);
VAR     p      : PCHAR;
BEGIN
        p := @scr.screen[y * (width SHL 1)];
        WHILE width <> 0 DO BEGIN
                IF p^ <> ' ' THEN IF p[1] = CHR(CLR_TEXT) THEN BEGIN
                        p[1] := hl_chars[ORD(p^) + 1];
                END;
                INC(p, 2);
                DEC(width);
        END;
END;

FUNCTION IsInSelBounds(y : WORD; VAR ctx : TDWH_VIEW_CTX) : BOOLEAN;
VAR
        y0, y1, y2 : WORD;
BEGIN
        y0 := ctx.cur.ofsy + y;
        y1 := ctx.selection.ofsy + ctx.selection.y;
        y2 := ctx.cur.ofsy + ctx.cur.y;
        IsInSelBounds := ctx.issel
        AND (Min(y1, y2) <= y0) AND (y0 <= Max(y1, y2));
END;

PROCEDURE PrintLine(y, width : INTEGER;
        abody : PCHAR;
        VAR aptr, nid : WORD;
        istitle, isselrow : BOOLEAN;
        VAR hl_chars : STRING;
        VAR ctx : TDWH_VIEW_CTX);
VAR
        s     : STRING;
        c, ch : BYTE;
        idx   : ^WORD;
BEGIN
        CASE abody[aptr] OF
        CHR(DWH_LT_TEXT): BEGIN
                INC(aptr);
                Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                INC(aptr, ORD(abody[aptr]) + 1);
                s := dwh_rle_unpack(s);
                c := CLR_TEXT;
                ch := CLR_HITEXT;
                IF istitle THEN BEGIN
                        c := CLR_TITLE;
                        s := wtoa(ctx.id) + ': ' + s;
                        upstr(s);
                END;
                IF IsInSelBounds(y, ctx) THEN BEGIN
                        ch := CLR_SEL_TEXT;
                        c := CLR_SEL_TEXT;
                        hprint(1, y, CLR_SEL_TEXT, ' ', width - 1);
                END;
                scr.printhl(1, y, c, ch, s);
                IF NOT istitle THEN high_light(y, width, hl_chars);
        END;
        CHR(DWH_LT_LINK): BEGIN
                INC(aptr);
                idx := @abody[aptr];
                nid := idx^;
                INC(aptr, 2);
                Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                INC(aptr, ORD(abody[aptr]) + 1);
                s := dwh_rle_unpack(s);
                s := LINK_CHAR + s;
                IF isselrow THEN c := CLR_HILINK ELSE c := CLR_LINK;
                IF IsInSelBounds(y, ctx) THEN BEGIN
                        c := CLR_SEL_TEXT;
                END;
                scr.print(1, y, c, s);
        END;
        CHR(DWH_LT_LINE): BEGIN
                INC(aptr);
                IF IsInSelBounds(y, ctx) THEN BEGIN
                        c := CLR_SEL_TEXT;
                END ELSE c := CLR_LINE;
                scr.hprint(1, y, c, LINE_CHAR, width - 2);
        END;
        END;
END;

PROCEDURE CopyCB(VAR ctx : TDWH_VIEW_CTX; abody : PCHAR);
VAR     iswincb   : BOOLEAN;
        i, y1, y2 : WORD;
        aptr      : WORD;
        cb, cbc   : PCHAR;
        s         : STRING;
BEGIN
        iswincb := WCB_Detect;
        y1 := ctx.selection.ofsy + ctx.selection.y;
        y2 := ctx.cur.ofsy + ctx.cur.y;
        i := y1;
        y1 := Min(i, y2);
        y2 := Max(i, y2);
        i := y1;
        GetMem(cb, 65000);
        cbc := cb;
        aptr := LocateLine(abody, i);
        WHILE i <= y2 DO BEGIN
                s := '';
                CASE abody[aptr] OF
                CHR(DWH_LT_TEXT): BEGIN
                        INC(aptr);
                        Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                        INC(aptr, ORD(abody[aptr]) + 1);
                        s := dwh_rle_unpack(s);
                END;
                CHR(DWH_LT_LINK): BEGIN
                        INC(aptr);
                        INC(aptr, 2);
                        Move(abody[aptr], s[0], ORD(abody[aptr]) + 1);
                        INC(aptr, ORD(abody[aptr]) + 1);
                        s := '>' + dwh_rle_unpack(s);
                END;
                CHR(DWH_LT_LINE): BEGIN
                        INC(aptr);
                        s := '---------------';
                END;
                END;
                Move(s[1], cbc^, ORD(s[0]));
                INC(cbc, ORD(s[0]));
                cbc^ := #$0D;
                INC(cbc);
                cbc^ := #$0A;
                INC(cbc);
                INC(i);
        END;
        IF iswincb THEN WCB_Copy(cb^, cbc - cb);
        FreeMem(cb, 65000);
END;

FUNCTION Load_help(VAR ctx : TDWH_VIEW_CTX; VAR f : BFILE; VAR hdrofs : LONGINT) : BOOLEAN;
VAR
        width, height : INTEGER;
        event : TEvent;
        fname : STRING;
        r     : BOOLEAN;
BEGIN
        r := FALSE;
        height := scr.getheight - 1;
        width := scr.getwidth;
        fname := ctx.fname;
        scr.hprint(0, height, $70, ' ', width);
        scr.print(0, height, $70, 'Load:');
        WHILE TRUE DO BEGIN
                scr.show;
                scrui.editstr(event, 6, height, $70, fname, 32, 255);
                if event.etype = KEYBOARD then begin
                        if event.scancode = SCAN_ESC then begin break; end;
                        if event.scancode = SCAN_ENTER then begin
                                IF dwh_lookup(fname) THEN BEGIN
                                        Close(f);
                                        Assign(f, fname);
                                        Reset(f);
                                        hdrofs := dwh_GetHdrOfs(f, hdrofs);
                                        ctx.fname := fname;
                                        ctx.prev.x := 65000;
                                        ctx.prev.y := 65000;
                                        ctx.prev.ofsy := 65000;
                                        ctx.prev.id := 65000;
                                        ctx.id := 0;
                                        r := TRUE;
                                END;
                                BREAK;
                        END;
                END;
        END;

        scr.hprint(0, height, $70, ' ', width);
        scr.printhl(0, height, $70, $74, HELP_STR);
        scr.show;
        Load_help := r;
END;

PROCEDURE VIEW(VAR ctx : TDWH_VIEW_CTX; VAR f : BFILE; hdrofs : LONGINT);
VAR
        len     : WORD;
        ofsfile : LONGINT;
        i, acount, size, lines, rlines : WORD;
        t       : WORD;
        abody   : PCHAR;
        aptr    : WORD;
        s       : STRING;
        idx     : ^WORD;
        nidx    : WORD;
        key     : WORD;
        visible_start, visible_len : WORD;
        width, height : INTEGER;
        c       : BYTE;
        stop    : BOOLEAN;
        hlchars : STRING;
        isshift : BOOLEAN;
        isctrl  : BOOLEAN;
BEGIN
        scr.cursor_big;
        kbd_reset;
        height := scr.getheight - 1;
        width := scr.getwidth;
        stop := FALSE;

        scr.cls(CLR_TEXT);
        scr.hprint(0, height, $70, ' ', width);
        scr.printhl(0, height, $70, $74, HELP_STR);

        acount := dwh_GetArtCount(f, hdrofs);
        s := '';
        FOR i := $B3 TO $DA DO s := s + CHR(i);
        s := '`^#@*_.,?+*/\-()!:;"=<>(){}[]~|' + s + #$27;
        FillChar(hlchars[1], 255, CHR(CLR_TEXT));
        hlchars[0] := #$FF;
        FOR i := 1 TO ORD(s[0]) DO hlchars[ORD(s[i]) + 1] := CHR(CLR_TEXT2);

        GetMem(abody, 65535);
        WHILE (NOT stop) AND (ctx.id < acount) DO BEGIN
                acount := dwh_GetArtCount(f, hdrofs);
                Seek(f, dwh_GetArtOfs(f, hdrofs, ctx.id));
                dwh_read(f, t, SizeOf(WORD));
                dwh_read(f, size, SizeOf(WORD));
                dwh_read(f, lines, SizeOf(WORD));
                dwh_read(f, abody^, size);
                rlines := lines;
                ctx.cur.ofsy := 0; ctx.cur.x := 1; ctx.cur.y := 2;
                isshift := FALSE;
                ctx.prev.x := 65000;
                ctx.prev.y := 65000;
                ctx.prev.ofsy := 65000;
                ctx.prev.id := 65000;
                WHILE TRUE DO BEGIN
                        IF (ctx.prev.x <> ctx.cur.x) OR (ctx.prev.y <> ctx.cur.y) OR (ctx.prev.ofsy <> ctx.cur.ofsy) THEN BEGIN
                                visible_start := 0;
                                visible_len := height;
                                IF (ctx.prev.ofsy = ctx.cur.ofsy)
                                        AND((ctx.prev.y + 1 = ctx.cur.y)
                                        OR (ctx.prev.y - 1 = ctx.cur.y)) THEN BEGIN
                                        visible_start := Min(ctx.prev.y, ctx.cur.y);
                                        visible_len := 2;
                                END ELSE IF (ctx.prev.ofsy = ctx.cur.ofsy) AND (ctx.prev.y = ctx.cur.y) THEN BEGIN
                                        visible_start := ctx.cur.y;
                                        visible_len := 1;
                                END ELSE IF (ctx.prev.ofsy + 1 = ctx.cur.ofsy) THEN BEGIN
                                        scroll_up(0, 0, width, height - 1, 1);
                                        visible_start := height - 2;
                                        visible_len := 2;
                                END ELSE IF (ctx.prev.ofsy - 1 = ctx.cur.ofsy) THEN BEGIN
                                        scroll_down(0, 0, width, height - 1, 1);
                                        visible_start := 0;
                                        visible_len := 2;
                                END;
                                IF visible_len <> 0 THEN BEGIN
                                        INC(visible_len, visible_start);
                                        i := visible_start;
                                        aptr := LocateLine(abody, ctx.cur.ofsy + i);
                                        WHILE (i < visible_len) AND (i < height) DO BEGIN
                                                scr.hprint(0, i, CLR_TEXT, ' ', width);
                                                IF ctx.cur.ofsy + i < lines THEN BEGIN
                                                        PrintLine(i, width, abody, aptr, nidx
                                                        , (ctx.cur.ofsy OR i) = 0, i = ctx.cur.y, hlchars, ctx);
                                                END;
                                                INC(i);
                                        END;
                                END;
                                IF (ctx.prev.x <> ctx.cur.x) OR (ctx.prev.y <> ctx.cur.y) THEN BEGIN
                                        scr.locate(ctx.cur.x, ctx.cur.y);
                                END;
                                ctx.prev := ctx.cur;
                                scr.show;
                        END;
                        key := kbd_getkey;
                        isshift := is_shift(kbd_getflags);
                        isctrl := is_ctrl(kbd_getflags);
                        CASE hi(key) OF
                        SCAN_ESC: BEGIN
                                stop := TRUE;
                                BREAK;
                        END;
                        SCAN_UP: BEGIN
                                IF ctx.cur.y > 0 THEN BEGIN
                                        DEC(ctx.cur.y);
                                END ELSE IF (ctx.cur.y = 0) AND (ctx.cur.ofsy > 0) THEN BEGIN
                                        DEC(ctx.cur.ofsy);
                                END;
                        END;
                        SCAN_DOWN: BEGIN
                                IF (ctx.cur.y < height - 1) AND (ctx.cur.y + ctx.cur.ofsy + 1< lines) THEN BEGIN
                                        INC(ctx.cur.y);
                                END ELSE IF (ctx.cur.y = height - 1) AND (ctx.cur.ofsy + height < lines) THEN BEGIN
                                        INC(ctx.cur.ofsy);
                                END;
                        END;
                        SCAN_LEFT: IF ctx.cur.x > 1 THEN DEC(ctx.cur.x);
                        SCAN_RIGHT: IF ctx.cur.x < width - 1 THEN INC(ctx.cur.x);
                        SCAN_HOME: ctx.cur.x := 1;
                        SCAN_END: ctx.cur.x := width - 1;
                        SCAN_BS: BEGIN
                                IF ctx.hptr <> 0 THEN BEGIN
                                        DEC(ctx.hptr);
                                        ctx.id := ctx.history[ctx.hptr];
                                        ctx.issel := FALSE;
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
                                IF ctx.cur.y <> 0 THEN BEGIN
                                        ctx.cur.y := 0; ctx.cur.x := 1;
                                END ELSE IF ctx.cur.ofsy > height THEN BEGIN
                                        DEC(ctx.cur.ofsy, height - 1);
                                END ELSE BEGIN
                                        ctx.cur.ofsy := 0; ctx.cur.x := 1; ctx.cur.y := 0;
                                END;
                        END;
                        SCAN_PGDN: BEGIN
                                IF ctx.cur.y < height - 1 THEN BEGIN
                                        IF ctx.cur.ofsy + height - 1 < lines THEN ctx.cur.y := height - 1
                                        ELSE ctx.cur.y := lines - ctx.cur.ofsy - 1;
                                END ELSE IF ctx.cur.ofsy + height < lines THEN BEGIN
                                        INC(ctx.cur.ofsy, height);
                                        IF ctx.cur.ofsy + ctx.cur.y >= lines THEN ctx.cur.y := lines - ctx.cur.ofsy - 1;
                                END ELSE ctx.cur.y := lines - ctx.cur.ofsy - 1;
                        END;
                        SCAN_F3: IF Load_help(ctx, f, hdrofs) THEN BEGIN
                                acount := dwh_GetArtCount(f, hdrofs);
                                BREAK;
                        END;
                        SCAN_ENTER: BEGIN
                                aptr := LocateLine(abody, ctx.cur.ofsy + ctx.cur.y);
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
                        SCAN_C: IF isctrl AND ctx.issel THEN CopyCB(ctx, abody);
                        END;
                        IF isshift AND (NOT ctx.issel) THEN BEGIN
                                ctx.issel := TRUE;
                                ctx.selection := ctx.prev;
                        END ELSE IF (NOT isshift) AND ctx.issel THEN BEGIN
                                ctx.issel := FALSE;
                                ctx.prev.x := 65000;
                                ctx.prev.y := 65000;
                                ctx.prev.ofsy := 65000;
                        END;
                        IF ctx.issel THEN ctx.cur.x := 1;
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
