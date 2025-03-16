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

{$A-}
{$DEFINE DWH_CRYPT1}
UNIT DWH;

INTERFACE

USES System2;

CONST
DWH_LT_TEXT = $01;
DWH_LT_LINE = $02;
DWH_LT_LINK = $03;

DWH_ET_ART  = $04;
DWH_ET_IMG  = $05;

DWH_HEADER_NOT_FOUND = -1;

{$IFDEF DWH_CRYPT}
TYPE
TDWH_MAGIC = ARRAY[0..6] OF CHAR;
CONST
DWH_MAGIC : TDWH_MAGIC = 'DWHFC1'+#0;
{$ENDIF}

{$IFNDEF DWH_CRYPT}
TYPE
TDWH_MAGIC = ARRAY[0..5] OF CHAR;
CONST
DWH_MAGIC : TDWH_MAGIC = 'DWHF1'+#0;
{$ENDIF}


TYPE
DWH_FILE = BFILE;

IMPLEMENTATION

END.
