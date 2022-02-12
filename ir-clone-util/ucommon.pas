//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Common system definitions.
//
// Copyright (c) 2022 Dilshan R Jayakody
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// Last updated: Dilshan Jayakody [18th Dec 2020]
//
// Update log:
// [18/12/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TCycleData = record
    HighLevelLength: Integer;
    LowLevelLength: Integer;
  end;

  TCycleConfig = record
    CycleData: TCycleData;
    CycleCount: Word;
  end;

  TMoveDirection = (mvUp, mvDown, mvTop, mvBottom);

const
  PROFILE_COUNT : Word = 16;
  PROFILE_BUFFER_SIZE : Word = 120;
  MINIMUM_TIME_VALUE : Word = 32;
  EEPROM_MAP_SIZE : Word = $1000;
  STEP_TO_MICRO_SEC : double = 31.831666667; // 190.99 / 6 (190.99 = sum of times of first 6 cycles of the NEC protocol).

  DEFAULT_GRAPH_COLOR : TColor = clNavy;
  GRAPH_HIGHLIGHT_COLOR : TColor = clRed;

implementation

end.

