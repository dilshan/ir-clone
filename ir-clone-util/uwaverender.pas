//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Waveform render.
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

unit uwaverender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASeries, Graphics, ucommon;

type
  TWaveFormRender = class(TThread)
    private
      FCycleCount: Integer;
      FTotalTime: Double;
      FMemBuffer: TMemoryStream;
      renderBuffer: TMemoryStream;
      line: TLineSeries;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean; lineSeries: TLineSeries);
      destructor Destroy; override;
      property DataBuffer : TMemoryStream read FMemBuffer write FMemBuffer;
      property CycleCount : Integer read FCycleCount;
      property TotalTime : Double read FTotalTime;
  end;

const
  LEVEL_HIGH : Integer = 1;
  LEVEL_LOW : Integer = 0;

implementation

constructor TWaveFormRender.Create(CreateSuspended: Boolean; lineSeries: TLineSeries);
begin
  inherited Create(CreateSuspended);
  renderBuffer := TMemoryStream.Create;
  line := lineSeries;
  FCycleCount := 0;
  FTotalTime := 0;
end;

destructor TWaveFormRender.Destroy;
begin
  FreeAndNil(renderBuffer);
  inherited;
end;

procedure TWaveFormRender.Execute;
var
  currentTime: double;
  wavePos: Integer;
  isLevelHigh: Boolean;
  timeline: Double;
begin
  isLevelHigh := false;
  FCycleCount := 0;
  FTotalTime := 0;

  line.BeginUpdate;

  // Clear existing graph.
  line.Clear;
  renderBuffer.CopyFrom(FMemBuffer, FMemBuffer.Size);
  renderBuffer.Position := 0;
  isLevelHigh := true;
  timeline := 0;

  // Plot each edge into the line graph.
  while((renderBuffer.Position < renderBuffer.Size) and (not Suspended)) do
  begin
    // Get the logic level of the current step.
    if(isLevelHigh) then
      wavePos := LEVEL_HIGH
    else
      wavePos := LEVEL_LOW;

    isLevelHigh := not isLevelHigh;
    inc(FCycleCount);

    // Add logic level and time into the line series.
    currentTime := renderBuffer.ReadWord * STEP_TO_MICRO_SEC;
    timeline := timeline + currentTime;
    line.AddXY(timeline, wavePos, '', DEFAULT_GRAPH_COLOR);
  end;

  // End of line updating.
  FTotalTime := timeline;
  renderBuffer.Position := 0;
  line.EndUpdate;
end;

end.

