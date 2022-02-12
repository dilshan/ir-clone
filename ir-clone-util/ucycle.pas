//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Cycle definition frame.
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

unit ucycle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  Spin, ucommon;

type

  { TfmCycle }

  TfmCycle = class(TFrame)
    btnShow: TSpeedButton;
    btnInsert: TSpeedButton;
    btnMoveBottom: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnDelete: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnMoveTop: TSpeedButton;
    bvSep1: TBevel;
    bvSep2: TBevel;
    bvSep3: TBevel;
    lblsHiLen: TLabel;
    lblsLoLen1: TLabel;
    lblsHiUs: TLabel;
    lblsLoUs: TLabel;
    btnSwap: TSpeedButton;
    txtPosLen: TSpinEdit;
    txtNegLen: TSpinEdit;
    txtPosition: TEdit;
    lblsPosition: TLabel;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnMoveBottomClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveTopClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnSwapClick(Sender: TObject);
    procedure txtNegLenChange(Sender: TObject);
  private
    FPosition : Integer;
    FOnValueSwap: TNotifyEvent;
    FOnValueChanged: TNotifyEvent;
    mainWindow: TForm;
    procedure SetPosition(pos: Integer);
    procedure SetCycleValues(data: TCycleData);
    function GetCycleValues() : TCycleData;
  public
    Constructor Create(AOwner: TComponent; baseWindow: TForm); reintroduce;
    property CyclePosition : Integer read FPosition write SetPosition;
    property CycleData : TCycleData read GetCycleValues write SetCycleValues;
    property OnValueSwap : TNotifyEvent read FOnValueSwap write FOnValueSwap;
    property OnValueChanged : TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;

implementation

uses
  umain;

{$R *.lfm}

Constructor TfmCycle.Create(AOwner: TComponent; baseWindow: TForm);
begin
  inherited Create(AOwner);
  mainWindow :=  baseWindow;
end;

procedure TfmCycle.btnSwapClick(Sender: TObject);
var
  tempVal : Integer;
begin
  // Swap positive and negative values.
  tempVal := txtNegLen.Value;
  txtNegLen.Value := txtPosLen.Value;

  if(tempVal < txtPosLen.MinValue) then
  begin
    // Adjust positive minimum.
    tempVal := txtPosLen.MinValue;
  end;

  txtPosLen.Value := tempVal;

  // Raise swap event.
  if(Assigned(FOnValueSwap)) then
  begin
    FOnValueSwap(self);
  end;
end;

procedure TfmCycle.txtNegLenChange(Sender: TObject);
begin
  if(Assigned(FOnValueChanged)) then
  begin
    FOnValueChanged(self);
  end;
end;

procedure TfmCycle.btnMoveUpClick(Sender: TObject);
begin
  TfrmMain(mainWindow).MoveCyclePosition((FPosition - 1), mvUp);
end;

procedure TfmCycle.btnShowClick(Sender: TObject);
begin
  TfrmMain(mainWindow).HighlightWaveformCycle((FPosition - 1) * 2);
end;

procedure TfmCycle.btnMoveDownClick(Sender: TObject);
begin
  TfrmMain(mainWindow).MoveCyclePosition((FPosition - 1), mvDown);
end;

procedure TfmCycle.btnMoveBottomClick(Sender: TObject);
begin
  TfrmMain(mainWindow).MoveCyclePosition((FPosition - 1), mvBottom);
end;

procedure TfmCycle.btnDeleteClick(Sender: TObject);
begin
  TfrmMain(mainWindow).DeleteCycle(FPosition - 1);
end;

procedure TfmCycle.btnInsertClick(Sender: TObject);
begin
  TfrmMain(mainWindow).InsertCycles(FPosition);
end;

procedure TfmCycle.btnMoveTopClick(Sender: TObject);
begin
  TfrmMain(mainWindow).MoveCyclePosition((FPosition - 1), mvTop);
end;

procedure TfmCycle.SetPosition(pos: Integer);
begin
  FPosition := pos;
  txtPosition.Text := IntToStr(pos);

  // Disable navigative time of the 1st panel.
  txtNegLen.Enabled := (pos > 1);
  if(txtNegLen.Enabled) then
  begin
    txtNegLen.MinValue := MINIMUM_TIME_VALUE;
  end
  else
  begin
    txtNegLen.MinValue := 0;
    txtNegLen.Value := 0;
  end;
end;

procedure TfmCycle.SetCycleValues(data: TCycleData);
begin
  // For the first element the value is always zero.
  if(FPosition > 1) then
  begin
    if(data.LowLevelLength > 0) then
    begin
      txtNegLen.Value := data.LowLevelLength;
    end
    else
    begin
      // The minimum allowd value is 32.
      txtNegLen.Value := MINIMUM_TIME_VALUE;
    end;
  end
  else
  begin
    // For the 1st position, disable the control and set value to 0.
    txtNegLen.Value := 0;
  end;

  if(data.HighLevelLength = 0) then
  begin
    // The minimum allowd value is 32.
    txtPosLen.Value := MINIMUM_TIME_VALUE;
  end
  else
  begin
    txtPosLen.Value := data.HighLevelLength;
  end;
end;

function TfmCycle.GetCycleValues() : TCycleData;
begin
  result.LowLevelLength := txtNegLen.Value;
  result.HighLevelLength := txtPosLen.Value;
end;

end.

