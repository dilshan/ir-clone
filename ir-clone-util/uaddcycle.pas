//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Add cycle window.
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

unit uaddcycle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin;

type

  { TfrmAddCycle }

  TfrmAddCycle = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblsHiUs: TLabel;
    lblsLoUs: TLabel;
    txtPosLen: TSpinEdit;
    txtCycleCount: TSpinEdit;
    lblsCycleNum: TLabel;
    lblsHiLen: TLabel;
    lblsLoLen: TLabel;
    txtNegLen: TSpinEdit;
  private

  public

  end;

var
  frmAddCycle: TfrmAddCycle;

implementation

{$R *.lfm}

end.

