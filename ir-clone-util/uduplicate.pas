//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Duplicate window.
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

unit uduplicate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TfrmDuplicate }

  TfrmDuplicate = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    lblsDuplicateID: TLabel;
    txtProfile: TEdit;
    updProfile: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure txtProfileEditingDone(Sender: TObject);
    procedure updProfileChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
  private
    FSourceProfile: Word;
  public
    property SourceProfile : Word read FSourceProfile write FSourceProfile;
  end;

var
  frmDuplicate: TfrmDuplicate;

implementation

{$R *.lfm}

{ TfrmDuplicate }

procedure TfrmDuplicate.FormShow(Sender: TObject);
begin
  Self.CancelControl := btnCancel;
  btnOK.Enabled := (updProfile.Position <> FSourceProfile);
end;

procedure TfrmDuplicate.txtProfileEditingDone(Sender: TObject);
begin
  btnOK.Enabled := (updProfile.Position <> FSourceProfile);
end;

procedure TfrmDuplicate.updProfileChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  btnOK.Enabled := (NewValue <> FSourceProfile);
end;

end.

