//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// About window.
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

unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fileinfo, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    imgIcon: TImage;
    lblAppName: TLabel;
    lblCopyright: TLabel;
    lblRepoID: TLabel;
    lblURL: TLabel;
    lblVersion: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
var
  versionInfo: TFileVersionInfo;
  lblLength: Integer;
begin
  // Extract version information of the application executable.
  versionInfo := TFileVersionInfo.Create(nil);
  versionInfo.ReadFileInfo;
  lblAppName.Caption := versionInfo.VersionStrings.Values['FileDescription'];
  lblVersion.Caption := 'Version: ' + versionInfo.VersionStrings.Values['ProductVersion'] + '   (' + versionInfo.VersionStrings.Values['FileVersion'] + ')';
  lblCopyright.Caption := versionInfo.VersionStrings.Values['LegalCopyright'];
  lblURL.Caption := versionInfo.VersionStrings.Values['Comments'];
  FreeAndNil(versionInfo);

  // Show complete URL label (This routine is used to fix label auto size issue in FPC).
  lblLength := lblURL.Width + 5;
  lblURL.AutoSize := false;
  lblURL.Width := lblLength;
  lblURL.Height := 20;
end;

procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: char);
begin
  if(key = #27) then
  begin
    close;
  end;
end;

procedure TfrmAbout.lblURLClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
end;

end.

