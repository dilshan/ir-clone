//----------------------------------------------------------------------------------
// IR signal signal synthesizer.
// Main window file.
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

unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, TAGraph, TASeries, TATools, uaddcycle, ucommon, ucycle,
  math, uwaverender, uabout, uduplicate, Types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    beSep1: TBevel;
    beSep2: TBevel;
    beSep3: TBevel;
    btnInfo: TSpeedButton;
    btnAdd: TSpeedButton;
    btnDuplicate: TSpeedButton;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    btnNew: TSpeedButton;
    btnClear: TSpeedButton;
    ctWaveformView: TChartToolset;
    chWaveform: TChart;
    chWaveformLineSeries: TLineSeries;
    ctWaveformViewZoomDragTool1: TZoomDragTool;
    dlgFileOpen: TOpenDialog;
    GroupBox1: TGroupBox;
    lblActiveProfile: TLabel;
    lblCycleNum: TLabel;
    lblProfileCount: TLabel;
    lblsActiveProfile: TLabel;
    lblsLength: TLabel;
    lblsProfileCount: TLabel;
    lblsWaveformCount: TLabel;
    lblWaveformLength: TLabel;
    lblWaveformLengthMs: TLabel;
    pnlWaveform: TPanel;
    pnlProfile: TPanel;
    pnlProfileInfo: TPanel;
    dlgFileSave: TSaveDialog;
    sbWaveDefinition: TScrollBox;
    Splitter1: TSplitter;
    spWaveform: TSplitter;
    tmrUpdateGraph: TTimer;
    txtProfile: TEdit;
    lblProfile: TLabel;
    pnlToolbar: TPanel;
    updProfile: TUpDown;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDuplicateClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure ctWaveformViewZoomDragTool1AfterMouseUp(ATool: TChartTool;
      APoint: TPoint);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrUpdateGraphTimer(Sender: TObject);
    procedure txtProfileKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure updProfileChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure OnCycleValueChanged(Sender: TObject);
  private
    FBinFileName: string;
    cycleUID: Int64;
    lastProfile: Byte;
    updateGraph: Boolean;
    dataChanged: Boolean;
    isRenderGraph: Boolean;
    waveRender: TWaveFormRender;
    procedure OpenEEPROMFile(filename: string);
    procedure SaveEEPROMFile(filename: string);
    function BinToCycle(loData : Word; hiData: Word) : TCycleData;
    procedure OnWaveformRenderCompleted(ASender : TObject);
    procedure SetBinFileName(filename: string);
    procedure AddCycleEx(var cycleObj: TfmCycle; pos: Word; panelTop: Integer; data: TCycleData; isAdd : Boolean = true);
  public
    cycleList: TList;
    function ShowAddCycleWindow(var cycleData: TCycleConfig) : Boolean;
    procedure AddCycle(pos: Word; data: TCycleData);
    procedure MoveCyclePosition(index: Word; direction: TMoveDirection);
    procedure LoadProfile(profileID: byte);
    procedure UpdateWaveformGraph(profileID : byte);
    procedure UpdateWavetableData(profileID: byte; isUpdateGraph: boolean);
    procedure DeleteCycle(index: Word);
    procedure InsertCycles(index: Word);
    procedure HighlightWaveformCycle(index : Word);
    property BinFileName : string read FBinFileName write SetBinFileName;
  end;

const
  // Default values for "Add Cycle(s)" window.
  DEFAULT_CYCLE_COUNT : Word = 1;
  DEFAULT_LOW_LENGTH : Word = 600;
  DEFAULT_HIGH_LENGTH : Word = 600;

var
  frmMain: TfrmMain;
  wavetable: array of TMemoryStream;

resourcestring
  srFileOpenError = 'An error occurred while openning the file';
  srFileSaveError = 'An error occurred while saving the file';
  srUnsupportedFile = 'Unsupported file or corrupted file';
  srMemoryError = 'Invalid data or corrupted memory';
  srUnsavedData = 'Existing configuration is changed!' + LineEnding + 'Continue without saving the existing configuration?';
  srMaxCycleCount = 'Maximum cycle count is reached' + LineEnding + 'Unable to add more cycles to this profile.';
  srSourceEmpty = 'Source profile is empty';

implementation

{$R *.lfm}

procedure TfrmMain.SetBinFileName(filename: string);
begin
  FBinFileName := filename;

  // Update main window caption with filename.
  if(FBinFileName <> '') then
    Caption := Application.Title + ' - ' + ExtractFileName(filename)
  else
    Caption := Application.Title;
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  cycleData: TCycleConfig;
  cycPos: Integer;
begin
  // Check for maximum cycle limit.
  if(cycleList.Count >= ((PROFILE_BUFFER_SIZE / 2) - 2)) then
  begin
    MessageDlg(Application.Title, srMaxCycleCount, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    exit;
  end;

  // Assign default cycle data.
  cycleData.CycleCount := DEFAULT_CYCLE_COUNT;
  cycleData.CycleData.HighLevelLength := DEFAULT_HIGH_LENGTH;
  cycleData.CycleData.LowLevelLength := DEFAULT_LOW_LENGTH;

  // Show add cycle window.
  if(ShowAddCycleWindow(cycleData)) then
  begin
    try
      Screen.Cursor := crHourGlass;
      Invalidate;

      // Add cycles into the scroll box view.
      for cycPos := 0 to (cycleData.CycleCount - 1) do
      begin
        AddCycle(cycleList.Count + 1, cycleData.CycleData);
      end;
    finally
      // Update waveform output.
      UpdateWavetableData(lastProfile, true);

      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
var
  tempCycle: TfmCycle;
begin
  if(cycleList.Count > 0) then
  begin
    try
      // Hide wave definition to speedup the removal process.
      sbWaveDefinition.VertScrollBar.Position := 0;
      sbWaveDefinition.Visible := false;
      Invalidate;

      BeginFormUpdate;

      // Remove all the cycle panels in the wave definition panel.
      while(cycleList.Count > 0) do
      begin
        tempCycle := TfmCycle(cycleList.Items[0]);
        cycleList.Delete(0);
        FreeAndNil(tempCycle);
      end;
    finally
      sbWaveDefinition.Visible := true;
      EndFormUpdate;

      // Update waveform output.
      UpdateWavetableData(lastProfile, true);
    end;
  end;
end;

procedure TfrmMain.btnDuplicateClick(Sender: TObject);
var
  frmDuplicate: TfrmDuplicate;
  srcBuffer, destBuffer : TMemoryStream;
begin
  // Check for empty profile.
  if(wavetable[updProfile.Position].Size = 0) then
  begin
    // Unable to copy empty wavetable.
    MessageDlg(Application.Title, srSourceEmpty, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    exit;
  end;

  frmDuplicate := TfrmDuplicate.Create(self);

  // Set maximum and minimum limits of the profile selector.
  frmDuplicate.updProfile.Max := updProfile.Max;
  frmDuplicate.updProfile.Min := updProfile.Min;
  frmDuplicate.SourceProfile := updProfile.Position;

  // Selected different profile as default destination.
  if(updProfile.Position = updProfile.Min) then
    frmDuplicate.updProfile.Position := frmDuplicate.updProfile.Max
  else
    frmDuplicate.updProfile.Position := frmDuplicate.updProfile.Min;

  try
    if(frmDuplicate.ShowModal() = mrOK) then
    begin
      // Start copying current profile into selected profile.
      if(frmDuplicate.updProfile.Position = updProfile.Position) then
      begin
        // Ignore, because source and destiation profiles are same.
        exit;
      end;

      srcBuffer := wavetable[updProfile.Position];
      destBuffer := wavetable[frmDuplicate.updProfile.Position];

      // Start copying profile.
      srcBuffer.Position := 0;
      destBuffer.Clear;

      while(srcBuffer.Position < srcBuffer.Size) do
      begin
        destBuffer.WriteByte(srcBuffer.ReadByte);
      end;

      // Rewind both source and destination buffers.
      destBuffer.Position := 0;
      srcBuffer.Position := 0;
    end;
  finally
    FreeAndNil(frmDuplicate);
  end;
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
var
  frmAbout: TfrmAbout;
begin
  frmAbout := TfrmAbout.Create(self);
  frmAbout.ShowModal;
  FreeAndNil(frmAbout);
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
var
  msgStatus : Integer;
  profilePos: byte;
  profileBuffer: TMemoryStream;
  tempCycleData: TfrmAddCycle;
begin
  // Check for any unsaved changes.
  if(dataChanged) then
  begin
    // Unsaved changes are available, get user confirmation about the next action.
    msgStatus := MessageDlg(Application.Title, srUnsavedData, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
    if(msgStatus = mrNo) then
    begin
      // User cancel the file open action.
      exit;
    end;
  end;

  // Reset all waveform objects and variables to defaults.
  if(cycleList.Count > 0) then
  begin
    while(cycleList.Count > 0) do
    begin
      tempCycleData := TfrmAddCycle(cycleList.Items[0]);
      if(Assigned(tempCycleData)) then
      begin
        FreeAndNil(tempCycleData);
      end;
      cycleList.Delete(0);
    end;
  end;

  // Reset all profile objects and variables to defaults.
  for profilePos := 0 to (PROFILE_COUNT - 1) do
  begin
    profileBuffer := wavetable[profilePos];
    profileBuffer.Clear;
  end;

  // Clear waveform graph.
  chWaveformLineSeries.Clear;
  chWaveform.ZoomFull();

  lblActiveProfile.Caption := '0';
  lblProfileCount.Caption := IntToStr(PROFILE_COUNT);
  lblCycleNum.Caption := '0';
  lblWaveformLength.Caption := '0 µs';
  lblWaveformLengthMs.Caption := '(0 ms)';
  updProfile.Position := 0;

  tmrUpdateGraph.Enabled := true;

  // Update global vraibles to default values.
  dataChanged := false;
  BinFileName := '';
end;

procedure TfrmMain.OpenEEPROMFile(filename: string);
var
  fileBuffer : TMemoryStream;
  profileID : Byte;
  wavetableSize, pos, stepData: Word;
begin
  binFileName := filename;

  try
    fileBuffer := TMemoryStream.Create;
    fileBuffer.LoadFromFile(binFileName);

    // Load file content into each wavetable profiles.
    profileID := 0;
    while(profileID < PROFILE_COUNT) do
    begin
      wavetable[profileID].Clear;
      wavetableSize := fileBuffer.ReadByte;

      if((wavetableSize = $FF) or (wavetableSize = 0)) then
      begin
        // Unallocated wavetable profile. Continue to next profile.
        inc(profileID);
        fileBuffer.Position := (profileID * PROFILE_BUFFER_SIZE * 2);
        continue;
      end;

      if(wavetableSize > PROFILE_BUFFER_SIZE) then
      begin
        // Invalid profile size, may be invalid file?
        MessageDlg(Application.Title, srUnsupportedFile, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        FreeAndNil(fileBuffer);
        exit;
      end;

      if(wavetableSize > 0) then
      begin
        // Load wavetable into profile.
        for pos := 0 to (wavetableSize - 1) do
        begin
          stepData := fileBuffer.ReadByte shl 8;
          stepData := stepData + fileBuffer.ReadByte;
          wavetable[profileID].WriteWord(stepData);
        end;
      end;

      // Load next profile.
      inc(profileID);
      fileBuffer.Position := (profileID * PROFILE_BUFFER_SIZE * 2);
    end;

    // File loading is completed.
    FreeAndNil(fileBuffer);

    // Try to load 1st profile into the view.
    updProfile.Position := 0;
    LoadProfile(0);

    dataChanged := false;

  except on E : Exception do
    begin
      // File open / read operation has failed!
      MessageDlg(Application.Title, (srFileOpenError + LineEnding + E.Message), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);

      if(Assigned(fileBuffer)) then
      begin
        FreeAndNil(fileBuffer);
      end;
    end;
  end;
end;

procedure TfrmMain.OnWaveformRenderCompleted(ASender : TObject);
begin
  // Waveform generation is completed.
  lblCycleNum.Caption := IntToStr(waveRender.CycleCount);
  lblWaveformLength.Caption := Format('%.2f', [waveRender.TotalTime]) + ' µs';
  lblWaveformLengthMs.Caption := '(' + Format('%.2f', [waveRender.TotalTime / 1000]) + ' ms)';
  waveRender := nil;
  isRenderGraph := false;
end;

procedure TfrmMain.UpdateWaveformGraph(profileID : byte);
begin
  try
    // Check status of the existing thread.
    if(isRenderGraph) then
    begin
      exit;
    end;

    updateGraph := false;
    isRenderGraph := true;
    wavetable[profileID].Position := 0;

    // Create waveform rendering thread.
    waveRender := TWaveFormRender.Create(true, chWaveformLineSeries);
    waveRender.OnTerminate := @OnWaveformRenderCompleted;
    waveRender.FreeOnTerminate := true;
    waveRender.DataBuffer := wavetable[profileID];
    waveRender.Start;
  finally
  end;
end;

procedure TfrmMain.SaveEEPROMFile(filename: string);
var
  fileBuffer, profileBuffer: TMemoryStream;
  profileID, waveData: Word;
  dataLength: Word;
begin
  // Update wavetable data with UI settings.
  UpdateWavetableData(lastProfile, false);

  fileBuffer := TMemoryStream.Create;

  // Write each profile data into the memory stream.
  for profileID := 0 to (PROFILE_COUNT - 1) do
  begin
    dataLength := 0;
    profileBuffer := wavetable[profileID];

    // Check for valid profile buffer.
    if((profileBuffer.Size mod 2) <> 0) then
    begin
      // Invalid buffer size!, Corrupted profile memory.
      MessageDlg(Application.Title, srMemoryError, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      FreeAndNil(fileBuffer);
      exit;
    end;

    // Write profile size into the stream.
    fileBuffer.WriteByte(profileBuffer.Size div 2);

    // Write profile data into the file buffer.
    if(profileBuffer.Size > 0) then
    begin
      profileBuffer.Position := 0;
      // Write waveform data into the file buffer.
      while(profileBuffer.Position < profileBuffer.Size) do
      begin
        waveData := profileBuffer.ReadWord;
        fileBuffer.WriteByte((waveData shr 8) and $FF);
        fileBuffer.WriteByte(waveData and $FF);
      end;
    end;

    // Fill unused profile space.
    dataLength := profileBuffer.Position;
    while(dataLength < ((PROFILE_BUFFER_SIZE * 2) - 1)) do
    begin
      fileBuffer.WriteByte($FF);
      inc(dataLength);
    end;
  end;

  dataLength := fileBuffer.Size;

  // Fill unused EEPROM space at the end of the file.
  while(dataLength < EEPROM_MAP_SIZE) do
  begin
    fileBuffer.WriteByte($FF);
    inc(dataLength);
  end;

  // Write file buffer data into the file.
  try
    fileBuffer.Position := 0;
    fileBuffer.SaveToFile(filename);
  except on E : Exception do
    // File save operation has failed!
    MessageDlg(Application.Title, (srFileSaveError + LineEnding + E.Message), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;

  FreeAndNil(fileBuffer);
  dataChanged := false;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  msgStatus : Integer;
begin
  // Check for any unsaved changes.
  if(dataChanged) then
  begin
    // Unsaved changes are available, get user confirmation about the next action.
    msgStatus := MessageDlg(Application.Title, srUnsavedData, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
    if(msgStatus = mrNo) then
    begin
      // User cancel the file open action.
      exit;
    end;
  end;

  if(dlgFileOpen.Execute()) then
  begin
    // Open selected EEPROM file.
    OpenEEPROMFile(dlgFileOpen.FileName);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if(binFileName = '') then
  begin
    // Unsaved file, show file save dialog box.
    if(dlgFileSave.Execute()) then
    begin
      // User select / specify filename to save the data.
      binFileName := dlgFileSave.FileName;
    end
    else
    begin
      // User cancel file save dialog box.
      exit;
    end;
  end;

  // Perfrom file save operation.
  SaveEEPROMFile(binFileName);
end;

procedure TfrmMain.ctWaveformViewZoomDragTool1AfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
var
  tempPos: Integer;
begin
  tempPos := 0;

  // Reset entire waveform into default color.
  while(tempPos < chWaveformLineSeries.Count) do
  begin
    chWaveformLineSeries.SetColor(tempPos, DEFAULT_GRAPH_COLOR);
    inc(tempPos);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  pos: Integer;
begin
  // Initialize global variables.
  cycleUID := 0;
  cycleList := TList.Create;
  binFileName := '';
  lastProfile := $FF;
  updateGraph := false;
  isRenderGraph := false;

  // Create wavetables to save binary data of the waveforms.
  SetLength(wavetable, PROFILE_COUNT);
  for pos := 0 to (PROFILE_COUNT - 1) do
  begin
    wavetable[pos] := TMemoryStream.Create;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  pos: Integer;
begin
  // Release all allocated objects.
  if(cycleList <> nil) then
  begin
    FreeAndNil(cycleList);
  end;

  // Release wavetable.
  for pos := 0 to (PROFILE_COUNT - 1) do
  begin
    if(wavetable[pos] <> nil) then
    begin
      FreeAndNil(wavetable[pos]);
    end;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  // Assign default configuration into UI.
  updProfile.Max := (PROFILE_COUNT - 1);
  updProfile.Position := 0;
  lastProfile := updProfile.Position;

  // Update UI into default values.
  lblProfileCount.Caption := IntToStr(PROFILE_COUNT);

  if((Application.ParamCount > 0) and (FileExists(Application.Params[1]))) then
  begin
    // Try to open file specified in commandline arguments.
    OpenEEPROMFile(Application.Params[1]);
  end
  else
  begin
    // If commandline is not specified, open new document.
    btnNewClick(nil);
  end;
end;

procedure TfrmMain.tmrUpdateGraphTimer(Sender: TObject);
begin
  if(updateGraph) then
  begin
    UpdateWaveformGraph(lastProfile);
  end;
end;

procedure TfrmMain.txtProfileKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  tempAllow : Boolean;
begin
  if(Key = 13) then
  begin
    updProfile.Position := StrToIntDef(Trim(txtProfile.Text), updProfile.Position);

    // Load specified profile into the view.
    tempAllow := true;
    updProfileChangingEx(Sender, tempAllow, updProfile.Position, TUpDownDirection.updNone);
  end;
end;

procedure TfrmMain.updProfileChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if(NewValue <> lastProfile) then
  begin
    // Load new profile into UI.
    lastProfile := NewValue;
    LoadProfile(lastProfile);
  end;

  lblActiveProfile.Caption := IntToStr(lastProfile);
end;

function TfrmMain.BinToCycle(loData : Word; hiData: Word) : TCycleData;
begin
  result.LowLevelLength := Ceil(loData * STEP_TO_MICRO_SEC);
  result.HighLevelLength := Ceil(hiData * STEP_TO_MICRO_SEC);
end;

procedure TfrmMain.LoadProfile(profileID: byte);
var
  tempCycle: TfrmAddCycle;
  cyclePos: Word;
  profileBuffer: TMemoryStream;
  dataLo, dataHi: Word;
begin
  // Check the limit of the profile ID.
  if(profileID >= PROFILE_COUNT) then
  begin
    // Invalid profile ID.
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    sbWaveDefinition.Visible := false;
    Invalidate;

    tmrUpdateGraph.Enabled := false;

    BeginFormUpdate;

    // Move scroll box to top position to avoid screen redraw delays
    // at the delete operation.
    sbWaveDefinition.VertScrollBar.Position := 0;

    updProfile.Enabled := false;
    txtProfile.Enabled := false;

    // Clear existing wavetable view.
    while(cycleList.Count > 0) do
    begin
      if(cycleList.Items[0] <> nil) then
      begin
        tempCycle := TfrmAddCycle(cycleList.Items[0]);
        FreeAndNil(tempCycle);
      end;

      cycleList.Delete(0);
    end;

    profileBuffer := wavetable[profileID];
    profileBuffer.Position := 0;

    if(profileBuffer.Size < 1) then
    begin
      // Empty wavetable.
      EndFormUpdate;
      exit;
    end;

    // Start reading wavetable data and create cycles.
    cyclePos := 1;

    while(profileBuffer.Position < profileBuffer.Size) do
    begin
      dataLo := profileBuffer.ReadWord;
      dataHi := profileBuffer.ReadWord;

      AddCycle(cyclePos, BinToCycle(dataLo, dataHi));
      inc(cyclePos);
    end;

  finally
    // Update waveform graph.
    chWaveform.ZoomFull();
    UpdateWaveformGraph(profileID);

    updProfile.Enabled := true;
    txtProfile.Enabled := true;
    lastProfile := profileID;
    tmrUpdateGraph.Enabled := true;
    sbWaveDefinition.Visible := true;

    EndFormUpdate;
    Screen.Cursor := crDefault;
  end;
end;

function TfrmMain.ShowAddCycleWindow(var cycleData: TCycleConfig) : Boolean;
var
  frmAddCycle: TfrmAddCycle;
begin
  // Create new window instance and assign data structure values to UI controls.
  frmAddCycle := TfrmAddCycle.Create(self);
  frmAddCycle.CancelControl := frmAddCycle.btnCancel;

  // Set maximum limit of the cycles.
  frmAddCycle.txtCycleCount.MaxValue := ((PROFILE_BUFFER_SIZE / 2) - 2) - cycleList.Count;

  frmAddCycle.txtCycleCount.Value := cycleData.CycleCount;
  frmAddCycle.txtPosLen.Value := cycleData.CycleData.HighLevelLength;
  frmAddCycle.txtNegLen.Value := cycleData.CycleData.LowLevelLength;

  // If only one cycle is available then disable the cycle counter.
  if(frmAddCycle.txtCycleCount.MaxValue = frmAddCycle.txtCycleCount.MinValue) then
  begin
    frmAddCycle.txtCycleCount.Value := frmAddCycle.txtCycleCount.MaxValue;
    frmAddCycle.txtCycleCount.Enabled := false;
  end;

  if(frmAddCycle.ShowModal = mrOK) then
  begin
    // User accept the window.
    cycleData.CycleCount := frmAddCycle.txtCycleCount.Value;
    cycleData.CycleData.HighLevelLength := frmAddCycle.txtPosLen.Value;
    cycleData.CycleData.LowLevelLength := frmAddCycle.txtNegLen.Value;
    result := true;
  end
  else
  begin
    // User cancel the window.
    result := false;
  end;

  // Release window object.
  FreeAndnil(frmAddCycle);
end;

procedure TfrmMain.AddCycleEx(var cycleObj: TfmCycle; pos: Word; panelTop: Integer; data: TCycleData; isAdd : Boolean = true);
begin
  // Create new cycle panel.
  cycleObj.Name := 'CyclePanel' + IntToStr(cycleUID);

  // Increment UID value to provide unique component names.
  inc(cycleUID);

  // Add cycle panel into the scrollbox.
  cycleObj.Align := alTop;
  cycleObj.Top := panelTop;
  cycleObj.Parent := sbWaveDefinition;

  if(isAdd) then
  begin
    cycleList.Add(cycleObj)
  end
  else
  begin
    cycleList.Insert((pos - 1), cycleObj);
  end;

  // Update values of the cycles panel.
  cycleObj.CyclePosition := pos;
  cycleObj.CycleData := data;

  // Register event handlers.
  cycleObj.OnValueChanged := @OnCycleValueChanged;
  cycleObj.OnValueSwap := @OnCycleValueChanged;
end;

procedure TfrmMain.AddCycle(pos: Word; data: TCycleData);
var
  topPos : Integer;
  tempCycle : TfmCycle;
begin
  // Create new cycle panel.
  tempCycle := TfmCycle.Create(sbWaveDefinition, self);
  topPos := 0;

  // Add new cycle panel to the end of the list.
  if(cycleList.Count > 0) then
  begin
    if(cycleList.Items[cycleList.Count - 1] <> nil) then
    begin
      topPos := TfmCycle(cycleList.Items[cycleList.Count - 1]).Top + tempCycle.Height;
    end;
  end;

  // Add cycle panel into the scrollbox.
  AddCycleEx(tempCycle, pos, topPos, data);
end;

procedure TfrmMain.OnCycleValueChanged(Sender: TObject);
begin
  // Redraw graph on value change.
  UpdateWavetableData(lastProfile, true);
  dataChanged := true;
end;

procedure TfrmMain.MoveCyclePosition(index: Word; direction: TMoveDirection);
var
  targetPos: integer;
  targetTop, currentTop: integer;
  listTempPos: integer;
begin
  if((index = 0) and ((direction = mvUp) or (direction = mvTop))) then
  begin
    // Selected item is already at the top.
    exit;
  end;

  if((index = (cycleList.Count - 1)) and ((direction = mvDown) or (direction = mvBottom))) then
  begin
    // Selected item is already at the bottom.
    exit;
  end;

  // Mark this as data change.
  dataChanged := true;

  // Perform item movement.
  if(index < cycleList.Count) then
  begin
    BeginFormUpdate;

    if((direction = mvUp) or (direction = mvDown)) then
    begin
      // Item move up or down in the list.
      if(direction = mvUp) then
      begin
        // Move selected panel to one level up.
        targetPos := index - 1;
      end
      else
      begin
        // Move selected panel to one level down.
        targetPos := index + 1;
      end;

      targetTop := TfmCycle(cycleList.Items[targetPos]).Top;
      currentTop := TfmCycle(cycleList.Items[index]).Top;

      // Perform exchange and update position and top values.
      cycleList.Exchange(index, targetPos);
      TfmCycle(cycleList.Items[targetPos]).Top := targetTop;
      TfmCycle(cycleList.Items[targetPos]).CyclePosition := (targetPos + 1);

      TfmCycle(cycleList.Items[index]).Top := currentTop;
      TfmCycle(cycleList.Items[index]).CyclePosition := (index + 1);
    end
    else
    begin
      // Item moves to the top or bottom in the list.
      if(direction = mvTop) then
      begin
        // Move selected panel to the top.
        targetPos := index;

        while(targetPos >= 1) do
        begin
          targetTop := TfmCycle(cycleList.Items[targetPos - 1]).Top;
          listTempPos := TfmCycle(cycleList.Items[targetPos - 1]).CyclePosition;
          currentTop := TfmCycle(cycleList.Items[targetPos]).Top;

          cycleList.Exchange((targetPos - 1), targetPos);

          TfmCycle(cycleList.Items[targetPos - 1]).Top := targetTop;
          TfmCycle(cycleList.Items[targetPos]).Top := currentTop;

          TfmCycle(cycleList.Items[targetPos]).CyclePosition := TfmCycle(cycleList.Items[targetPos - 1]).CyclePosition ;
          TfmCycle(cycleList.Items[targetPos - 1]).CyclePosition := listTempPos;

          dec(targetPos);
        end;
      end
      else
      begin
        // Move selected panel to the bottom.
        targetPos := index;

        while(targetPos < (cycleList.Count - 1)) do
        begin
          targetTop := TfmCycle(cycleList.Items[targetPos + 1]).Top;
          listTempPos := TfmCycle(cycleList.Items[targetPos + 1]).CyclePosition;
          currentTop := TfmCycle(cycleList.Items[targetPos]).Top;

          cycleList.Exchange((targetPos + 1), targetPos);

          TfmCycle(cycleList.Items[targetPos + 1]).Top := targetTop;
          TfmCycle(cycleList.Items[targetPos]).Top := currentTop;

          TfmCycle(cycleList.Items[targetPos]).CyclePosition := TfmCycle(cycleList.Items[targetPos + 1]).CyclePosition ;
          TfmCycle(cycleList.Items[targetPos + 1]).CyclePosition := listTempPos;

          inc(targetPos);
        end;
      end;
    end;

    EndFormUpdate;

    // Update data buffer and output graph.
    UpdateWavetableData(lastProfile, true);
  end;
end;

procedure TfrmMain.UpdateWavetableData(profileID: byte; isUpdateGraph: boolean);
var
  pos, tempData: Word;
  dataBuffer: TMemoryStream;
begin
  // Check the limit of the profile ID.
  if(profileID >= PROFILE_COUNT) then
  begin
    // Invalid profile ID.
    exit;
  end;

  // Stop graph update timer.
  tmrUpdateGraph.Enabled := false;

  // Load selected data buffer and remove existing data.
  dataBuffer := wavetable[profileID];
  dataBuffer.Clear;

  // Update data buffer with values specified in UI.
  if(cycleList.Count > 0) then
  begin
    for pos := 0 to (cycleList.Count - 1) do
    begin
      tempData := ceil64(TfmCycle(cycleList.Items[pos]).CycleData.LowLevelLength / STEP_TO_MICRO_SEC);
      dataBuffer.WriteWord(tempData);
      tempData := ceil64(TfmCycle(cycleList.Items[pos]).CycleData.HighLevelLength / STEP_TO_MICRO_SEC);
      dataBuffer.WriteWord(tempData);
    end;
  end;

  dataBuffer.Position := 0;

  // Start graph update timer.
  tmrUpdateGraph.Enabled := true;

  // Update the waveform graph based on the specified flag.
  if(isUpdateGraph) then
  begin
    updateGraph := true;
  end;
end;

procedure TfrmMain.DeleteCycle(index: Word);
var
  tempCycle: TfmCycle;
  tempPosVal: Integer;
begin
  if(index = (cycleList.Count - 1)) then
  begin
    // Delete the last item.
    tempCycle := TfmCycle(cycleList.Items[index]);
    cycleList.Delete(index);
    FreeAndNil(tempCycle);
  end
  else
  begin
    // Extract panel details which is going to delete.
    tempCycle := TfmCycle(cycleList.Items[index]);
    cycleList.Delete(index);
    FreeAndNil(tempCycle);

    // Adjust position IDs of existing items.
    tempPosVal := index;
    while(tempPosVal < cycleList.Count) do
    begin
      TfmCycle(cycleList.Items[tempPosVal]).CyclePosition := TfmCycle(cycleList.Items[tempPosVal]).CyclePosition - 1;
      Inc(tempPosVal);
    end;
  end;

  // Update waveform with modified data.
  UpdateWavetableData(lastProfile, true);
end;

procedure TfrmMain.InsertCycles(index: Word);
var
  cycleData: TCycleConfig;
  tempPos, tempCount, topOffset, posOffset: Integer;
  tempPanel, refPanel: TfmCycle;
begin
  if(index = cycleList.Count) then
  begin
    // Item is inserted into the last cycle.
    btnAddClick(nil);
  end
  else
  begin
    // Item is inserted into the middle of the cycle list.

    // Check for maximum cycle limit.
    if(cycleList.Count >= ((PROFILE_BUFFER_SIZE / 2) - 2)) then
    begin
      MessageDlg(Application.Title, srMaxCycleCount, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      exit;
    end;

    // Assign default cycle data.
    cycleData.CycleCount := DEFAULT_CYCLE_COUNT;
    cycleData.CycleData.HighLevelLength := DEFAULT_HIGH_LENGTH;
    cycleData.CycleData.LowLevelLength := DEFAULT_LOW_LENGTH;

    // Show add cycle window.
    if(ShowAddCycleWindow(cycleData)) then
    begin
      // Adjust existing cycle panels to get the space for new panels.
      tempPos := index;
      topOffset := (TfmCycle(cycleList.Items[0]).Height) * cycleData.CycleCount;
      posOffset := cycleData.CycleCount;
      while(tempPos < cycleList.Count) do
      begin
        tempPanel := TfmCycle(cycleList.Items[tempPos]);
        tempPanel.Align := alNone;
        tempPanel.CyclePosition := tempPanel.CyclePosition + posOffset;
        tempPanel.Top := tempPanel.Top + topOffset;
        inc(tempPos);
      end;

      // insert new panels into the list.
      tempPos := index;
      refPanel := TfmCycle(cycleList.Items[tempPos]);

      for tempCount := 0 to (cycleData.CycleCount - 1) do
      begin
        tempPanel := TfmCycle.Create(sbWaveDefinition, self);
        AddCycleEx(tempPanel, (tempPos + 1), refPanel.Top + ((TfmCycle(cycleList.Items[tempPos]).Height) * (tempCount + 1)), cycleData.CycleData, false);
        Inc(tempPos);
      end;

      // Activate top alignment in all the cycle panels.
      for tempCount := 0 to (cycleList.Count - 1) do
      begin
        TfmCycle(cycleList.Items[tempCount]).Align := alTop;
      end;

      // Update waveform with modified data.
      UpdateWavetableData(lastProfile, true);
    end;
  end;
end;

procedure TfrmMain.HighlightWaveformCycle(index : Word);
var
  wavePos: Integer; fillColor: TColor;
begin
  if(chWaveformLineSeries.Count > 0) then
  begin
    // Switch waveform graph into full-view.
    wavePos := 0;
    chWaveform.ZoomFull();

    while(wavePos < chWaveformLineSeries.Count) do
    begin
      if((index = wavePos) or (index = (wavePos + 1))) then
        // Highlight position is detected.
        fillColor := GRAPH_HIGHLIGHT_COLOR
      else
        // All other points must fill with default color.
        fillColor := DEFAULT_GRAPH_COLOR;

      chWaveformLineSeries.SetColor(wavePos, fillColor);

      // Only half is available for the 1st cycle.
      if(wavePos > 1) then
      begin
        chWaveformLineSeries.SetColor(wavePos - 1, fillColor);
      end;

      inc(wavePos, 2);
    end;

    chWaveform.Invalidate;
  end;
end;

end.

