unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, ComCtrls, Buttons, ExtCtrls, Menus, UniqueInstance, DateUtils,
  Registry
  {$IFDEF Windows}, Windows{$ENDIF}
  {$IFDEF Darwin}, MacOSAll{$ENDIF}
  {$IFDEF Darwin}, CocoaAll{$ENDIF}
  {$IFDEF Linux}, BaseUnix{$ENDIF};
type

  TTaskRecord = class
    tag: Integer;
    Date: String;
    TStart: String;
    TEnd: String;
    Task: String;
    DurStr: String;
    DurInt: Integer;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Edit2: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButtonImages: TImageList;
    ListView1: TListView;
    Panel1: TPanel;
    SpeedButton2: TSpeedButton;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure Edit2Change(Sender: TObject);
    procedure Edit2Enter(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1DrawItem(Sender: TCustomListView; AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListView1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
  private
    SDateTime: TDateTime;
    StopFilter: Boolean;
    data: TList;
    Tick: Integer;
    FilteredIndexes: array of Integer;
    procedure ApplyFilter(const FilterText: string);
    procedure DoStart();
    procedure DoFinish();
    procedure SaveToFile();
    procedure UpdateActiveTask();
    function GetActiveTask(): TTaskRecord;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  Unit2;

const
  CSV_HEADER = 'Date,Start,End,Task,Duration';
  DELIMITER = ',';
  QUOTE = '"';

function EscapeCSVField(const Field: string): string;
begin
  if (Pos(DELIMITER, Field) > 0) or (Pos(QUOTE, Field) > 0) or (Pos(#13#10, Field) > 0) then begin
    Result := QUOTE + StringReplace(Field, QUOTE, QUOTE + QUOTE, [rfReplaceAll]) + QUOTE;
  end else begin
    Result := Field;
  end;
end;

function ParseCSVLine(const Line: string): TStringArray;
var
  i: Integer;
  InQuotes: Boolean;
  Fields: TStringArray;
  CurrentField: string;
begin
  InQuotes := False;
  CurrentField := '';
  SetLength(Fields, 0);

  i := 0;
  while i <= Length(Line) do begin
    Inc(i);
    if Line[i] = QUOTE then begin
      if InQuotes and (i < Length(Line)) and (Line[i+1] = QUOTE) then begin
        // This is an escaped quote.
        CurrentField := CurrentField + QUOTE;
        Inc(i); // Skip the next quote
      end else begin
        // This is the beginning or end of the escaped field.
        InQuotes := not InQuotes;
      end;
    end else if (Line[i] = DELIMITER) and not InQuotes then begin
      // End of the field
      SetLength(Fields, Length(Fields) + 1);
      Fields[High(Fields)] := CurrentField;
      CurrentField := '';
    end else begin
      // Regular symbol
      CurrentField := CurrentField + Line[i];
    end;
  end;

  // Adding the last field
  SetLength(Fields, Length(Fields) + 1);
  Fields[High(Fields)] := CurrentField;

  Result := Fields;
end;

function DurationToStr(mm: Integer): String;
var
  Hours, Minutes: Integer;
begin
  Hours   := mm div 60;
  Minutes := mm mod 60;

  if Hours > 0 then
    Result := Format('%.2dh %.2dm', [Hours, Minutes])
  else if Minutes > 9 then
    Result := Format('%.2dm', [Minutes])
  else
    Result := IntToStr(Minutes) + 'm';
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ApplyFilter(const FilterText: string);
var
  i: Integer;
  LowerFilter, s: string;
begin
  LowerFilter := AnsiLowerCase(Trim(FilterText));
  SetLength(FilteredIndexes, 0);

  if LowerFilter = '' then begin
    // No filter - show all elements
    SetLength(FilteredIndexes, data.Count);
    for i := 0 to data.Count-1 do
      FilteredIndexes[i] := i;
  end else begin
    // Filtration
    for i := 0 to data.Count-1 do begin
      if TTaskRecord(data[i]).tag = 0 then Continue;
      s := AnsiLowerCase(TTaskRecord(data[i]).Task);
      if (Pos(LowerFilter, s) > 0) then begin
        SetLength(FilteredIndexes, Length(FilteredIndexes) + 1);
        FilteredIndexes[High(FilteredIndexes)] := i;
      end;
    end;
  end;

  ListView1.Items.Count := Length(FilteredIndexes);
  if not Assigned(ListView1.Selected) then begin
    ListView1.Selected := ListView1.Items[0];
  end;
  ListView1.Invalidate;
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
var
  Index: Integer;
begin
  if (Item.Index >= 0) and (Item.Index <= High(FilteredIndexes)) then begin
    Index := FilteredIndexes[Item.Index];
    Item.Caption := TTaskRecord(data[Index]).Task;
  end else begin
    Item.Caption := 'error';
  end;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
var
  El: TTaskRecord;
begin
  if ListView1.Selected = nil then Exit;
  if not Edit2.Enabled then Exit;
  El := TTaskRecord(data[FilteredIndexes[ListView1.Selected.Index]]);
  if El.tag = 0 then Exit;
  StopFilter := True;
  Edit2.Text := El.Task;
  Edit2.SetFocus;
  Application.ProcessMessages;
  Edit2.SelStart := Length(Edit2.Text);
  Edit2.ClearSelection;
  StopFilter := False;
end;

procedure TForm1.ListView1DrawItem(Sender: TCustomListView; AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
var
  c: TCanvas;
  i, w: Integer;
  El: TTaskRecord;
  R: TRect;
  ts: TTextStyle;
begin
  if (AItem.Index < 0) or (AItem.Index > High(FilteredIndexes)) then Exit;

  c := ListView1.Canvas;

  // If the element is selected, paint the background
  if (odSelected in AState) then begin
    c.Brush.Color := clHighlight;
    c.Font.Color := clHighlightText;
  end else begin
    c.Brush.Color := clWindow;
    c.Font.Color := clWindowText;
  end;

  El := TTaskRecord(data[FilteredIndexes[AItem.Index]]);

  if El.tag = 0 then begin
    c.Font.Style := [fsBold]
  end;

  c.FillRect(ARect);

  R := ARect;
  w := ARect.Right;
  R.Right := R.Right - c.TextWidth('00h 00m') - 4 - 12;

  if El.tag = 0 then
    c.TextOut(ARect.Left + 4, ARect.Top + 2, El.Task)
  else begin
    ts := c.TextStyle;
    ts.EndEllipsis := True;
    ts.SingleLine  := True;
    c.TextRect(R, R.Left + 12, ARect.Top + 2, El.Task, ts);

    R.Left  := R.Right + 12;
    R.Right := w - 4;
    ts.Alignment := taRightJustify;
    c.TextRect(R, R.Left + 4, ARect.Top + 2, El.DurStr, ts);
  end;
end;

procedure TForm1.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not Edit2.Enabled then Exit;
  if (Key = VK_RETURN) and (ssCtrl in Shift) then Exit;
  if Key = VK_UP then begin
    if (ListView1.Selected <> nil) and (ListView1.Selected.Index = 0) then begin
      Key := 0;
      Edit2.SetFocus;
    end;
  end;
  if Key = 13 then begin
    ListView1DblClick(Sender);
  end;
end;

procedure TForm1.ListView1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if not Edit2.Enabled then Exit;
  if (UTF8Key = #$0D) or (UTF8Key = #$0A) then Exit;
  Edit2.SetFocus;
  Edit2.Text     := Edit2.Caption + UTF8Key;
  Edit2.SelStart := Length(Edit2.Text);
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  SaveToFile();
  Application.Terminate;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
 TrayIcon1DblClick(Sender);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  LCLIntf.OpenURL('https://elcoan.github.io/tempo/');
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
  if Edit2.Focused then begin
    with Panel1.Canvas do begin
      Pen.Color := clHighlight;
      Rectangle(0, 0, Panel1.ClientWidth, Panel1.ClientHeight);
    end;
  end else begin
    with Panel1.Canvas do begin
      Pen.Color := clWindowFrame;
      Rectangle(0, 0, Panel1.ClientWidth, Panel1.ClientHeight);
    end;
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if Edit2.Enabled and (Edit2.Text = '') then Exit;

  if SpeedButton2.ImageIndex = 0 then begin
    SpeedButton2.ImageIndex := 1;
    Edit2.Enabled := False;
    Panel1.Color := TColorRef($F0F0F0);
    DoStart();
    Tick := 0;
    Timer1.Enabled := True;
    GetActiveTask();
    ApplyFilter('');
  end else begin
    Timer1.Enabled := False;
    StopFilter := True;
    SpeedButton2.ImageIndex := 0;
    Edit2.Enabled := True;
    Panel1.Color := clWhite;
    DoFinish();
    Edit2.SetFocus;
    StopFilter := False;
  end;
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  if Form1.Visible then begin
    Hide;
  end else begin
    Show;
    WindowState := wsNormal;
    BringToFront;
  end;
end;

procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
begin
  Show;
  WindowState := wsNormal;
  BringToFront;
end;

function FormatDateSpecial(y, m, d: string): string;
var
  DayName, MonthName: string;
  ShortMonthNames: array[1..12] of String = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  ShortDayNames: array[1..7] of String = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
begin
  DayName := ShortDayNames[DayOfWeek(EncodeDate(StrToInt(y), StrToInt(m), StrToInt(d)))];

  MonthName := ShortMonthNames[StrToInt(m)];

  Result := Format('%d %s (%s)', [
    StrToInt(d),
    MonthName,
    DayName
  ]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  r, D: TTaskRecord;
  tt, curDate: String;
  CSVData: TStringList;
  Fields: TStringArray;
begin
  data := TList.Create;
  curDate := '';
  D := nil;
  //
  if FileExists('tasks.csv') then begin
    CSVData := TStringList.Create;
    CSVData.LoadFromFile('tasks.csv');
    CSVData.Delete(0);
    for i := 0 to CSVData.Count - 1 do begin
      Fields := ParseCSVLine(CSVData[i]);
      if Length(Fields) = 5 then begin
        if (curDate <> Fields[0]) then begin
          // Group of Date
          curDate := Fields[0];
          D := TTaskRecord.Create;
          tt := FormatDateSpecial(Copy(curDate, 1, 4), Copy(curDate, 6, 2), Copy(curDate, 9, 2));
          D.Task := tt;
          D.Date := curDate;
          D.tag  := 0;
          data.Insert(0, D);
        end;
        // Test record
        r := TTaskRecord.Create;
        r.tag := 1;
        // Date
        tt := Fields[0];
        r.Date := tt;
        // Start time
        tt := Fields[1];
        r.TStart := tt;
        // End time
        tt := Fields[2];
        r.TEnd := tt;
        // Task text
        r.Task := Fields[3];
        // Duration
        tt := TrimRight(Fields[4]);
        r.DurInt := StrToInt(tt);
        r.DurStr := DurationToStr(r.DurInt);

        data.Insert(1, r);
      end;
    end;
    CSVData.Free;
  end;
  ListView1.Items.Count := data.Count;
  ApplyFilter('');
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then begin
    try
      SpeedButton2Click(Sender);
    except
    end;
  end;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  SpeedButton2.Enabled := (Edit2.Text <> '');
  if StopFilter then Exit;
  ApplyFilter(Edit2.Text);
end;

procedure TForm1.Edit2Enter(Sender: TObject);
begin
  Panel1.Repaint;
end;

procedure TForm1.Edit2Exit(Sender: TObject);
begin
  Panel1.Repaint;
end;

procedure TForm1.Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0;
    Edit2.ClearSelection;
    Exit;
  end;
  if Key = VK_ESCAPE then begin
    Edit2.Text := '';
    ApplyFilter('');
    Exit;
  end;
  if Key = VK_DOWN then begin
    ListView1.SetFocus;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
  Hide;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ListView1.Column[0].Width := ListView1.ClientWidth;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then begin
    Hide;
  end;
end;

procedure TForm1.DoStart();
begin
  SDateTime := Now;
end;

procedure TForm1.DoFinish();
begin
  UpdateActiveTask();
  SaveToFile();
end;

procedure TForm1.UpdateActiveTask();
var
  El: TTaskRecord;
  SDate, NDate: String;
  NDateTime, EDateTime: TDateTime;
begin
  if Edit2.Enabled then Exit;

  // Let's check if there was a transition to a new day
  NDateTime := Now;
  SDate := FormatDateTime('yyyy-mm-dd', SDateTime);
  NDate := FormatDateTime('yyyy-mm-dd', NDateTime);

  // There was no transition to a new day
  if NDate = SDate then begin

    El := GetActiveTask();
    El.DurInt := El.DurInt + 1;
    El.DurStr := DurationToStr(El.DurInt);
    El.TEnd   := FormatDateTime('HH:MM', NDateTime);

  end else begin

    // Transition to a new day
    EDateTime := EndOfTheDay(SDateTime);

    // Let's close the "old" task
    El := GetActiveTask();
    El.DurInt := El.DurInt + 1; // MinutesBetween(EDateTime, SDateTime);
    El.DurStr := DurationToStr(El.DurInt);
    El.TEnd   := FormatDateTime('HH:MM', EDateTime);

    // Let's create a new task for today
    SDateTime := NDateTime;

    El := GetActiveTask();
    //El.DurInt := El.DurInt;
    El.DurStr := DurationToStr(El.DurInt);
    El.TEnd   := FormatDateTime('HH:MM', NDateTime);

  end;

  ApplyFilter('');
end;

function TForm1.GetActiveTask(): TTaskRecord;
var
  SDate: String;
  dayAdded: Boolean;
  El, D: TTaskRecord;
begin
  if Edit2.Enabled then begin Result := nil; Exit; end;

  SDate := FormatDateTime('yyyy-mm-dd', SDateTime);

  // Let's check if there is a grouping on the start date
  D := nil;
  if data.Count > 0 then begin
    El := TTaskRecord(data[0]);
    if (El.tag = 0) and (El.Date = SDate) then begin
      D := El;
    end;
  end;

  // Добавляем группировку на дату начала
  dayAdded := False;
  if D = nil then begin
    D := TTaskRecord.Create;
    D.tag  := 0;
    D.Date := SDate;
    D.Task := FormatDateSpecial(Copy(SDate, 1, 4), Copy(SDate, 6, 2), Copy(SDate, 9, 2));
    data.Insert(0, D);
    dayAdded := True;
  end;

  // If there is already a grouping on the start date, let's check what the last task is in it
  if not dayAdded then begin
    El := TTaskRecord(data[1]);
    if (El.tag = 1) and (El.Task = Edit2.Text) then begin
      Result := El;
      Exit
    end;
  end;

  // Let's add a task
  El := TTaskRecord.Create;
  El.Date   := SDate;
  El.TStart := FormatDateTime('HH:MM', SDateTime);
  El.tag    := 1;
  El.Task   := Edit2.Text;
  El.DurInt := 0;
  El.DurStr := DurationToStr(El.DurInt);
  data.Insert(1, El);

  Result := El;
end;

procedure TForm1.SaveToFile();
var
  i: Integer;
  Line: string;
  El: TTaskRecord;
  CSVFile: TStringList;
begin
  CSVFile := TStringList.Create;
  try
    CSVFile.Add(CSV_HEADER);

    for i := data.Count-1 downto 0 do begin
      El := TTaskRecord(data[i]);
      if El.tag = 0 then Continue;
      Line := EscapeCSVField(El.Date) + DELIMITER +
              EscapeCSVField(El.TStart) + DELIMITER +
              EscapeCSVField(El.TEnd) + DELIMITER +
              EscapeCSVField(El.Task) + DELIMITER +
              EscapeCSVField(IntToStr(El.DurInt));
      CSVFile.Add(Line);
    end;

    // Save to file
    CSVFile.SaveToFile('tasks.tmp');
    DeleteFile('tasks.csv');
    RenameFile('tasks.tmp', 'tasks.csv');
    DeleteFile('tasks.tmp');
  finally
    CSVFile.Free;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateActiveTask();

  Inc(Tick);

  if Tick > 15 then begin
    Tick := 0;
    SaveToFile();
  end;
end;


end.

