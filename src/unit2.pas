unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label3Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label5MouseEnter(Sender: TObject);
    procedure Label5MouseLeave(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.Label3Click(Sender: TObject);
begin
  LCLIntf.OpenURL('https://elcoan.github.io/tempo/');
end;

procedure TForm2.Label5Click(Sender: TObject);
begin
  LCLIntf.OpenURL('mailto://hello.tempo.app@gmail.com');
end;

procedure TForm2.Label6Click(Sender: TObject);
begin
  LCLIntf.OpenURL('https://t.me/tempo_chat');
end;

procedure TForm2.Label7Click(Sender: TObject);
begin
  LCLIntf.OpenURL('https://t.me/lakoano');
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then begin
    Close;
  end;
end;

procedure TForm2.Label5MouseEnter(Sender: TObject);
begin
  TEdit(Sender).Font.Style := TEdit(Sender).Font.Style + [fsUnderline];
end;

procedure TForm2.Label5MouseLeave(Sender: TObject);
begin
  TEdit(Sender).Font.Style := TEdit(Sender).Font.Style - [fsUnderline];
end;

end.

