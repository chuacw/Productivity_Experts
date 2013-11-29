unit frmGotoRegionImpl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmGotoRegion = class(TForm)
    ListBox1: TListBox;
    cbGoRegionTop: TCheckBox;
    cbExpandRegion: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRegion: Integer;
    function GetExpandRegion: Boolean;
    function GetGoRegionTop: Boolean;
    procedure SetCaptionPrefix(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    procedure AddRegion(const ARegion: string);
    property Region: Integer read FRegion;
    property ExpandRegion: Boolean read GetExpandRegion;
    property GoRegionTop: Boolean read GetGoRegionTop;
    property CaptionPrefix: string write SetCaptionPrefix;
  end;

var
  frmGotoRegion: TfrmGotoRegion;

implementation

{$R *.dfm}

function TfrmGotoRegion.GetGoRegionTop: Boolean;
begin
  Result := cbGoRegionTop.Checked;
end;

function TfrmGotoRegion.GetExpandRegion: Boolean;
begin
  Result := cbExpandRegion.Checked;
end;

procedure TfrmGotoRegion.FormCreate(Sender: TObject);
begin
  FRegion := -1;
end;

procedure TfrmGotoRegion.AddRegion(const ARegion: string);
begin
  ListBox1.Items.Add(ARegion);
end;

procedure TfrmGotoRegion.ListBox1Click(Sender: TObject);
begin
  FRegion := ListBox1.ItemIndex;
end;

procedure TfrmGotoRegion.ListBox1DblClick(Sender: TObject);
begin
  FRegion := ListBox1.ItemIndex;
  ModalResult := mrOk;
{$IF CompilerVersion>=16.0}
  CloseModal;
{$ELSE}
  Close;
{$IFEND}
end;

procedure TfrmGotoRegion.SetCaptionPrefix(const Value: string);
begin
  Caption := Format('%s - %s', [Value, 'Select region to go to...']);
end;

end.
