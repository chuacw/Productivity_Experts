// Productivity Experts by Chua Chee Wee, Singapore
//
// Chua Chee Wee,
// Singapore
// 1 May 2005.
//
// 1) Region Expert - marks a block of selected text as a region with a
//    keystroke in C# and Delphi.
// 2) Comment Expert - marks a block of selected text as comments with a
//    keystroke in C# and Delphi.
// 3) generates XML-styled comments for code declarations with a key
//    sequence in Delphi.
// 4) auto comments extension expert - automatically inserts // in between
//    blocks of lines in Delphi.
// 5) Goto Region Expert - iterates through all open files and presents a list
//    of regions, allowing you to select what region to go to, optionally expanding
//    the region, and displaying the selected region at the top of the editor.
//    Currently only supports Delphi as there's a bug in the OTAPI which doesn't
//    allow it to iterate C# regions. Activate using Ctrl O + Ctrl X ( NOT Ctrl O + X ).
// 6) Code Template Expert - supports C# and Delphi
//    Automatically expands any matched code templates. Code Template Names
//    may not differ by 1 letter, eg, tryc, trycf. If that happens, the first
//    match is always taken, ie, the name of a code template should not be the prefix
//    of another. Instead, use something like tryc1, trycf. To try in Delphi, try
//    typing arrayc. To try in C#, try typing tryc.
//
// Blog: http://chuacw.ath.cx/chuacw/category/78.aspx

unit AutoCodeTemplatesImpl;

interface

procedure Register;

implementation
uses
  Classes, Forms, Menus, Messages, ToolsAPI, IniFiles, CodeTemplatesImpl,
  SysUtils, OTAUtils, CodeTemplateInsightImpl, Controls;

type
  TIDENotifier = class(TNotifierObject, IOTAIDENotifier)
  protected
    SL: TStringList;
    FCodeTemplates: ICodeTemplates;
  public
    constructor Create;
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    destructor Destroy; override;
  end;

  TEditorNotifier = class(TNotifierObject, IOTANotifier, IOTAEditorNotifier,
      IOTAModuleNotifier)
  protected
    FEditorNotifierIndex,
      FModuleNotifierIndex: Integer;

    FModule: IOTAModule;
    FSourceEditor: IOTASourceEditor;
    FCodeTemplates: ICodeTemplates;
    SLI: Integer;
    FStringList: TStringList;
    procedure UpdateIndices;
  public
    constructor Create(AStringList: TStringList;
      AModule: IOTAModule; ASourceEditor: IOTASourceEditor;
      const ACodeTemplates: ICodeTemplates);
    destructor Destroy; override;
    procedure Modified;
    procedure ViewActivated(const AView: IOTAEditView);
    procedure ViewNotification(const AView: IOTAEditView;
      Operation: TOperation);

    // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);

    property CodeTemplates: ICodeTemplates read FCodeTemplates write FCodeTemplates;
    property EditorNotifierIndex: Integer read FEditorNotifierIndex write FEditorNotifierIndex;
    property ModuleNotifierIndex: Integer read FModuleNotifierIndex write FModuleNotifierIndex;
    property SourceEditor: IOTASourceEditor read FSourceEditor write FSourceEditor;
    property Module: IOTAModule read FModule write FModule;
    property StringList: TStringList read FStringList write FStringList;
  end;

var
  NotifierIndex: Integer;

procedure Register;
var
  LServices: IOTAServices;
  Notifier: IOTAIDENotifier;
begin
  LServices := BorlandIDEServices as IOTAServices;
  Notifier := TIDENotifier.Create;
  NotifierIndex := LServices.AddNotifier(Notifier);
end;

procedure RemoveNotifier;
var
  LServices: IOTAServices;
begin
  if NotifierIndex <> -1 then
    begin
      LServices := BorlandIDEServices as IOTAServices;
      LServices.RemoveNotifier(NotifierIndex);
    end;
end;

{ TIDENotifier }

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
end;

constructor TIDENotifier.Create;
var
  Services: IOTAServices;
  FileName: string;
  Path, CodeTemplateName: string;
begin
  inherited Create;
  SL := THashedStringList.Create;
  Services := BorlandIDEServices as IOTAServices;
  Path := IncludeTrailingPathDelimiter(Services.GetTemplateDirectory);
  FileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.dci');
  CodeTemplateName := Path + FileName;
  if not FileExists(CodeTemplateName) then
    begin
      Path := IncludeTrailingPathDelimiter(Services.GetBinDirectory);
      CodeTemplateName := Path + FileName;
    end;
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(CodeTemplateName);
  FCodeTemplates := TCodeTemplates.Create(CodeTemplateName);
end;

destructor TIDENotifier.Destroy;
var
  I: Integer;
  LEditorNotifier: TEditorNotifier;
begin
  for I := SL.Count - 1 downto 0 do
    begin
      LEditorNotifier := SL.Objects[I] as TEditorNotifier;
      LEditorNotifier.SourceEditor.RemoveNotifier(LEditorNotifier.EditorNotifierIndex);
    end;
  SL.Free;
  inherited;
end;

procedure TIDENotifier.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);

  procedure RemoveNotifier(ASourceEditor: IOTASourceEditor);
  var
    LEditorNotifier: TEditorNotifier;
    I: Integer;
  begin
    I := SL.IndexOf(ASourceEditor.FileName);
    if I <> -1 then
      begin
        LEditorNotifier := SL.Objects[I] as TEditorNotifier;
        LEditorNotifier.Module.RemoveNotifier(LEditorNotifier.ModuleNotifierIndex);
        LEditorNotifier.SourceEditor.RemoveNotifier(LEditorNotifier.EditorNotifierIndex);
        if SL.Count > 0 then
          TEditorNotifier(SL.Objects[0]).UpdateIndices;
      end;
  end;

  procedure AddNotifier(Module: IOTAModule; ASourceEditor: IOTASourceEditor);
  var
    I: Integer;
    ProjectGroup: IOTAProjectGroup;
  begin
    ProjectGroup := GetCurrentProjectGroup;
    if (not Assigned(ProjectGroup)) or
      (ProjectGroup.FileName = ASourceEditor.FileName) then exit;
    I := SL.IndexOf(ASourceEditor.FileName);
    if I = -1 then
      begin
// No need to keep track of instance to free, as it'll be freed automatically.
        TEditorNotifier.Create(SL, Module, ASourceEditor, FCodeTemplates);
      end;
  end;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Project: IOTAProject;
begin
  case NotifyCode of
    ofnFileOpened:
      begin
// When modules are opened
        ModuleServices := BorlandIDEServices as IOTAModuleServices;
        Module := ModuleServices.FindModule(FileName);
        if Supports(Module, IOTAProject, Project) then
          begin
            Editor := Project.CurrentEditor;
            if Supports(Editor, IOTASourceEditor, SourceEditor) then
              AddNotifier(Module, SourceEditor);
          end;
        for I := 0 to Module.ModuleFileCount - 1 do
          begin
            if Supports(Module.ModuleFileEditors[I], IOTASourceEditor, SourceEditor) and
              (SourceEditor.EditViewCount >= 1) then
              AddNotifier(Module, SourceEditor) else
// C# support
              if Supports(Module.ModuleFileEditors[I], IOTAEditor, Editor) then
              if Supports(Editor, IOTASourceEditor, SourceEditor) then
                AddNotifier(Module, SourceEditor);
          end;
      end;
    ofnFileClosing:
      begin
        ModuleServices := BorlandIDEServices as IOTAModuleServices;
        Module := ModuleServices.FindModule(FileName);
        if Supports(Module, IOTAProject, Project) then
          begin
            Editor := Project.CurrentEditor;
            if Supports(Editor, IOTASourceEditor, SourceEditor) then
              RemoveNotifier(SourceEditor);
          end;
        if Assigned(Module) then
          begin
            for I := 0 to Module.ModuleFileCount - 1 do
              begin
                if Supports(Module.ModuleFileEditors[I], IOTASourceEditor, SourceEditor) and
                  (SourceEditor.EditViewCount >= 1) then
                  RemoveNotifier(SourceEditor) else
// C# support
                  if Supports(Module.ModuleFileEditors[I], IOTAEditor, Editor) and
                  Supports(Editor, IOTASourceEditor, SourceEditor) then
                  RemoveNotifier(SourceEditor);
              end;
          end;
      end;
  end;
end;

{ TEditorNotifier }

function TEditorNotifier.CheckOverwrite: Boolean;
begin
  Result := not FileExists(FSourceEditor.FileName);
end;

constructor TEditorNotifier.Create(AStringList: TStringList;
  AModule: IOTAModule; ASourceEditor: IOTASourceEditor;
  const ACodeTemplates: ICodeTemplates);
begin
  inherited Create;
  FStringList := AStringList;

  AStringList.AddObject(ASourceEditor.FileName, Self);
  UpdateIndices;

  FSourceEditor := ASourceEditor;
  FModule := AModule;
  FEditorNotifierIndex := ASourceEditor.AddNotifier(Self);
  FModuleNotifierIndex := AModule.AddNotifier(Self);
  FCodeTemplates := ACodeTemplates;
end;

destructor TEditorNotifier.Destroy;
var
  I: Integer;
begin
  I := FStringList.IndexOf(SourceEditor.FileName);
  if I <> -1 then
    FStringList.Delete(I);
  FSourceEditor := nil;
  inherited;
end;

procedure TEditorNotifier.Modified;
var
  LView: IOTAEditView;
  I, Element, Column, LineFlag, PipeIndex: Integer;
  Identifier, Template, Template1, Template2: string;
  ep: IOTAEditPosition;
  CursorPos: TOTAEditPos;
  CodeTemplate: TCodeTemplate;
  SL: TStringList;
begin
  if (SourceEditor.EditViewCount = 0) or
    Assigned(CodeTemplateInsight) then
    exit;
  LView := SourceEditor.EditViews[0];
  CursorPos := LView.CursorPos;
  if CursorPos.Col > 1 then
    Dec(CursorPos.Col);
  LView.GetAttributeAtPos(CursorPos, False, Element, LineFlag);
  if Element in [atIdentifier, atReservedWord] then
    begin
      ep := LView.Position;
      Identifier := ep.RipText('_', rfBackward or rfIncludeAlphaChars or rfIncludeNumericChars);
      if FCodeTemplates.Exists(Identifier, I) then
        begin
          CodeTemplate := FCodeTemplates.Item[I];
          if LView.Buffer.EditOptions.IDString = CodeTemplate.IDString then
            begin
              CodeTemplateInsight := TCodeTemplateInsight.Create(ep, CodeTemplate,
                LView, Length(Identifier));

              CodeTemplateInsight.Show;
              {
              ep.MoveRelative(0, -Length(Identifier));
              ep.Delete(Length(Identifier));
              Template := CodeTemplate.Template;
              SL := TStringList.Create;
              try
                SL.Text := Template;
                if (SL.Count > 1) and (LView.Buffer.EditOptions.BufferOptions.AutoIndent) then
                  begin
                    Column := ep.Column;
                    Template2 := #13#10;
                    for I := 0 to SL.Count - 1 do
                      begin
                        ep.Move(ep.Row, Column);
                        Template1 := SL[I];
                        PipeIndex := Pos('|', Template1);
                        if PipeIndex > 0 then
                          begin
                            CursorPos.Line := ep.Row;
                            CursorPos.Col := PipeIndex + Column - 1;
                            Delete(Template1, PipeIndex, 1);
                          end;
                        if I = SL.Count - 1 then
                          Template2 := '';
                        Template1 := Format('%s%s', [Template1, Template2]);
                        ep.InsertText(Template1);
                      end;
                    ep.Move(CursorPos.Line, CursorPos.Col);
                  end else
                  begin
                    I := Pos('|', Template);
                    Template1 := Copy(Template, 1, I - 1);
                    Template2 := Copy(Template, I + 1, Length(Template));
                    ep.InsertText(Template1);
                    CursorPos := LView.CursorPos;
                    ep.InsertText(Template2);
                    LView.CursorPos := CursorPos;
                  end;
              finally
                SL.Free;
              end;
              LView.Paint;
              }
            end;
        end;
    end;
end;

procedure TEditorNotifier.ModuleRenamed(const NewName: string);
begin
  FStringList[SLI] := NewName;
end;

procedure TEditorNotifier.UpdateIndices;
var
  I: Integer;
  LEditorNotifier: TEditorNotifier;
begin
  for I := 0 to FStringList.Count - 1 do
    begin
      LEditorNotifier := TEditorNotifier(FStringList.Objects[I]);
      LEditorNotifier.SLI := I;
    end;
end;

procedure TEditorNotifier.ViewActivated(const AView: IOTAEditView);
begin
end;

procedure TEditorNotifier.ViewNotification(const AView: IOTAEditView;
  Operation: TOperation);
begin
end;

initialization
  NotifierIndex := -1;
finalization
  RemoveNotifier;
end.

