unit RegionExpertImpl;

interface
{$IF CompilerVersion>=16.0}
uses
  SysUtils, Classes, ToolsAPI;

type
  TRegionExpertBinding = class(TNotifierObject, IOTAKeyboardBinding)
  public
    procedure GotoRegion(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);

    // IOTAKeyBoardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;
{$IFEND}
implementation

{$IF CompilerVersion>=16.0}
uses
  Menus, OTAUtils, Windows, Forms, frmGotoRegionImpl, Controls, TextUtils,
  DelphiParserImpl;

type
  TModuleRegion = class
  protected
    FModule: IOTAModule;
    FRegion: TOTARegion;
  public
    constructor Create(const AModule: IOTAModule; const ARegion: TOTARegion);
    destructor Destroy; override;
    property Module: IOTAModule read FModule;
    property Region: TOTARegion read FRegion;
  end;

  TRegionList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

constructor TModuleRegion.Create(const AModule: IOTAModule; const ARegion: TOTARegion);
begin
  FModule := AModule;
  FRegion := ARegion;
end;

destructor TModuleRegion.Destroy;
begin
  FModule := nil;
  inherited;
end;


procedure TRegionList.Notify(Ptr: Pointer; Action: TListNotification);
var
  P: TModuleRegion;
begin
  if Action = lnDeleted then
    begin
      P := Ptr;
      P.Free;
    end;
end;


{$REGION 'TRegionExpert implementation'}
{ TRegionExpertBinding }

procedure TRegionExpertBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([Shortcut(Ord('O'), [ssCtrl]),
    Shortcut(Ord('X'), [ssCtrl])], GotoRegion, nil);
end;

function TRegionExpertBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TRegionExpertBinding.GetName: string;
begin
  Result := 'chuacw.Productivity Experts.RegionExpert';
end;

function TRegionExpertBinding.GetDisplayName: string;
begin
  Result := 'Chee Wee''s Region Expert';
end;

{$REGION 'GotoRegion implementation'}
procedure TRegionExpertBinding.GotoRegion(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var
  MS: IOTAMessageServices;
  MG: IOTAMessageGroup;

  procedure InternalGotoRegion(const AModuleRegion: TModuleRegion);
  var
    SourceEditor: IOTASourceEditor;
    EditView: IOTAEditView;
    ElideAction: IOTAElideActions;
    Module: IOTAModule;
    Region: TOTARegion;
  begin
    Module := AModuleRegion.Module;
    Region := AModuleRegion.Region;

    if Supports(Module.CurrentEditor, IOTASourceEditor, SourceEditor) then
      begin
        EditView := SourceEditor.EditViews[0];
        SourceEditor.Show;
        SourceEditor.SwitchToView(0);
        EditView.Position.Move(Region.Start.Line, 1);
        if Supports(EditView, IOTAElideActions, ElideAction) then
          begin
            if frmGotoRegion.ExpandRegion then
              ElideAction.UnElideNearestBlock;
            if frmGotoRegion.GoRegionTop then
              EditView.SetTopLeft(Region.Start.Line, 1);
          end;
      end;
  end;

var
  List: TRegionList;

  procedure CheckModule(Module: IOTAModule);
  const
    sRegion: array[rkRegion..rkGlobal] of string = (
     'Region', 'If', 'Namespace', 'Type', 'Method', 'Nested method', 'Global'
    );
  var
    I, J: Integer;
    ModuleRegions: IOTAModuleRegions;
    Regions: TOTARegions;
    Region: TOTARegion;
    SourceEditor: IOTASourceEditor;
    EditView: IOTAEditView;
    ElideAction: IOTAElideActions;
    P: Pointer;
    LModuleRegion: TModuleRegion;
    Parent, Child: Pointer;
    LPosition: IOTAEditPosition;
    MethodName, Text: string;
  begin
    if Module = nil then exit;

{$IFDEF DEBUG}
    MS.AddTitleMessage(Format('Module: %s',  [Module.FileName]), MG);
{$ENDIF}

    if Supports(Module, IOTAModuleRegions, ModuleRegions) then
       begin

          Parent := nil; Child := nil;
{$IFDEF DEBUG}
          MS.AddTitleMessage(Format('IOTAModuleRegions found for %s...',
            [Module.CurrentEditor.FileName]), MG);
          MS.AddToolMessage(Module.CurrentEditor.FileName, Module.CurrentEditor.FileName,
            '', 1, 1, Parent, Child, MG);
          Parent := Child;
{$ENDIF}

          Regions := ModuleRegions.GetRegions('ProductivityExpertImpl.pas');
          for I := Low(Regions) to High(Regions) do
            begin
              Region := Regions[I];

{$IFDEF DEBUG}
              MS.AddToolMessage(Module.CurrentEditor.FileName, Format('Region: "%s" Kind: %s',
                [Region.Name, sRegion[Region.RegionKind]]),
                '', Region.Start.Line, 1, Parent, Child, MG);
{$ENDIF}
              if Region.RegionKind in [rkRegion, rkMethod]  then
                begin

{$IFDEF DEBUG}
                  MS.AddTitleMessage(Format('Region Container: %s', [Module.CurrentEditor.FileName]), MG);
{$ENDIF}

                  if Supports(Module.CurrentEditor, IOTASourceEditor, SourceEditor) then
                    begin

{$IFDEF DEBUG}
                      MS.AddTitleMessage(Format('Source Editor supported: %s', [Module.CurrentEditor.FileName]), MG);
{$ENDIF}

                      EditView := SourceEditor.EditViews[0];

                      if Region.RegionKind = rkMethod then
                        begin
                          LPosition := EditView.Position;
                          LPosition.Save;
                          try
                            LPosition.Move(Region.Start.Line, 1);
                            Text := LPosition.Read(1024);
                            for J := 1 to 3 do
                              begin
                                if ParseDelphiMethodName(Text, MethodName) then Break;
                                LPosition.Move(Region.Start.Line-J, 1);
                                Text := LPosition.Read(1024);
                              end;
                            Region.Name := MethodName;
                            MS.AddToolMessage(Module.CurrentEditor.FileName,
                              Format('Method: %s', [Region.Name]),
                              '', Region.Start.Line, 1, Parent, Child, MG);
                          finally
                            LPosition.Restore;
                          end;
                        end;

                      if Supports(EditView, IOTAElideActions, ElideAction) then
                        begin
                          LModuleRegion := TModuleRegion.Create(Module, Region);
                          P := LModuleRegion;
                          List.Add(P);
                        end;
                    end;
                end;
            end;
       end;
  end;

var
  I: Integer;
  Project: IOTAProject;
  Module: IOTAModule;
  ModuleServices: IOTAModuleServices;
  LModuleRegion: TModuleRegion;
  LCursor: TCursor;
begin
  BindingResult := krHandled;
  List := TRegionList.Create;
  if not Assigned(frmGotoRegion) then
    begin
      frmGotoRegion := TfrmGotoRegion.Create(Application);
      frmGotoRegion.CaptionPrefix := GetDisplayName;
    end;
  LCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    MS := BorlandIDEServices as IOTAMessageServices;
    MG := MS.AddMessageGroup(GetDisplayName);
    MG.AutoScroll := True;
{$IFDEF DEBUG}
    MS.AddTitleMessage('Looking for regions...', MG);
{$ENDIF}

    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    for I := 0 to ModuleServices.ModuleCount-1 do
      CheckModule(ModuleServices.Modules[I]);
    for I := 0 to List.Count-1 do
      begin
        LModuleRegion := List[I];
        frmGotoRegion.AddRegion(Format('%s: %s',
          [ExtractFileName(LModuleRegion.Module.FileName),
            LModuleRegion.Region.Name]));
      end;
    if (List.Count>0) then
    begin
    if (frmGotoRegion.ShowModal = mrOk) then
      begin
        I := frmGotoRegion.Region;
        LModuleRegion := List.Items[I];
        InternalGotoRegion(LModuleRegion);
      end;
    end else
    begin
      MS.AddTitleMessage('No regions found.', MG);
    end;
    FreeAndNil(frmGotoRegion);
  finally
    Screen.Cursor := LCursor;
    List.Free;
  end;
end;
{$ENDREGION 'GotoRegion implementation'}
{$ENDREGION 'TRegionExpert implementation'}

{$IFEND}
end.
