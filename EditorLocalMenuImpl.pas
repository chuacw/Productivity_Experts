unit EditorLocalMenuImpl;
// References:
// "How do I add an item to te delphi editor menu?" [sic]
// http://groups-beta.google.com/group/borland.public.delphi.opentoolsapi/browse_frm/thread/da06b091817127d6/777569bdba83b09f#777569bdba83b09f
interface

procedure Register;

implementation
uses
  SysUtils, Classes, Forms, ActnList, StdActns, ToolsAPI;

function GetIDEBaseEditorActionList: TCustomActionList;
var
  MS: IOTAMessageServices;

  procedure FindActionList(const Prefix: string; AComponent: TComponent);
  var
    I: Integer;
    LComponent: TComponent;
  begin
    for I := 0 to AComponent.ComponentCount-1 do
      begin
        LComponent := AComponent.Components[I];
        MS.AddTitleMessage(Format('%sComponent: %s', [Prefix, LComponent.Name]));
        FindActionList(Prefix + '  ', LComponent);
      end;
  end;

var
  LForm: TForm;
  EditorModule: TDataModule;
  EditorComponent: TComponent;
  I: Integer;
begin
  MS := BorlandIDEServices as IOTAMessageServices;
  Result := nil;
  EditorModule := nil;
  for I := 0 to Screen.FormCount - 1 do
    begin
      LForm := Screen.Forms[I];
      MS.AddTitleMessage(Format('Form: "%s"', [LForm.Name]));
      FindActionList('  ', LForm);
    end;
  for I := 0 to Screen.DataModuleCount - 1 do
    begin
      MS.AddTitleMessage(Format('Data Module: "%s"', [Screen.DataModules[I].Name]));
      FindActionList('  ', Screen.DataModules[I]);
    end;
end;

procedure Register;
const
  sMenuRegionName = 'chuacw_ProductivityExperts_DefineRegion';
var
  EditorActions: TCustomActionList;
  MS: IOTAMessageServices;
begin
  MS := BorlandIDEServices as IOTAMessageServices;
  MS.AddTitleMessage('Calling GetIDEBaseEditorActionList');
  EditorActions := GetIDEBaseEditorActionList;
end;

end.

