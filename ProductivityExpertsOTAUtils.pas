
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ProductivityExpertsOTAUtils;

interface
uses
  ToolsAPI;

type
  TOTADebugMsg = class

  end;

function FindModuleInterface(AInterface: TGUID): IUnknown;
function GetCurrentProjectGroup: IOTAProjectGroup;
function GetCurrentProject: IOTAProject;
function ProjectExists(AProject: IOTAProject): Boolean;

{$IF CompilerVersion >= 16.0} // Delphi 8 or later
type
  TPersonality = (Unknown, BCB, DelphiWin32);
function ProjectPersonality(AProject: IOTAProject): TPersonality;
function PersonalityToDisplayName(APersonality: TPersonality): string;
{$IFEND}

implementation
uses
  Windows, SysUtils;

{$IF CompilerVersion >= 16.0} // Delphi 8 or later
function ProjectPersonality(AProject: IOTAProject): TPersonality;
var
  LPersonality: string;
begin
  Result := Unknown;
  LPersonality := AProject.Personality;
  if LPersonality = sCBuilderPersonality then
    Result := BCB else
  if LPersonality = sDelphiPersonality then
    Result := DelphiWin32;
end;

function PersonalityToDisplayName(APersonality: TPersonality): string;
begin
  case APersonality of
    BCB: Result := 'C++';
    DelphiWin32: Result := 'Delphi Win32';
  end;
end;

{$IFEND}

function FindModuleInterface(AInterface: TGUID): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as IOTAModuleServices do
    for I := 0 to ModuleCount - 1 do
      if (Modules[I].QueryInterface(AInterface, Result) = S_OK) then
        Break;
end;

function GetCurrentProjectGroup: IOTAProjectGroup;
begin
  Result := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
end;

function GetCurrentProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GetCurrentProjectGroup;
  if Assigned(ProjectGroup) then
    Result := ProjectGroup.ActiveProject
  else
    Result := FindModuleInterface(IOTAProject) as IOTAProject;
end;

function ProjectExists(AProject: IOTAProject): Boolean;
var
  LProject: TInterfacedObject;
begin
  Result := False;
  if Supports(AProject, IUnknown, LProject) then
    Result := LProject.RefCount>1;
end;

end.
