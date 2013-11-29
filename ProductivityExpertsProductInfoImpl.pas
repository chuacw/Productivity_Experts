unit ProductivityExpertsProductInfoImpl;

interface

{$IF CompilerVersion>=17.0} // Delphi 2005 or later
procedure Register;
{$IFEND}

implementation
uses SysUtils, ToolsAPI;

{$IF CompilerVersion >= 17.0}
var
  iProductInfo: Integer;

procedure Register;
var
  LAboutBox: IOTAAboutBoxServices;
begin
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, LAboutBox) then
    begin
      iProductInfo :=
      LAboutBox.AddProductInfo('Chee Wee''s Productivity Experts',
        'Copyright (c) 1995-2010 Embarcadero Technologies, Inc.',

// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
        'Chee Wee''s Productivity Experts', // Installed Products List
        'Product Info description', 0, 0);
    end;
end;

/// <summary>
/// Remove the product info each time we're uninstalled.
/// Otherwise, there could be multiple copies of the Product Info.
/// </summary>
procedure RemoveProductInfo;
var
  LAboutBox: IOTAAboutBoxServices;
begin
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, LAboutBox) then
    LAboutBox.RemoveProductInfo(iProductInfo);
end;

procedure ShowSplash;
var
  LSplash: IOTASplashScreenServices;
begin
  if Supports(SplashScreenServices, IOTASplashScreenServices, LSplash) then
    begin
//      LSplash.StatusMessage('Chee Wee''s Productivity Experts');
//      Causes a status message to be displayed at the bottom of the Splash screen

      LSplash.AddPluginBitmap('Chee Wee''s Productivity Experts', 0);
    end;
end;

initialization
  ShowSplash;
finalization
  RemoveProductInfo;
{$IFEND}

end.
