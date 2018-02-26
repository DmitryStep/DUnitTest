unit uUpdateManager;

interface

uses
  uSettingsStructure, uShellAPI;

const
  cUnPackScript = '7z.exe x -o"<SERVERDIR>" -y -aoa "<ARCHIVE_PATH>" ';
  cCopyScript = 'xcopy "<SRC_FILEMASK>" "DEST_FILEMASK" /S /E /Y';
  cDBUpdateScript = '';

procedure UnpackFiles(const ASourceZip: string;
                      const ADestinationFolder: string);
procedure CopyFiles(const ASource: string; const ADestination: string);
procedure UpdateDatabase(const AProgram: TProgramSettings;
                         const ADBUpdater: TDBUpdaterSettings);


implementation


procedure UnpackFiles(const ASourceZip: string;
                      const ADestinationFolder: string);
begin

end;


procedure CopyFiles(const ASource: string; const ADestination: string);
begin

end;


procedure UpdateDatabase(const AProgram: TProgramSettings;
                         const ADBUpdater: TDBUpdaterSettings);
begin

end;

end.
