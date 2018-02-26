unit uBackupManager;

interface

uses
  uSettingsStructure;

const
  cDBBackUpScript = 'BACKUP DATABASE <DBNAME> TO DISK = ''<BACKUP_PATH>'' ' +
                    'WITH FORMAT, '+
                    'MEDIANAME = ''<DBNAME>'', ' +
                    'NAME = ''Full Backup of <DBNAME>'' ' + #10#13 + 'GO';
  cProgramPackScript = '7z.exe a -tzip -ssw -mx7  -sdel <ARCHIVE_FILENAME> <FILE_MASK>';

procedure BackupDatabase(const ADatabase: TDatabaseSettings; const ABackupPath: string);
procedure BackupProgram(const AProgram: TProgramSettings; const ABackupPath: string);

implementation

procedure BackupDatabase(const ADatabase: TDatabaseSettings; const ABackupPath: string);
begin

end;


procedure BackupProgram(const AProgram: TProgramSettings; const ABackupPath: string);
begin

end;

end.
