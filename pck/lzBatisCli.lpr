program lzBatisCli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  lzBatis.application,
  zdbc,
  LazLogger,
  ZDbcIntfs,
  lzbatis.lib,
  lzbatis.om.DB,
  lzbatis.om.pascal,
  lzbatis.om.config,
  lzbatiz.writers;

var
  Application: TlzBatisGenerator;

{$R *.res}

begin
  DebugLnEnter('Main');
  Application := TlzBatisGenerator.Create(nil);
  Application.Title := 'lzBatis Generator';
  Application.Run;
  Application.Free;
  DebugLnExit('Main');
end.
