unit lzBatis.application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

type
  { TlzBatisGenerator }
  TlzBatisGenerator = class(TCustomApplication)
  private
  protected
    FPrepare: record
      active: boolean;
      connectionString: string;
      username: string;
      password: string;
    end;
    FReverse: record
      active: boolean;
      connectionString: string;
      username: string;
      password: string;
    end;
    FTargetLocation: string;
    FSkipPattern: string;
    FTables: TStringList;
    FContexts: TStringList;
    FOverride: boolean;
    FFileNameInput: string;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lzBatis.configuration,
  lzBatis.mappers;

{ lzBatisGenerator }


procedure TlzBatisGenerator.DoRun;
var
  configurationPreparator: TConfigurationPreparator;
  configurationReverse: TConfigurationReverse;
  mapperGenerator: TMapperGenerator;
begin
  if FFileNameInput <> '' then
  begin
    if (FPrepare.active = False) and (FReverse.active = False) then
    begin
      mapperGenerator := TMapperGenerator.Create(self);
      mapperGenerator.DoOverride := FOverride;
      mapperGenerator.FileNameInput := FFileNameInput;
      mapperGenerator.Contexts := FContexts;
      mapperGenerator.DoRun;
    end
    else
    if FPrepare.active = True then
    begin
      configurationPreparator := TConfigurationPreparator.Create;
      configurationPreparator.ConnectionString := FPrepare.connectionString;
      configurationPreparator.UserName := FPrepare.username;
      configurationPreparator.Password := FPrepare.password;
      configurationPreparator.TargetFile := FFileNameInput;
      try
        configurationPreparator.prepare;
      except
        on e: Exception do
        begin
          Writeln(e.Message);
        end;
      end;
      FreeAndNil(configurationPreparator);
    end
    else
    begin
      configurationReverse := TConfigurationReverse.Create;
      configurationReverse.ConnectionString := FReverse.connectionString;
      configurationReverse.UserName := FReverse.username;
      configurationReverse.Password := FReverse.password;
      configurationReverse.TargetFile := FFileNameInput;
      try
        configurationReverse.reverse;
      except
        on e: Exception do
        begin
          Writeln(e.Message);
        end;
      end;
      FreeAndNil(configurationReverse);
    end;
  end
  else
  begin
    Writeln('Missing required parameter: -configfile');
    Writeln(ExtractFileName(ParamStr(0)));
    Writeln(' -configfile file_name (required) Specifies the name of the configuration file.');
    Writeln(' -overwrite (optional) If specified, override target mapper file. Else preserve older file.');
    Writeln(' -contextids context1,context2,...(optional)');
    Writeln(' -prepare: Prepare the xml file configuration. Use -configfile parameter as target additional parmeter is required. "connectionurl" "username" "password" became mandatory if -prepare is provided as parameter.');
    Writeln(' -reverse: Reverse Database. Use -configfile parameter as target additional parmeter is required. "connectionurl" "username" "password" became mandatory if -reverse is provided as parameter.');
    //Writeln(' -tables table1, table2,... (optional)');
  end;
  Terminate;
end;

constructor TlzBatisGenerator.Create(TheOwner: TComponent);
var
  idx: integer = 1;
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FOverride := False;
  FTables := TStringList.Create;
  FContexts := TStringList.Create;
  FPrepare.active := False;
  FReverse.active := False;
  repeat
    if ParamStr(idx) = '-configfile' then
    begin
      FFileNameInput := ParamStr(idx + 1);
      Inc(idx);
      Continue;
    end;
    if ParamStr(idx) = '-override' then
    begin
      FOverride := True;
    end;
    if ParamStr(idx) = '-tables' then
    begin
      Inc(idx);
      FTables.CommaText := ParamStr(idx);
    end;

    if ParamStr(idx) = '-contextids' then
    begin
      Inc(idx);
      FContexts.CommaText := ParamStr(idx);
    end;
    if ParamStr(idx) = '-prepare' then
    begin
      Inc(idx);
      FPrepare.active := True;
      FPrepare.connectionString := ParamStr(idx);
      Inc(idx);
      FPrepare.username := ParamStr(idx);
      Inc(idx);
      FPrepare.password := ParamStr(idx);
    end;
    if ParamStr(idx) = '-reverse' then
    begin
      Inc(idx);
      FReverse.active := True;
      FReverse.connectionString := ParamStr(idx);
      Inc(idx);
      FReverse.username := ParamStr(idx);
      Inc(idx);
      FReverse.password := ParamStr(idx);
    end;
    Inc(idx);
  until idx > ParamCount;
end;

destructor TlzBatisGenerator.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FContexts);
  inherited Destroy;
end;

end.
