unit lzbatis.lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZDbcIntfs;

type
  Timestamp = TDatetime;
  Time = TDatetime;
  Date = TDatetime;
  Long = int64;
  Small = int16;

  TBaseEntityState = (esLoading, esNew, esDirty, edSyncronized);
  IBaseEntity = interface;
  TBaseEntity = class;
  TBaseMapper = class;
  TBaseMapperClass = class of TBaseMapper;
  TBaseEntityClass = class of TBaseEntity;


  { IBaseEntity }

  IBaseEntity = interface
    ['{04C04938-E3E6-4F63-B3F3-BE43CD0A9A75}']
    function GetState: TBaseEntityState;
    procedure SetState(AValue: TBaseEntityState);
    property state: TBaseEntityState read GetState write SetState;
  end;

  IDatabaseSession = interface
    ['{42B60A6C-6116-4501-99AD-DD03655F4743}']
    function createPreparedStatement(const sql: string): IZPreparedStatement;
    function createCallableStatement(const sql: string): IZCallableStatement;
    function connection: IZConnection;
  end;

  { TBaseEntity }

  TBaseEntity = class(TInterfacedPersistent, IBaseEntity, IFPObserved, IFPObserver)
  private
    FEntityState: TBaseEntityState;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure SetState(AValue: TBaseEntityState);
    function GetState: TBaseEntityState;
  protected
    procedure FireChangeNotify;
    procedure NotifyChanged(aSender: TObject); virtual;
    procedure NotifyFree(aSender: TObject); virtual;
  public
    property state: TBaseEntityState read GetState write SetState;
    destructor Destroy; override;
  end;

  { TBaseMapper }

  TBaseMapper = class(TPersistent)
  private
    FSession: IDatabaseSession;
    procedure SetSession(AValue: IDatabaseSession);
  protected
    function createPreparedStatement(sqlStatement: string): IZPreparedStatement;
    function createCallableStatement(sqlStatement: string): IZCallableStatement;
    procedure InitStatements; virtual;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: string); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: int64); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: single); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: double); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: extended); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: boolean); overload;
    procedure setParameter(targetStatement: IZPreparedStatement; index: integer; Value: TDateTime); overload;
  public
    constructor Create(aSession: IDatabaseSession);
    destructor Destroy; override;
  published
    property Session: IDatabaseSession read FSession write SetSession;
  end;


procedure registerMapper(Name: string; mapper: TBaseMapper);
function getMapper(Name: string): TBaseMapper;

implementation


type
  { TMapperRepository }

  TMapperRepository = class
  protected
    FRepository: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMapper(Name: string; mapper: TBaseMapper);
    function getMapper(Name: string): TBaseMapper;
  end;

var
  singletonRegistry: TMapperRepository = nil;

procedure registerMapper(Name: string; mapper: TBaseMapper);
begin
  if singletonRegistry = nil then
  begin
    singletonRegistry := TMapperRepository.Create;
  end;
  singletonRegistry.RegisterMapper(Name, mapper);
end;

function getMapper(Name: string): TBaseMapper;
begin
  if singletonRegistry = nil then
  begin
    singletonRegistry := TMapperRepository.Create;
  end;
  Result := singletonRegistry.getMapper(Name);
end;


{ TMapperRepository }

constructor TMapperRepository.Create;
begin
  FRepository := TStringList.Create;
end;

destructor TMapperRepository.Destroy;
var
  idx: integer;
begin
  for idx := FRepository.Count - 1 downto 0 do
  begin
    FRepository.Delete(idx);
  end;
  FreeAndNil(FRepository);
  inherited Destroy;
end;

procedure TMapperRepository.RegisterMapper(Name: string; mapper: TBaseMapper);
begin
  FRepository.AddObject(Name, mapper);
end;

function TMapperRepository.getMapper(Name: string): TBaseMapper;
begin
  Result := FRepository.Objects[FRepository.IndexOf(Name)] as TBaseMapper;
end;

{ TLZBatisMapper }

procedure TBaseMapper.SetSession(AValue: IDatabaseSession);
begin
  if FSession = AValue then
  begin
    Exit;
  end;
  FSession := AValue;
end;

function TBaseMapper.createPreparedStatement(sqlStatement: string): IZPreparedStatement;
begin
  Result := FSession.createPreparedStatement(sqlStatement);
end;

function TBaseMapper.createCallableStatement(sqlStatement: string): IZCallableStatement;
begin
  Result := FSession.createCallableStatement(sqlStatement);
end;

procedure TBaseMapper.InitStatements;
begin

end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: string);
begin
  targetStatement.SetString(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: int64);
begin
  targetStatement.SetLong(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: single);
begin
  targetStatement.SetFloat(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: double);
begin
  targetStatement.SetDouble(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: extended);
begin
  targetStatement.SetBigDecimal(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: boolean);
begin
  targetStatement.SetBoolean(index, Value);
end;

procedure TBaseMapper.setParameter(targetStatement: IZPreparedStatement; index: integer; Value: TDateTime);
begin
  targetStatement.SetTimestamp(index, Value);
end;

constructor TBaseMapper.Create(aSession: IDatabaseSession);
begin
  FSession := aSession;
  InitStatements;
end;

destructor TBaseMapper.Destroy;
begin
  inherited Destroy;
end;

{ TBaseEntity }

procedure TBaseEntity.FireChangeNotify;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

procedure TBaseEntity.NotifyChanged(aSender: TObject);
begin
end;

procedure TBaseEntity.NotifyFree(aSender: TObject);
begin
end;

procedure TBaseEntity.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange:
    begin
      NotifyChanged(ASender);
    end;
    ooFree:
    begin
      NotifyFree(ASender);
    end;
  end;
end;

procedure TBaseEntity.SetState(AValue: TBaseEntityState);
begin
  if FEntityState = AValue then
  begin
    Exit;
  end;
  FEntityState := AValue;
end;

function TBaseEntity.GetState: TBaseEntityState;
begin
  Result := FEntityState;
end;

destructor TBaseEntity.Destroy;
begin
  FPONotifyObservers(Self, ooFree, nil);
  inherited Destroy;
end;

initialization

finalization
  FreeAndNil(singletonRegistry);
end.
