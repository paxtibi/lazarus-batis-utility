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
    FConnection: IZConnection;
    procedure SetConnection(AValue: IZConnection);
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
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Connection: IZConnection read FConnection write SetConnection;
  end;


implementation

{ TLZBatisMapper }

procedure TBaseMapper.SetConnection(AValue: IZConnection);
begin
  if FConnection = AValue then
  begin
    Exit;
  end;
  FConnection := AValue;
end;

function TBaseMapper.createPreparedStatement(sqlStatement: string): IZPreparedStatement;
begin
  Result := FConnection.PrepareStatement(sqlStatement);
end;

function TBaseMapper.createCallableStatement(sqlStatement: string): IZCallableStatement;
begin
  Result := FConnection.PrepareCall(sqlStatement);
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

constructor TBaseMapper.Create;
begin
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

end.
