unit lzbatis.writers;

{$mode objfpc}{$H+}

interface

uses
  paxtibi.utils,
  Classes, SysUtils, lzBatis.om.config, lzBatis.om.pascal;

type

  { TUnitWriter }

  TUnitWriter = class
  private
    FCurrentContext: TConfigurationContext;
    procedure SetCurrentContext(AValue: TConfigurationContext);
  protected
    FTarget: TPrintStream;
  protected
    procedure generateClass(entity: TOMClass); overload;
    procedure generateClass(entity: TOMMapper); overload;
    procedure generateClasses(entities: TOMClasses); overload;
    procedure generateClasses(entities: TOMMappers); overload;
    procedure generateInitStatementMethod(entity: TOMMapper); overload;
    procedure generateMapperMethods(cu: TOMCompilationUnit; entity: TOMMapper); overload;
    procedure generateMapperMethods(cu: TOMCompilationUnit); overload;
    procedure generateForWard(Classes: TOMClasses); overload;
    procedure generateForWard(compilationUnit: TOMCompilationUnit); overload;
    procedure generateInterface(intf: TOMInterface); overload;
    procedure generateInterfaces(interfaces: TOMInterfaces); overload;
    procedure generateSetterOrGetterMethod(entities: TOMClasses); overload;
    procedure generateSetterOrGetterMethod(entity: TOMClass; method: TOMMethod); overload;
  public
    procedure generate(compilationUnit: TOMCompilationUnit);
    property CurrentContext: TConfigurationContext read FCurrentContext write SetCurrentContext;
  end;

implementation

uses
  LazLogger, RegExpr;


{ TUnitWriter }

procedure TUnitWriter.generateSetterOrGetterMethod(entity: TOMClass; method: TOMMethod);
var
  Parameter: TOMParameter;
begin
  FTarget.println();
  if method.ReturnType = nil then
  begin
    FTarget.print('procedure');
  end
  else
  begin
    FTarget.print('function');
  end;
  FTarget.print(' ').print(entity.Name).print('.').print(method.Name).print('(');
  for Parameter in method.Parameters do
  begin
    FTarget.print(Parameter.Name).print(':').print(Parameter.ParameterType.Name);
    if (method.Parameters.IndexOf(Parameter)) < (method.Parameters.Count - 1) then
    begin
      FTarget.print(';');
    end;
  end;
  FTarget.print(')');
  if method.ReturnType <> nil then
  begin
    FTarget.print(':').print(method.ReturnType.Name);
  end;
  FTarget.print(';');
  FTarget.println();
  FTarget.print('Begin').println();
  if method.ReturnType = nil then
  begin
    FTarget.print('  if (').print(method.SetterOf.Name).print('<> aValue) then').println;
    FTarget.print('    ').print(method.SetterOf.Name).print(':= aValue;').println;
  end
  else
  begin
    FTarget.print('  Result := ').print(method.SetterOf.Name).print(';').println();
  end;
  FTarget.print('End;').println();
end;

procedure TUnitWriter.generateSetterOrGetterMethod(entities: TOMClasses);
var
  entity: TOMClass;
  method: TOMMethod;
begin
  FTarget.println().println();
  for entity in entities do
  begin
    for method in entity.Methods do
    begin
      generateSetterOrGetterMethod(entity, method);
    end;
  end;
  FTarget.println();
end;

procedure TUnitWriter.generateForWard(compilationUnit: TOMCompilationUnit);
var
  intf: TOMInterface;
  mapper: TOMMapper;
begin
  if (compilationUnit.Mappers.Count > 0) and (compilationUnit.Interfaces.Count > 0) then
  begin
    FTarget.print('type').println();
  end;

  if compilationUnit.Interfaces.Count > 0 then
  begin
    for intf in compilationUnit.Interfaces do
    begin
      FTarget.print('  ').print(intf.Name).print(' = Interface;').println();
    end;
  end;
  if compilationUnit.Mappers.Count > 0 then
  begin
    for mapper in compilationUnit.Mappers do
    begin
      FTarget.print('  ').print(mapper.Name).print(' = Class;').println();
    end;
  end;
end;

procedure TUnitWriter.generateForWard(Classes: TOMClasses);
var
  entity: TOMClass;
begin
  if (Classes.Count > 0) then
  begin
    FTarget.print('type').println();
  end;

  if Classes.Count > 0 then
  begin
    for entity in Classes do
    begin
      FTarget.print('  ').print(entity.Name).print(' = class;').println();
    end;
  end;
end;

procedure TUnitWriter.generateInterface(intf: TOMInterface);
var
  p: TOMParameter;
  m: TOMMethod;
  _property: TOMProperty;
begin
  FTarget.print('  ').print(intf.Name).print(' = interface(IBaseEntity)').println();
  FTarget.print('    [''').print(intf.GUID).print(''']').println();
  for m in intf.Methods do
  begin
    if (m.ReturnType <> nil) then
    begin
      FTarget.print('    function ').print(m.Name).print(':').print(m.ReturnType.Name).print(';').println();
    end
    else
    begin
      FTarget.print('    procedure ').print(m.Name).print('(');
      for p in m.Parameters do
      begin
        FTarget.print(p.Name).print(':').print(p.ParameterType.Name);
        if (m.Parameters.IndexOf(p)) < (m.Parameters.Count - 1) then
        begin
          FTarget.print(',');
        end;
      end;
      FTarget.print(');').println();
    end;
  end;
  for _property in intf.Properties do
  begin
    FTarget.print('    property ').print(_property.Name).print(':').print(_property.Getter.ReturnType.Name).print(' read ').print(_property.Getter.Name);
    FTarget.print(' write ').print(_property.Setter.Name).print(';').println();
  end;
  FTarget.print('  End;').println();
end;

function sortField(const f1, f2: TOMField): integer;
begin
  Result := CompareText(f1.Name, f2.Name);
  if Result = 0 then
  begin
    Result := Ord(f1.Visibility) - Ord(f2.Visibility);
  end;
end;


function sortMethod(const f1, f2: TOMMethod): integer;
begin
  Result := CompareText(f1.Name, f2.Name);
  if Result = 0 then
  begin
    Result := Ord(f1.Visibility) - Ord(f2.Visibility);
  end;
end;

procedure TUnitWriter.SetCurrentContext(AValue: TConfigurationContext);
begin
  if FCurrentContext = AValue then
  begin
    Exit;
  end;
  FCurrentContext := AValue;
end;

procedure TUnitWriter.generateClass(entity: TOMClass);
var
  p: TOMParameter;
  f: TOMField;
  m: TOMMethod;
  intf: TOMInterface;
  lastVisibility: TVisibleLevel;
begin
  FTarget.print('  ').print(entity.Name).print(' = class(TBaseEntity');
  for intf in entity.Impls do
  begin
    FTarget.print(',').print(intf.Name);
  end;
  FTarget.print(')').println();
  entity.Fields.Sort(@sortField);
  entity.Methods.Sort(@sortMethod);
  lastVisibility := vlUnknow;
  for f in entity.Fields do
  begin
    if lastVisibility <> f.Visibility then
    begin
      FTarget.print('  ').print(f.getVisibilityName).println;
      lastVisibility := f.Visibility;
    end;
    FTarget.print('    ').print(f.Name).print(':').print(f.ReferencedType.Name).print(';').println();
  end;
  lastVisibility := vlUnknow;
  for m in entity.Methods do
  begin
    if lastVisibility <> m.Visibility then
    begin
      FTarget.print('  ').print(m.getVisibilityName).println;
      lastVisibility := m.Visibility;
    end;
    FTarget.print('    ');
    if (m.ReturnType <> nil) then
    begin
      FTarget.print('function ').print(m.Name).print(':').print(m.ReturnType.Name).print(';').println();
    end
    else
    begin
      FTarget.print('procedure ').print(m.Name).print('(');
      for p in m.Parameters do
      begin
        FTarget.print(p.Name).print(':').print(p.ParameterType.Name);
        if (m.Parameters.IndexOf(p)) < (m.Parameters.Count - 1) then
        begin
          FTarget.print(';');
        end;
      end;
      FTarget.print(');').println();
    end;
  end;
  FTarget.print('  End;').println();
end;

procedure TUnitWriter.generateInterfaces(interfaces: TOMInterfaces);
var
  intf: TOMInterface;
begin
  for intf in interfaces do
  begin
    FTarget.println();
    generateInterface(intf);
  end;
end;

procedure TUnitWriter.generateClasses(entities: TOMClasses);
var
  entity: TOMClass;
begin
  for entity in entities do
  begin
    FTarget.println();
    generateClass(entity);
  end;
end;

procedure TUnitWriter.generateClasses(entities: TOMMappers);
var
  cursor: TOMMapper;
begin
  for cursor in entities do
  begin
    generateClass(cursor);
  end;
  FTarget.flush();
end;

procedure TUnitWriter.generateInitStatementMethod(entity: TOMMapper);
var
  field: TOMField;
  query: string;
begin
  FTarget.print('Procedure ').print(entity.Name).print('.').print('InitStatements').print(';').println();
  FTarget.print('Begin').println();
  for field in entity.Fields do
  begin
    if field.InitializiationValue <> '' then
    begin
      query := ReplaceRegExpr('\$\{(\S*)\}', field.InitializiationValue, '?', True);
      FTarget.print('  ').print(field.Name).print(' := createPreparedStatement(').print(QuotedStr(query)).print(');').println();
    end;
  end;
  FTarget.print('End;').println();
end;


procedure TUnitWriter.generateMapperMethods(cu: TOMCompilationUnit; entity: TOMMapper);
var
  method: TOMMethod;
  Parameter: TOMParameter;
  _property: TOMProperty;
  intf: TOMInterface;
  re: TRegExpr;
  idx: integer = 1;
begin
  re := TRegExpr.Create('\$\{(\S*)\}');
  for method in entity.Methods do
  begin
    FTarget.println();
    if (method.ReturnType = nil) and (method.ResultName = '') then
    begin
      FTarget.print('procedure');
    end
    else
    begin
      FTarget.print('function');
    end;
    FTarget.print(' ').print(entity.Name).print('.').print(method.Name).print('(');
    for Parameter in method.Parameters do
    begin
      FTarget.print(Parameter.Name).print(':').print(Parameter.ParameterTypeName);
      if (method.Parameters.IndexOf(Parameter)) < (method.Parameters.Count - 1) then
      begin
        FTarget.print(';');
      end;
    end;
    FTarget.print(')');
    if method.ReturnType <> nil then
    begin
      FTarget.print(':').print(method.ReturnType.Name);
    end
    else
    if method.ResultName <> '' then
    begin
      FTarget.print(':').print(method.ResultName);
    end;
    FTarget.print(';');
    FTarget.println();
    FTarget.print('var').println();
    FTarget.print('  rs : IZResultSet;').println();
    FTarget.print('Begin').println();
    re.InputString := method.SetterOf.InitializiationValue;
    re.Compile;
    idx := 1;
    if re.Exec(1) then
    begin
      repeat
        FTarget.print('  setParameter(').print(method.SetterOf.Name).print(',').print(idx).print(',').print(re.Match[1]).print(');').println;
        Inc(idx);
      until re.ExecNext = False;
    end;
    FTarget.print('  rs := ').print(method.SetterOf.Name).print('.ExecuteQueryPrepared;').println();
    if (method.ReturnType = nil) and (method.ResultName <> '') then
    begin
      method.ReturnType := cu.getInterfaceByName(method.ResultName);
    end;
    if (method.ReturnType is TOMInterface) then
    begin
      intf := method.ReturnType as TOMInterface;
      if intf.ConcreteClass <> nil then
      begin
        FTarget.print('  result := ').print(intf.ConcreteClass.Name).print('.Create;').println();
      end
      else
      begin
        FTarget.print('  result := ').print(intf.Name).print('.Create;').println();
      end;
      FTarget.print('  result._addRef;').println;
      FTarget.print('  result.state := esLoading;').println;
      FTarget.print('  if rs.next then').println;
      FTarget.print('  begin').println;
      for _property in intf.Properties do
      begin
        FTarget.print('    result.').print(_property.Name).print(' := rs.');
        try
          FTarget.print(FCurrentContext.findTypeHandler(_property.Getter.ResultName).GetMethod);
        except

        end;
        FTarget.print('ByName(''');
        if (_property.Getter.SetterOf.ReferencedColumn <> nil) then
        begin
          FTarget.print(_property.Getter.SetterOf.ReferencedColumn.ColumnName);
        end
        else
        begin
          FTarget.print(_property.Name);
        end;
        FTarget.print(''');').println;
      end;
      FTarget.print('  end;').println;
      FTarget.print('  result.state := edSyncronized;').println;
    end;
    FTarget.print('End;').println();
    FTarget.flush();
  end;
end;

procedure TUnitWriter.generateMapperMethods(cu: TOMCompilationUnit);
var
  omm: TOMMapper;
begin
  for omm in cu.Mappers do
  begin
    generateInitStatementMethod(omm);
    generateMapperMethods(cu, omm);
  end;
end;

procedure TUnitWriter.generateClass(entity: TOMMapper);
var
  lastVisibility: TVisibleLevel = vlUnknow;
  method: TOMMethod;
  field: TOMField;
  parameter: TOMParameter;
begin
  FTarget.println();
  FTarget.print('  ').print(entity.Name).print(' = class(TBaseMapper)').println();
  entity.Fields.Sort(@sortField);
  entity.Methods.Sort(@sortMethod);
  lastVisibility := vlUnknow;
  for field in entity.Fields do
  begin
    if lastVisibility <> field.Visibility then
    begin
      FTarget.print('  ').print(field.getVisibilityName).println;
      lastVisibility := field.Visibility;
    end;
    FTarget.print('    ').print(field.Name).print(':').print(field.TypeName).print(';').println();
  end;
  lastVisibility := vlUnknow;
  FTarget.print('  protected').println();
  FTarget.print('    procedure InitStatements; Override;').println();
  for method in entity.Methods do
  begin
    if lastVisibility <> method.Visibility then
    begin
      FTarget.print('  ').print(method.getVisibilityName).println;
      lastVisibility := method.Visibility;
    end;
    FTarget.print('    ');
    if (method.ReturnType <> nil) then
    begin
      FTarget.print('function ');
    end
    else
    if (method.ResultName <> '') then
    begin
      FTarget.print('function ');
    end
    else
    begin
      FTarget.print('procedure ');
    end;
    FTarget.print(method.Name).print('(');
    for parameter in method.Parameters do
    begin
      if parameter.ParameterType <> nil then
      begin
        FTarget.print(parameter.Name).print(':').print(parameter.ParameterType.Name);
      end
      else
      begin
        FTarget.print(parameter.Name).print(':').print(parameter.ParameterTypeName);
      end;
      if (method.Parameters.IndexOf(parameter)) < (method.Parameters.Count - 1) then
      begin
        FTarget.print(';');
      end;
    end;
    FTarget.print(')');
    if (method.ReturnType <> nil) then
    begin
      FTarget.print(':').print(method.ReturnType.Name);
    end
    else
    if (method.ResultName <> '') then
    begin
      FTarget.print(':').print(method.ResultName);
    end;
    FTarget.print(';').println();
  end;

  FTarget.print('  End;').println();
end;

procedure TUnitWriter.generate(compilationUnit: TOMCompilationUnit);
begin
  ForceDirectories(ExtractFileDir(compilationUnit.FileName));
  FTarget := TPrintStream.Create(TFileStream.Create(compilationUnit.FileName, fmCreate));
  FTarget.print('unit ').print(compilationUnit.UnitName).print(';').println;
  FTarget.print('(*').println.print(compilationUnit.Copyright).println.print('*)').println;
  FTarget.print('{$mode objfpc}{$H+}').println;
  FTarget.println.print('interface').println;
  FTarget.println.print('uses').println;
  FTarget.println.print('  Classes, SysUtils, ZDbcIntfs, lzbatis.lib;').println;
  generateForWard(compilationUnit);
  generateInterfaces(compilationUnit.Interfaces);
  generateClasses(compilationUnit.Mappers);
  FTarget.println.print('implementation').println;
  generateForWard(compilationUnit.Classes);
  generateClasses(compilationUnit.Classes);
  generateSetterOrGetterMethod(compilationUnit.Classes);
  generateMapperMethods(compilationUnit);
  FTarget.println.print('end.').println;
end;


initialization

finalization

end.
