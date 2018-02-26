unit lzbatis.dom.aspects;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, Laz2_Dom, SysUtils;

type
  TJQueryResult = specialize TFPGObjectList<TDOMNode>;
  { TJQuery }

  TJQuery = class helper for TDOMNode
  private
    function GetHRef: DOMString;
    procedure SetHRef(AValue: DOMString);
  public
    function attr(attrName: DOMString): DOMString; overload;
    function hasAttr(attrName: DOMString): boolean;
    function hasClass(aClassName: DOMString): boolean;
    function newElement(const aTagName, aClass: DOMString; const aText: DOMString = ''): TDOMElement;
    function select(selector: DOMString; debug: boolean = False): TJQueryResult;
    function selectOne(selector: DOMString; debug: boolean = False): TDOMNode;
    function Count(selector: DOMString; debug: boolean = False): integer;
    function Text: UTF8String; overload;
    procedure addClass(aClassName: DOMString); overload;
    procedure attr(attrName: DOMString; attrValue: DOMString); overload;
    procedure removeAttr(attrName: DOMString); overload;
    procedure removeChilds(selector: DOMString);
    procedure removeClass(aClassName: string); overload;
    procedure toggleClass(aClassName: DOMString); overload;
    property HRef: DOMString read GetHRef write SetHRef;
    function attrs: TJQueryResult;
    function prettyPrint(const level: integer = -1): DOMString;
  end;


implementation

uses
  dateutils, laz2_XMLWrite, laz2_xpath, regexpr, SAX_HTML;

{ TJQuery }
procedure TJQuery.SetHRef(AValue: DOMString);
begin
  attr('href', AValue);
end;

procedure TJQuery.toggleClass(aClassName: DOMString);
begin
  if (hasClass(aClassName)) then
  begin
    removeClass(aClassName);
  end
  else
  begin
    addClass(aClassName);
  end;
end;

function TJQuery.attrs: TJQueryResult;
var
  N: TDOMNode;
  idx: integer;
begin
  Result := TJQueryResult.Create(False);
  for idx := 0 to self.Attributes.Length - 1 do
  begin
    Result.add(self.Attributes.Item[idx]);
  end;
end;

function TJQuery.prettyPrint(const level: integer): DOMString;
var
  n: TDOMNode;
  idx: integer;
begin
  if assigned(self) then
  begin
    Result := '<' + self.NodeName;
    if assigned(Self.Attributes) then
    begin
      for idx := 0 to Self.Attributes.Length - 1 do
      begin
        n      := Self.Attributes.Item[idx];
        Result += ' ' + n.NodeName + '="' + NodeValue + '"';
      end;
    end;
    if assigned(self.ChildNodes) then
    begin
      if (self.ChildNodes.Count = 0) then
      begin
        Result += '/>';
        exit;
      end
      else
      begin
        Result += '>';
        if (level <> 0) then
        begin
          for idx := 0 to Self.ChildNodes.Length - 1 do
          begin
            n := Self.ChildNodes.Item[idx];
            if assigned(N) then
            begin
              Result += LineEnding + '  ' + n.prettyPrint(level - 1);
            end;
          end;
        end;
        Result +=
          '</' + NodeName + '>';
      end;
    end;
  end;
end;

function TJQuery.GetHRef: DOMString;
begin
  Result := attr('href');
end;

procedure TJQuery.removeClass(aClassName: string);
begin
  attr('class', StringReplace(attr('class'), aClassName, '', [rfReplaceAll, rfIgnoreCase]));
end;

procedure TJQuery.addClass(aClassName: DOMString);
begin
  if not hasClass(aClassName) then
  begin
    attr('class', attr('class') + ' ' + aClassName);
  end;
end;

procedure TJQuery.removeAttr(attrName: DOMString);
var
  node: TDOMNode;
begin
  node := self.Attributes.GetNamedItem(attrName);
  if assigned(node) then
  begin
    self.Attributes.RemoveNamedItem(attrName);
  end;
end;

procedure TJQuery.removeChilds(selector: DOMString);
var
  nl: TJQueryResult;
  indice: integer;
  n, pn: TDOMNode;
begin
  nl := nil;
  try
    try
      nl := select(selector);
      for indice := nl.Count - 1 downto 0 do
      begin
        n  := nl[indice];
        pn := n.ParentNode;
        pn.RemoveChild(n);
      end;
    finally
      nl.Free;
    end;
  except
  end;
end;

function TJQuery.hasClass(aClassName: DOMString): boolean;
begin
  Result := Pos(aClassName, attr('class')) > 0;
end;

function TJQuery.select(selector: DOMString; debug: boolean): TJQueryResult;
var
  xPathResult: TXPathVariable;
  idx: integer;
  node: TDOMNode;
  xpathSelector: string;
begin
  if (selector[1] = '/') then
  begin
    xpathSelector := selector;
  end
  else
  begin
    xpathSelector := '//' + selector;
    xpathSelector := ReplaceRegExpr('\.([a-zA-Z0-9\-]*)', xpathSelector, '[contains(@class,"$1")]', True);
    xpathSelector := ReplaceRegExpr('\#([a-zA-Z0-9\-]*)', xpathSelector, '[contains(@id,"$1")]', True);
  end;
  Result      := TJQueryResult.Create(False);
  xPathResult := EvaluateXPathExpression(DOMString(xpathSelector), self);
  if assigned(xPathResult) then
  begin
    if (debug) then
    begin
      WriteLn(xpathSelector);
      Write(xPathResult.TypeName, ' ');
      if (xPathResult is TXPathNodeSetVariable) then
      begin
        Writeln('conteggio nodi : ', xPathResult.AsNodeSet.Count);
      end
      else
      if (xPathResult is TXPathBooleanVariable) then
      begin
        Writeln('Boolean : ', xPathResult.AsBoolean);
      end
      else
      if (xPathResult is TXPathNumberVariable) then
      begin
        Writeln('Number : ', xPathResult.AsNumber);
      end
      else
      if (xPathResult is TXPathStringVariable) then
      begin
        Writeln('String : ', xPathResult.AsText);
      end;
    end;
    for idx := 0 to xPathResult.AsNodeSet.Count - 1 do
    begin
      node := TDOMNode(xPathResult.AsNodeSet.Items[idx]);
      Result.Add(node);
    end;
    xPathResult.Release;
  end;
end;

function TJQuery.selectOne(selector: DOMString; debug: boolean): TDOMNode;
var
  xPathResult: TXPathVariable;
  xpathSelector: string;
begin
  Result := nil;
  if (selector[1] = '/') then
  begin
    xpathSelector := selector;
  end
  else
  begin
    xpathSelector := '//' + selector;
    xpathSelector := ReplaceRegExpr('\.([a-zA-Z0-9\-]*)', xpathSelector, '[contains(@class,"$1")]', True);
    xpathSelector := ReplaceRegExpr('\#([a-zA-Z0-9\-]*)', xpathSelector, '[contains(@id,"$1")]', True);
  end;
  xPathResult := EvaluateXPathExpression(DOMString(xpathSelector), self);
  if assigned(xPathResult) then
  begin
    if (debug) then
    begin
      WriteLn(xpathSelector);
      Write(xPathResult.TypeName, ' ');
      if (xPathResult is TXPathNodeSetVariable) then
      begin
        Writeln('conteggio nodi : ', xPathResult.AsNodeSet.Count);
      end
      else
      if (xPathResult is TXPathBooleanVariable) then
      begin
        Writeln('Boolean : ', xPathResult.AsBoolean);
      end
      else
      if (xPathResult is TXPathNumberVariable) then
      begin
        Writeln('Number : ', xPathResult.AsNumber);
      end
      else
      if (xPathResult is TXPathStringVariable) then
      begin
        Writeln('String : ', xPathResult.AsText);
      end;
    end;
    if (xPathResult.AsNodeSet.Count > 0) then
    begin
      Result := TDOMNode(xPathResult.AsNodeSet.Items[0]);
    end
    else
    begin
      Result := nil;
    end;
    xPathResult.Release;
  end;
end;

function TJQuery.Count(selector: DOMString; debug: boolean): integer;
var
  nl: TJQueryResult;
begin
  nl     := select(selector, debug);
  Result := nl.Count;
  nl.Free;
end;

function TJQuery.attr(attrName: DOMString): DOMString;
var
  node: TDOMNode;
begin
  Result := '';
  if assigned(self) and assigned(self.Attributes) then
  begin
    node := self.Attributes.GetNamedItem(attrName);
    if assigned(node) then
    begin
      Result := node.NodeValue;
    end;
  end;
end;

function TJQuery.hasAttr(attrName: DOMString): boolean;
var
  node: TDOMNode;
begin
  Result := False;
  if assigned(self) and assigned(self.Attributes) then
  begin
    node := self.Attributes.GetNamedItem(attrName);
    if assigned(node) then
    begin
      Result := True;
    end;
  end;
end;

procedure TJQuery.attr(attrName: DOMString; attrValue: DOMString);
var
  node: TDOMNode;
begin
  node := self.Attributes.GetNamedItem(attrName);
  if node = nil then
  begin
    node := FOwnerDocument.CreateAttribute(attrName);
    self.Attributes.setNamedItemNS(node);
  end;
  node.NodeValue := attrValue;
end;

function TJQuery.Text: UTF8String;
begin
  Result := UTF8Encode(TextContent);
end;

function TJQuery.newElement(const aTagName, aClass: DOMString; const aText: DOMString): TDOMElement;
begin
  if Self is TDOMDocument then
  begin
    Result := TDoMDocument(Self).CreateElement(aTagName);
  end
  else
  begin
    Result := self.OwnerDocument.CreateElement(aTagName);
  end;
  if (aClass <> '') then
  begin
    Result.AttribStrings['class'] := aClass;
  end;
  Result.TextContent := aText;
end;


end.
