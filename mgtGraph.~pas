unit mgtGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes, basicGraph;

type
  //характеристики дуги
  TMgtGraphEdgeCharacteristic = ( mgtgecHasBus = 0, mgtgecHasTram = 1, mgtgecHasTrolley = 2, mgtgecIsCompensatory = 3 );

  TMgtGraphEdgeCharacteristicSet = set of TMgtGraphEdgeCharacteristic;

  //для порядка вводим новый класс МГТ-узла, но он точно такой же, как и базовый
  TMgtGraphNode = class( TBasicGraphNode )
  public
    constructor Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ); reintroduce;
    destructor Destroy(); override;
  end;
  //constructor
  //destructor

  //
  TMgtGraphEdge = class( TBasicGraphEdge )
  protected
    //флаги возможности проезда по дуге
    FflHasBus: Boolean;
    FflHasTram: Boolean;
    FflHasTrolley: Boolean;
    FflIsCompensatory: Boolean;
  public
    constructor Create( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0 ); reintroduce;

    destructor Destroy(); override;

    //геттеры
    function GetNodeFrom(): TMgtGraphNode; reintroduce;
    function GetNodeTo(): TMgtGraphNode; reintroduce;

    //сеттеры
    procedure SetNodeFrom( ANewNodeFrom: TMgtGraphNode ); reintroduce;
    procedure SetNodeTo( ANewNodeTo: TMgtGraphNode ); reintroduce;

    property NodeFrom: TMgtGraphNode read GetNodeFrom write SetNodeFrom;
    property NodeTo: TMgtGraphNode read GetNodeTo write SetNodeTo;
    property flHasBus: Boolean read FflHasBus write FflHasBus;
    property flHasTram: Boolean read FflHasTram write FflHasTram;
    property flHasTrolley: Boolean read FflHasTrolley write FflHasTrolley;
    property flIsCompensatory: Boolean read FflIsCompensatory write FflIsCompensatory;
  end;

implementation

{**********************************************************************************************
*TMgtGraphNode.Create
***********************************************************************************************}
constructor TMgtGraphNode.Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );
begin
  inherited Create( AUID, AIndex, AObject, AflOwnsObject);
end;

{**********************************************************************************************
* TMgtGraphNode.Destroy
***********************************************************************************************}
destructor TMgtGraphNode.Destroy();
begin
  inherited Destroy();
end;

{**********************************************************************************************
* TMgtGraphEdge.Create
***********************************************************************************************}
constructor TMgtGraphEdge.Create( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0 );
begin
  inherited Create( ANodeFrom, ANodeTo, AUID, AIndex, Aobject, AflOwnsObject, AflBiDirected, AWeight );
end;

{**********************************************************************************************
* TMgtGraphEdge.Destroy
***********************************************************************************************}
destructor TMgtGraphEdge.Destroy();
begin
  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraphEdge.GetNodeFrom
***********************************************************************************************}
function TMgtGraphEdge.GetNodeFrom(): TMgtGraphNode;
begin
  Result := TMgtGraphNode( Self.FNodeFrom );
end;

{**********************************************************************************************
* TBasicGraphEdge.GetNodeTo
***********************************************************************************************}
function TMgtGraphEdge.GetNodeTo(): TMgtGraphNode;
begin
  Result := TMgtGraphNode( Self.FNodeTo );
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TMgtGraphEdge.SetNodeFrom( ANewNodeFrom: TMgtGraphNode );
begin
  Self.FNodeFrom := ANewNodeFrom;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TMgtGraphEdge.SetNodeTo( ANewNodeTo: TMgtGraphNode );
begin
  Self.FNodeTo := ANewNodeTo;
end;

end.
