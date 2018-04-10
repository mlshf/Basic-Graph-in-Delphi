unit mgtGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes, basicGraph, DbConn;

type
  //характеристики дуги
  TMgtGraphEdgeCharacteristic = ( mgtgecHasBus = 0, mgtgecHasTram = 1, mgtgecHasTrolley = 2, mgtgecIsCompensatory = 3,
                                  mgtgecNoBus = 4, mgtgecNoTram = 5, mgtgecNoTrolley = 6, mgtgecNotCompensatory = 7 );

  TMgtGraphEdgeCharacteristicSet = set of TMgtGraphEdgeCharacteristic;

  TMgtGraphEdge = class;

  //для порядка вводим новый класс МГТ-узла, но он точно такой же, как и базовый
  TMgtGraphNode = class( TBasicGraphNode )
  public
    constructor Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ); reintroduce;
    destructor Destroy(); override;

    //возвращает дугу по её индексу в списке From/To
    function GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TMgtGraphNode; reintroduce;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetNeighbour_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TMgtGraphNode; reintroduce;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetNeighbour_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TMgtGraphNode; reintroduce;

    //возвращает дугу по её индексу в списке From/To
    function GetEdge_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TMgtGraphEdge; reintroduce;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetEdge_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TMgtGraphEdge; reintroduce;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetEdge_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TMgtGraphEdge; reintroduce;
  end;

  //отличается от базовой дуги наличием 4х флагов характеристик
  TMgtGraphEdge = class( TBasicGraphEdge )
  protected
    //флаги возможности проезда по дуге
    //автобуса
    FflHasBus: Boolean;
    //трамвая
    FflHasTram: Boolean;
    //троллейбуса
    FflHasTrolley: Boolean;
    //дуга компенсационная
    FflIsCompensatory: Boolean;
  public
    constructor Create( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0; ACharacteristics: TMgtGraphEdgeCharacteristicSet = [] ); reintroduce;

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

  TMgtGraph = class( TBasicGraph )
  private
    FLimitations_for_FindPath: TMgtGraphEdgeCharacteristicSet;
    FConnection: TDBAConnection;
    FDataBase: String;
    FNodesTable: String;
    FSectionsTable: String;
  protected
  
    //ДОБАВЛЕНИЕ УЗЛА
    function CreateNode( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                          AflOwnsObject: Boolean = False ): TBasicGraphNode; override;

    //добавление дуги
    function CreateEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                          AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                          AWeight: Double = 1.0 ): TBasicGraphEdge; override;
  public
    constructor Create( AConnection: TDBAConnection; ADataBase , ANodesTable, ASectionsTable: String ); reintroduce;
    destructor Destroy(); override;

    function AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TMgtGraphNode; reintroduce;

    //получние узла по его UID
    function GetNode_ByUID( AUID: Int64 ): TMgtGraphNode; reintroduce;
    //получение узла по его объекту
    function GetNode_ByObject( AObject: TOBject ): TMgtGraphNode; reintroduce;

    function AddEdge( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AObject: TObject = nil;
                      AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                      AWeight: Double = 1.0; ACharacteristics: TMgtGraphEdgeCharacteristicSet = [] ): TMgtGraphEdge;

    //получние дуги по её UID
    function GetEdge_ByUID( AUID: Int64 ): TMgtGraphEdge; reintroduce;
    //получение дуги по её объекту
    function GetEdge_ByObject( AObject: TOBject ): TMgtGraphEdge; reintroduce;

    function AddNode_ToEdge( AEdge: TMgtGraphEdge; ANode: TMgtGraphNode; AEdge_NewWeight: Double = 1.0;
                              ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                              ANewEdge_flOwnsObject: Boolean = false;
                              ANewEdge_Characteristics: TMgtGraphEdgeCharacteristicSet = [] ):
                              Boolean; reintroduce;

    //возвращает список подходящих для учёта узлов при алгоритме поиска пути
    function GetAppropriateEdges_for_FindPath( ANode: TBasicGraphNode ): TList; override;

    function FindPath( nodeFrom, nodeTo: TMgtGraphNode; path: TList;
                        ALimitations: TMgtGraphEdgeCharacteristicSet ): double; reintroduce;

    //заполнить граф из БД
    function FillGraph(): boolean;

    property Connection: TDBAConnection read FConnection;
    property DataBase: String read FDataBase;
    property NodesTable: String read FNodesTable;
    property SectionsTable: String read FSectionsTable;
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
* TMgtGraphNode.GetNeighbour_ByIndex
* возвращает дугу по её индексу в списке From/To
***********************************************************************************************}
function TMgtGraphNode.GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited GetNeighbour_ByIndex( AIndex, AListDirection ) );
end;

{**********************************************************************************************
* TMgtGraphNode.GetNeighbour_ByUID
* возвращает дугу по её UID, ищет в списках From/To/From&To
***********************************************************************************************}
function TMgtGraphNode.GetNeighbour_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited GetNeighbour_ByUID( AUID, AListDirection ) );
end;

{**********************************************************************************************
* TMgtGraphNode.GetNeighbour_ByObject
* возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
***********************************************************************************************}
function TMgtGraphNode.GetNeighbour_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited GetNeighbour_ByObject( AObject, AListDirection ) );
end;

{**********************************************************************************************
* TMgtGraphNode.GetEdge_ByIndex
* возвращает дугу по её индексу в списке From/To
***********************************************************************************************}
function TMgtGraphNode.GetEdge_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited GetEdge_ByIndex( AIndex, AlistDirection ) );
end;

{**********************************************************************************************
* TMgtGraphNode.GetEdge_ByUID
* возвращает дугу по её UID, ищет в списках From/To/From&To
***********************************************************************************************}
function TMgtGraphNode.GetEdge_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited GetEdge_ByUID( AUID, AlistDirection ) );
end;

{**********************************************************************************************
* TMgtGraphNode.GetEdge_ByObject
* возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
***********************************************************************************************}
function TMgtGraphNode.GetEdge_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited GetEdge_ByObject( AObject, AlistDirection ) );
end;

{**********************************************************************************************
* TMgtGraphEdge.Create
***********************************************************************************************}
constructor TMgtGraphEdge.Create( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                                  AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                                  AWeight: Double = 1.0; ACharacteristics: TMgtGraphEdgeCharacteristicSet = [] );
begin
  inherited Create( ANodeFrom, ANodeTo, AUID, AIndex, Aobject, AflOwnsObject, AflBiDirected, AWeight );

  FflHasBus := false;
  FflHasTram := false;
  FflHasTrolley := false;
  FflIsCompensatory := false;

  if mgtgecHasBus in ACharacteristics then
    FflHasBus := true;
  if mgtgecHasTram in ACharacteristics then
    FflHasTram := true;
  if mgtgecHasTrolley in ACharacteristics then
    FflHasTrolley := true;
  if mgtgecIsCompensatory in ACharacteristics then
    FflIsCompensatory := true;
end;

{**********************************************************************************************
* TMgtGraphEdge.Destroy
***********************************************************************************************}
destructor TMgtGraphEdge.Destroy();
begin
  inherited Destroy();
end;

{**********************************************************************************************
* TMgtGraphEdge.GetNodeFrom
***********************************************************************************************}
function TMgtGraphEdge.GetNodeFrom(): TMgtGraphNode;
begin
  Result := TMgtGraphNode( Self.FNodeFrom );
end;

{**********************************************************************************************
* TMgtGraphEdge.GetNodeTo
***********************************************************************************************}
function TMgtGraphEdge.GetNodeTo(): TMgtGraphNode;
begin
  Result := TMgtGraphNode( Self.FNodeTo );
end;

{**********************************************************************************************
* TMgtGraphEdge.SetNode1
***********************************************************************************************}
procedure TMgtGraphEdge.SetNodeFrom( ANewNodeFrom: TMgtGraphNode );
begin
  Self.FNodeFrom := ANewNodeFrom;
end;

{**********************************************************************************************
* TMgtGraphEdge.SetNode2
***********************************************************************************************}
procedure TMgtGraphEdge.SetNodeTo( ANewNodeTo: TMgtGraphNode );
begin
  Self.FNodeTo := ANewNodeTo;
end;

{**********************************************************************************************
* TMgtGraph.Create
***********************************************************************************************}
constructor TMgtGraph.Create( AConnection: TDBAConnection; ADataBase, ANodesTable, ASectionsTable: String );
begin
  inherited Create();
  
  FLimitations_for_FindPath := [];
  FConnection := AConnection;
  FDataBase := ADataBase;
  FNodesTable := ANodesTable;
  FSectionsTable := ASectionsTable;
end;

{**********************************************************************************************
* TMgtGraph.Destroy
***********************************************************************************************}
destructor TMgtGraph.Destroy();
begin
  inherited Destroy();
end;

{**********************************************************************************************
* TMgtGraph.CreateNode
***********************************************************************************************}
function TMgtGraph.CreateNode( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                                  AflOwnsObject: Boolean = False ): TBasicGraphNode;
begin
  Result := TMgtGraphNode.Create( AUID, AIndex, AObject, AflOwnsObject );
end;

{**********************************************************************************************
* TMgtGraph.AddNode
* добавить узел в список узлов, возвращает индекс
***********************************************************************************************}
function TMgtGraph.AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited AddNode( AUID, AObject, AflOwnsObject ) );
end;

{**********************************************************************************************
* TMgtGraph.GetNode_ByUID
* получние узла по его UID
***********************************************************************************************}
function TMgtGraph.GetNode_ByUID( AUID: Int64 ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited GetNode_ByUID( AUID ) );
end;

{**********************************************************************************************
* TMgtGraph.GetNode_ByObject
***********************************************************************************************}
function TMgtGraph.GetNode_ByObject( AObject: TObject ): TMgtGraphNode;
begin
  Result := TMgtGraphNode( inherited GetNode_ByObject( AObject ) );
end;

{**********************************************************************************************
* TMgtGraph.CreateEdge
* на вход будут подаваться только TMgtGraphNode
***********************************************************************************************}
function TMgtGraph.CreateEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                                AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                                AWeight: Double = 1.0 ): TBasicGraphEdge;
begin
  Result := TMgtGraphEdge.Create( TMgtGraphNode( ANodeFrom ), TMgtGraphNode( ANodeTo ), AUID, AIndex, AObject, AflOwnsObject,
                                  AflBiDirected, AWeight );
end;

{**********************************************************************************************
*  TMgtGraph.AddEdge
***********************************************************************************************}
function TMgtGraph.AddEdge( ANodeFrom, ANodeTo: TMgtGraphNode; AUID: Int64 = -1; AObject: TObject = nil;
                            AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                            AWeight: Double = 1.0; ACharacteristics: TMgtGraphEdgeCharacteristicSet = [] ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited AddEdge( ANodeFrom, ANodeTo, AUID, AObject, AflOwnsObject, AflBiDirected, AWeight ) );

  Result.flHasBus := false;
  Result.flHasTram := false;
  Result.flHasTrolley := false;
  Result.flIsCompensatory := false;

  if mgtgecHasBus in ACharacteristics then
    Result.flHasBus := true;
  if mgtgecHasTram in ACharacteristics then
    Result.flHasTram := true;
  if mgtgecHasTrolley in ACharacteristics then
    Result.flHasTrolley := true;
  if mgtgecIsCompensatory in ACharacteristics then
    Result.flIsCompensatory := true;
end;

{**********************************************************************************************
* TMgtGraph.GetEdge_ByUID
* получние дуги по её UID
***********************************************************************************************}
function TMgtGraph.GetEdge_ByUID( AUID: Int64 ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited GetEdge_ByUID( AUID ) );
end;

{**********************************************************************************************
* TMgtGraph.GetEdge_ByObject
* получение дуги по её объекту
***********************************************************************************************}
function TMgtGraph.GetEdge_ByObject( AObject: TOBject ): TMgtGraphEdge;
begin
  Result := TMgtGraphEdge( inherited GetEdge_ByObject( AObject ) );
end;

{**********************************************************************************************
* TMgtGraph.AddNode_ToEdge
***********************************************************************************************}
function TMgtGraph.AddNode_ToEdge( AEdge: TMgtGraphEdge; ANode: TMgtGraphNode; AEdge_NewWeight: Double = 1.0;
                                      ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                                      ANewEdge_flOwnsObject: Boolean = false;
                                      ANewEdge_Characteristics: TMgtGraphEdgeCharacteristicSet = [] ):
                                      Boolean;
var
  newEdge: TMgtGraphEdge;
  newEdge_Weight: Double;
begin
  Result := False;

  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EGraphError.Create( 'Невозможно добавить узел в несуществующий (nil) список узлов.' );

  newEdge_Weight := AEdge.Weight - AEdge_NewWeight;
  if ( newEdge_Weight >= AEdge.Weight )
  OR ( newEdge_Weight <= 0.0 ) then
    Exit;

  try
    AEdge.NodeTo.DeleteEdge( AEdge );
    newEdge := AddEdge( ANode, AEdge.NodeTo, ANewEdge_UID, ANewEdge_Object, ANewEdge_flOwnsObject,
                        AEdge.FlBiDirected, newEdge_Weight, ANewEdge_Characteristics );
    AEdge.NodeTo := ANode;
    ANode.AddEdge( AEdge );    
    AEdge.Weight := AEdge_NewWeight;
  except
    on E:Exception do
      raise EGraphError.Create( 'Не удалось добавить узел. Сообщение ошибки:' + sLineBreak + E.Message );
  end;

  Result := True;

end;

{**********************************************************************************************
*TMgtGraph.GetAppropriateEdges_for_FindPath
* на вход идёт нода класса МГТ
***********************************************************************************************}
function TMgtGraph.GetAppropriateEdges_for_FindPath( ANode: TBasicGraphNode ): TList;
var
  i: integer;
  currEdge: TMgtGraphEdge;
  node: TMgtGraphNode;
begin
  Result := TList.Create();
  node := TMgtGraphNode( ANode );
  for i := 0 to node.GetEdgesCount( gdFrom ) - 1 do
  begin
    currEdge := node.GetEdge_ByIndex( i, gdFrom );
    if ( FLimitations_for_FindPath <> [] )
      and
      (
        ( ( mgtgecHasBus in FLimitations_for_FindPath ) and ( not currEdge.flHasBus ) )
        or ( ( mgtgecHasTram in FLimitations_for_FindPath ) and ( not currEdge.flHasTram ) )
        or ( ( mgtgecHasTrolley in FLimitations_for_FindPath ) and ( not currEdge.flHasTrolley ) )
        or ( ( mgtgecIsCompensatory in FLimitations_for_FindPath ) and ( not currEdge.flIsCompensatory ) )
        or ( ( mgtgecNoBus in FLimitations_for_FindPath ) and ( currEdge.flHasBus ) )
        or ( ( mgtgecNoTram in FLimitations_for_FindPath ) and ( currEdge.flHasTram ) )
        or ( ( mgtgecNoTrolley in FLimitations_for_FindPath ) and ( currEdge.flHasTrolley ) )
        or ( ( mgtgecNotCompensatory in FLimitations_for_FindPath ) and ( currEdge.flIsCompensatory ) )
      )
    then
      Continue
    else
      Result.Add( currEdge );
  end;
end;

{**********************************************************************************************
* TMgtGraph.FindPath(
***********************************************************************************************}
function TMgtGraph.FindPath( nodeFrom, nodeTo: TMgtGraphNode; path: TList; ALimitations: TMgtGraphEdgeCharacteristicSet ): double;
begin
  if ( ( mgtgecHasBus in ALimitations ) and ( mgtgecNoBus in ALimitations ) )
    OR ( ( mgtgecHasTram in ALimitations ) and ( mgtgecNoTram in ALimitations ) )
    OR ( ( mgtgecHasTrolley in ALimitations ) and ( mgtgecNoTrolley in ALimitations ) )
    OR ( ( mgtgecIsCompensatory in ALimitations ) and ( mgtgecNotCompensatory in ALimitations ) )
  then
    raise EGraphError.Create( 'В множестве ограничений присутствуют взаимоисключающие условия.' );
  FLimitations_for_FindPath := ALimitations;
  Result := inherited FindPath( nodeFrom, nodeTo, path );
  FLimitations_for_FindPath := [];
end;

//заполнить граф из БД
function TMgtGraph.FillGraph(): boolean;
var
  dbResult_Nodes, dbResult_Edges: TDBResult;
  queryString_Nodes, queryString_Edges: String;
  characteristics: TMgtGraphEdgeCharacteristicSet;
begin
  Result := false;

  queryString_Nodes := 'SELECT '
                  + FConnection.quoteName( 'MUID' )
                  + ' FROM '
                  + FConnection.quoteName( FDataBase ) + '.' + FConnection.quoteName( FNodesTable )
                  + ' WHERE '
                  + FConnection.quoteName( 'sign_deleted' ) + ' = 0'
                  + ' AND ' + FConnection.quoteName( 'OKEY' ) + ' > 1';

  queryString_Edges := 'SELECT '
                  + FConnection.quoteName( 'MUID' ) + ', '
                  + FConnection.quoteName( 'startNodeMUID' ) + ', '
                  + FConnection.quoteName( 'endNodeMUID' ) + ', '
                  + FConnection.quoteName( 'ObjectLength' ) + ', '
                  + FConnection.quoteName( 'has_bus' ) + ', '
                  + FConnection.quoteName( 'has_tram' ) + ', '
                  + FConnection.quoteName( 'has_trolley' ) + ', '
                  + FConnection.quoteName( 'is_compensatory' )
                  + ' FROM '
                  + FConnection.quoteName( FDataBase ) + '.' + FConnection.quoteName( FSectionsTable )
                  + ' WHERE '
                  + FConnection.quoteName( 'sign_deleted' ) + ' = 0'
                  + ' AND ' + FConnection.quoteName( 'OKEY' ) + ' > 1';

  if ( FConnection.QueryOpen( queryString_Edges, dbResult_Edges, true ) = 0 )
    and ( FConnection.QueryOpen( queryString_Nodes, dbResult_Nodes, true ) = 0 )
  then
  begin
    while dbResult_Nodes.Fetch() do
    begin
      AddNode( dbResult_Nodes.asInt64( 0 ) );
    end;
    
    while dbResult_Edges.Fetch() do
    begin
      characteristics := [];
      if dbResult_Edges.asInteger( 4 ) = 1 then
        Include( characteristics, mgtgecHasBus );
      if dbResult_Edges.asInteger( 5 ) = 1 then
        Include( characteristics, mgtgecHasTram );
      if dbResult_Edges.asInteger( 6 ) = 1 then
        Include( characteristics, mgtgecHasTrolley );
      if dbResult_Edges.asInteger( 7 ) = 1 then
        Include( characteristics, mgtgecIsCompensatory );

      if ( GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ) <> nil )
        AND ( GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ) <> nil )
      then
      begin
        AddEdge(  GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ),
                  GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ),
                  dbResult_Edges.asInt64( 0 ),
                  nil,
                  false,
                  false,
                  dbResult_Edges.asFloat( 3 ),
                  characteristics );
        end;
    end;

    FreeAndNil( dbResult_Nodes );
    FreeAndNil( dbResult_Edges );

    Result := true;
  end;
end;

end.
