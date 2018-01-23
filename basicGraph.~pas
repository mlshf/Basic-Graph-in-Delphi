unit basicGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer;

type
  EBasicGraphError = class(Exception);

  TBasicGraphDirection = ( bgdFrom = 0, bgdTo = 1 );

  TBasicGraphDirectionArray = set of TBasicGraphDirection;

  TBasicGraphDeletionMode = ( bgdmAllowOnlyTarget = 0, bgdmAllowTargetAndEdges = 1 );

  TBasicGraphEdge = class;

  //БАЗОВЫЙ КЛАСС - ОПИСАНИЕ УЗЛА ГРАФА
  TBasicGraphNode = class( TObject )
  private
    //уникальный идентификатор
    FUID: Int64;
    //ассоциированный с узлом объект
    FObject: TObject;
    //флаг удаления объекта при удалении узла
    FflOwnsObject: Boolean;
    //список дуг, входящих в узел
    FEdgesList_To: TList;
    //список дуг, выходящих из узла
    FEdgesList_From: TList;
    //список узлов-соседей, из которых можно попасть в узел
    FNeighboursList_To: TList;
    //список узлов-соседей, в которые можно попасть из узла
    FNeighboursList_From: TList;

    //добавить дугу в список (или в оба списка) дуг
    procedure AddEdge( AEdge: TBasicGraphEdge );

    //удалить дугу из обоих списков дуг
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //удалить дугу из обоих списков дуг, найдя её по UID
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //удалить дугу из обоих списков дуг, найдя её по объекту
    procedure DeleteEdge_ByObject( AObject: TObject );

  public
    constructor Create( AUID: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //ПРОВЕРКА НАЛИЧИЯ УЗЛА-СОСЕДА
    //проверить наличие узла-соседа в списках соседей From/To/From&To
    function HasNeighbour( ANode: TBasicGraphNode; AListTypes: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие узла-соседа в списках соседей From/To/From&To по UID
    function HasNeighbour_ByUID( AUID: Int64; AListTypes: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие узла-соседа в списках соседей From/To/From&To по ассоциированному объекту
    function HasNeighbour_ByObject( AObject: TObject; AListTypes: TBasicGraphDirectionArray ): Boolean;

    //РАБОТА С УЗЛАМИ-СОСЕДЯМИ
    //возвращает количество узлов-соседей в списке From/To
    function GetNeighboursCount( AListType: TBasicGraphDirection ): Integer;
    //возвращает дугу по её индексу в списке From/To
    function GetNeighbour_ByIndex( AIndex: Integer; AListType: TBasicGraphDirection ): TBasicGraphEdge;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetNeighbour_ByUID( AUID: Int64; AListTypes: TBasicGraphDirectionArray ): TbasicGraphEdge;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetNeighbour_ByObject( AObject: TObject; AListTypes: TBasicGraphDirectionArray ): TbasicGraphEdge;

    //----------------------------------------------------------------------------------------------------------------

    //ПРОВЕРКА НАЛИЧИЯ ДУГИ
    //проверить наличие дуги в списках дуг From/To/From&To
    function HasEdge( AEdge: TBasicGraphEdge; AListTypes: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие дуги в списках дуг From/To/From&To по UID
    function HasEdge_ByUID( AUID: Int64; AListTypes: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие дуги в списках дуг From/To/From&To по ассоциированному объекту
    function HasEdge_ByObject( AObject: TObject; AListTypes: TBasicGraphDirectionArray ): Boolean;

    //РАБОТА С ДУГАМИ
    //возвращает количество дуг в списке From/To
    function GetEdgesCount( AListType: TBasicGraphDirection ): Integer;
    //возвращает дугу по её индексу в списке From/To
    function GetEdge_ByIndex( AIndex: Integer; AListType: TBasicGraphDirection ): TBasicGraphEdge;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetEdge_ByUID( AUID: Int64; AListTypes: TBasicGraphDirectionArray ): TbasicGraphEdge;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetEdge_ByObject( AObject: TObject; AListTypes: TBasicGraphDirectionArray ): TbasicGraphEdge;

    //----------------------------------------------------------------------------------------------------------------

    //сеттеры
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
  end;

  //БАЗОВЫЙ КЛАСС - ОПИСАНИЕ ДУГИ ГРАФА
  TBasicGraphEdge = class( TObject )
  private
    //Уникальный идентификатор
    FUID: Int64;
    //связанный с дугой объект
    FObject: TObject;
    //флаг удаления объекта при удалении дуги
    FflOwnsObject: Boolean;
    //узел - начало
    FNodeFrom: TBasicGraphNode;
    //узел - конец
    FNodeTo: TBasicGraphNode;
    //Флаг двунаправленности дуги. False (по-умолчанию): направление дуги From -> To. True: направление дуги From <-> To. 
    FflBiDirected: Boolean;
    //Вес дуги. По-умолчанию равен 1.
    FWeight: Double;

  public
    constructor Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0 ); reintroduce;

    destructor Destroy(); override;

    //сеттеры                    
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );
    procedure SetNodeFrom( ANewNodeFrom: TBasicGraphNode );
    procedure SetNodeTo( ANewNodeTo: TBasicGraphNode );
    procedure SetflBiDirected( AflBiDirected: Boolean );
    procedure SetWeight( AWeight: double );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property FlOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeFrom: TBasicGraphNode read FNodeFrom write SetNodeFrom;
    property NodeTo: TBasicGraphNode read FNodeTo write SetNodeTo;
    property FlBiDirected: Boolean read FflBiDirected write FflBiDirected;
    property Weight: Double read FWeight write SetWeight;
  end;

  TBasicGraph = class( TObject )
  private
    //Хэш-контейнер, хранящий узел по UID. Обладает своими объектами.
    FNodes_ByUID: THashContainerInt64;
    //Хэш-контейнер, хранящий узел по ассоциированному объекту. Не обладает своими объектами.
    FNodes_ByObject: THashContainer;
    //Хэш-контейнер, хранящий дугу по UID. Обладает своими объектами.
    FEdges_ByUID: THashContainerInt64;
    //Хэш-контейнер, хранящий дугу по ассоциированному объекту. Обладает своими объектами.
    FEdges_ByObject: THashContainer;

  public
    constructor Create(); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //ДОБАВЛЕНИЕ УЗЛА
    //Добавить узел в список узлов, возвращает объект
    function AddNode( AUID: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;

    //УДАЛЕНИЕ УЗЛА
    //Удалить узел. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteNode( ANode: TBasicGraphNode;
                          ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //Удалить узел по UID. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteNode_ByUniqueID( AUID: Integer;
                                    ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //Удалить узел по объекту. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteNode_ByObject( AObject: TObject;
                                  ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;

    //ПОИСК УЗЛА
    //получние узла по его UID
    function GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
    //получение узла по его объекту
    function GetNode_ByObject( AObject: TOBject ): TBasicGraphNode; 

    //----------------------------------------------------------------------------------------------------------------

    //ДОБАВЛЕНИЕ ДУГИ
    //Добавить дугу в список дуг, возвращает объект
    function AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: integer = -1; AObject: TObject = nil;
                      AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                      AWeight: Double = 1.0 ): TBasicGraphEdge;

    //УДАЛЕНИЕ ДУГИ
    //Удалить дугу. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteEdge( AEdge: TBasicGraphEdge;
                          ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //Удалить дугу по UID. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteEdge_ByUniqueID( AUID: Integer;
                                    ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //Удалить дугу по объекту. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteEdge_ByObject( AObject: TObject;
                                  ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;

    //ПОИСК ДУГИ
    //получние дуги по её UID
    function GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
    //получение дуги по её объекту
    function GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;
  end;

implementation

{**********************************************************************************************
* TBasicGraphNode.SetObject
***********************************************************************************************}
procedure TBasicGraphNode.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphNode.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphNode.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphNode.Create
***********************************************************************************************}
constructor TBasicGraphNode.Create( AUID: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );
begin
  inherited Create();

  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;

  FEdgesList_To := TList.Create();
  FEdgesList_From := TList.Create();
  FNeighboursList_To := TList.Create();
  FNeighboursList_From := TList.Create();
end;

{**********************************************************************************************
* TBasicGraphNode.AddEdge
* Добавляет дугу в соответствующий список дуг
* Добавляет узел-сосед в соответствующий список соседей
***********************************************************************************************}
procedure TBasicGraphNode.AddEdge( AEdge: TBasicGraphEdge );

  //добавляет нужные данные в списки From
  procedure AddEdge_FromSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //если есть дуга в списке Edges From, то просто выход
    for index := 0 to FEdgesList_From.Count - 1 do
      if TBasicGraphEdge( FEdgesList_From[ index ] ) = AEdge then
        Exit;

    FEdgesList_From.Add( AEdge );

    //если есть сосед в списке Neighbors From, то просто выход
    for index := 0 to FNeighboursList_From.Count - 1 do
      if TBasicGraphNode( FNeighboursList_From[ index ] ) = AEdge.NodeTo then
        Exit;
        
    FNeighboursList_From.Add( AEdge.NodeTo );
  end;

  //добавляет нужные данные в списки To
  procedure AddEdge_ToSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //если есть дуга в списке Edges To, то просто выход
    for index := 0 to FEdgesList_To.Count - 1 do
      if TBasicGraphEdge( FEdgesList_To[ index ] ) = AEdge then
        Exit;
    FEdgesList_To.Add( AEdge );

    //если есть сосед в списке Neighbours To, то просто выход
    for index := 0 to FNeighboursList_To.Count - 1 do
      if TBasicGraphNode( FNeighboursList_To[ index ] ) = AEdge.NodeFrom then
        Exit;
    FNeighboursList_To.Add( AEdge.NodeFrom );
  end;

begin
  if AEdge = nil then
    raise EBasicGraphError.Create( 'Невозможно добавить дугу в список дуг узла, т.к. дуга нулевая (nil).' );

  if ( FEdgesList_To = nil )
    OR ( FEdgesList_From = nil )
    OR ( FNeighboursList_To = nil )
    OR ( FNeighboursList_From = nil ) then
    raise EBasicGraphError.Create( 'Один из списков дуг/соседей нулевой (nil).' );

  try
    //если дуга двунаправленная
    if AEdge.flBiDirected then
    begin
      AddEdge_FromSelf( AEdge );
      AddEdge_ToSelf( AEdge );
    end
    else//дуга однонаправленная
    begin
      if Self = AEdge.NodeFrom then
      begin
        AddEdge_FromSelf( AEdge );
      end
      else
      begin
        if Self = AEdge.NodeTo then
        begin
          AddEdge_ToSelf( AEdge );  
        end
        else
          raise EBasicGraphError.Create( 'Попытка добавить в список дуг узла дугу, не связанную с этим узлом!' );
      end;
    
    end;
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось добавить дугу в список дуг.' + sLineBreak + 'Сообщение об ошибке:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge( AEdge: TBasicGraphEdge );
var
  index: integer;
begin
  if edge = nil then
    raise EBasicGraphError.Create( 'Невозможно удалить дугу, поскольку она нулевая (nil).' );

  if FEdgesList = nil then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
  try
    index := FEdgesList.IndexOf( edge );
    if index >= 0 then
      FEdgesList.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение об ошибке:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.
* delete edge from edge list by edge's index
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByIndex( index: integer );
begin    
  if FEdgesList = nil then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
  try
    if ( index >= 0 ) and ( index < FEdgesList.Count ) then
      FEdgesList.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByUniqueID
* работает медленнее, потому что в списке надо найти
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByUniqueID( uid: integer );
var
  index: integer;

  function IndexOf_ByUniqueID( EdgesList: TList; uid: integer ): integer;
  begin
    Result := 0;
    //если нет элементов, вернуть -1
    if EdgesList.Count = 0 then
    begin
      Result := -1;
      Exit;
    end;

    while ( Result < EdgesList.Count ) and ( TBasicGraphNode( EdgesList[Result] ).UniqueID <> uid ) do
      Inc( Result );

    //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
    if EdgesList.Count = Result then
      Result := -1;
  end;

begin       
  if FEdgesList = nil then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
    
  try
    index := IndexOf_ByUniqueID( FEdgesList, uid );
    if index >= 0 then
      FEdgesList.Delete( index );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.NeighborList
* outputs full list of neighbors
***********************************************************************************************}
function TBasicGraphNode.NeighborList(): TList;
var
  retval: TList;
  index: integer;
  neighbor: TBasicGraphNode;
begin
  Result := nil;
  if FEdgesList = nil then
    Exit;

  try
    retval := TList.Create();

    for index := 0 to Self.EdgesList.Count - 1 do
    begin
      neighbor := TBasicGraphEdge( Self.EdgesList[ index ] ).GetOtherNode( Self );

      if neighbor = nil then
      begin
        retval.Clear();
        FreeAndNil(retval);
        raise EBasicGraphError.Create( 'Обнаружена дуга только с одним узлом. Unique ID дуги = ' +
                                        IntToStr( TBasicGraphEdge( Self.EdgesList[ index ] ).UniqueID ) );
      end;

      retval.Add( neighbor );
    end;
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось создать список всех соседей узла.' + sLineBreak + 'Сообщение ошибки:'
                                      + sLineBreak + E.Message );
  end;

  Result := retval;
end;

{**********************************************************************************************
* TBasicGraphNode.NodesMatch
* сравнение узлов на равенство. Если не равны - false, иначе true
* списки дуг сравниваются
***********************************************************************************************}
function TBasicGraphNode.NodesMatch( node: TBasicGraphNode ): Boolean;
var
  index: integer;
  selfEdges, nodeEdges: TList;
begin
  Result := false;

  if self.UniqueID <> node.UniqueID then
    Exit;

  if self.AObject <> node.AObject then
    exit;

  if self.flOwnsObject <> node.flOwnsObject then
    Exit;

  if Self.EdgesList.Count <> node.EdgesList.Count then
    exit;

  index := 0;
  selfEdges := Self.EdgesList;
  nodeEdges := node.EdgesList;
  while ( index < selfEdges.Count ) and ( index < nodeEdges.Count ) do
  begin
    if selfEdges[ index ] <> nodeEdges[ index ] then
      Exit;
    index := index + 1;
  end;

  Result := true;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetUniqueID
***********************************************************************************************}
procedure TBasicGraphEdge.SetUniqueID( uid: integer );
begin
  Self.FUID := uid;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetFlOwnsObject( flOwnsObject: boolean );
begin
  Self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TBasicGraphEdge.SetNode1( new_node1: TBasicGraphNode );
begin
  Self.FNodeFrom := new_node1;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TBasicGraphEdge.SetNode2( new_node2: TBasicGraphNode );
begin
  Self.FNodeTo := new_node2;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetDirection
***********************************************************************************************}
procedure TBasicGraphEdge.SetDirection( direction: TBasicGraphEdgeDirection );
begin
  Self.FDirection := direction;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetWeight
***********************************************************************************************}
procedure TBasicGraphEdge.SetWeight( weight: double );
begin
  Self.FWeight := weight;
end;

{**********************************************************************************************
* TBasicGraphEdge.Create
***********************************************************************************************}
constructor TBasicGraphEdge.Create( nodeFrom, nodeTo: TBasicGraphNode; uid: integer = -1; AObject: TObject = nil;
                                    flOwnsObject: Boolean = false; weight: Double = 1.0;
                                    direction: TBasicGraphEdgeDirection = bgedBidirected );
begin
  inherited Create();

  if (nodeFrom = nil) or (nodeTo = nil) then
    raise EBasicGraphError.Create( 'Оба узла дуги должны существовать (не nil).' );

  FUID := uid;
  FObject := AObject;
  FflOwnsObject := flOwnsObject;
  FNodeFrom := nodeFrom;
  FNodeTo := nodeTo;
  FWeight := weight;
  FDirection := direction;

  //сначала нужно создать узлы, потом дугу между ними
  if FNodeFrom.EdgesList.IndexOf( self ) = -1 then
    FNodeFrom.AddEdge( self );
  if FNodeTo.EdgesList.IndexOf( self ) = -1 then
    FNodeTo.AddEdge( self );
end;

{**********************************************************************************************
* TBasicGraphEdge.GetOtherNode
***********************************************************************************************}
function TBasicGraphEdge.GetOtherNode( node: TBasicGraphNode ): TBasicGraphNode;
begin
  Result := nil;

  //мб тут хватит просто проверки на Node_i = node

  if self.FNodeFrom.NodesMatch( node ) then
    Result := self.FNodeTo
  else if Self.FNodeTo.NodesMatch( node ) then
    Result := self.FNodeFrom;
end; 

{**********************************************************************************************
* TBasicGraphEdge.EdgesMatch
* match = true
* else = false
***********************************************************************************************}
function TBasicGraphEdge.EdgesMatch( edge: TBasicGraphEdge ): boolean;
begin
  Result := false;

  if Self.UniqueID <> edge.UniqueID then
    Exit;

  if Self.AObject <> edge.AObject then
    exit;

  if Self.flOwnsObject <> edge.flOwnsObject then
    exit;

  if Self.nodeFrom <> edge.nodeFrom then
    exit;

  if Self.nodeTo <> edge.nodeTo then
    exit;

  if Self.Weight <> edge.Weight then
    exit;

  if Self.Direction <> edge.Direction then
    exit;

  Result := true;
end;

{**********************************************************************************************
* TBasicGraph.SetNodes
***********************************************************************************************}
procedure TBasicGraph.SetNodes( nodesList: Tlist );
begin
  nodesList.Assign( nodesList );
end;

{**********************************************************************************************
* TBasicGraph.SetEdges
***********************************************************************************************}
procedure TBasicGraph.SetEdges( edgesList: Tlist );
begin
  edgesList.Assign( edgesList );
end;

{**********************************************************************************************
* TBasicGraph.Create
* nil, nil - просто инициализируются списк узлов и дуг
* nil, smth - по списку дуг (у каждой дуги ОБЯЗАТЕЛЬНО есть 2 узла) собирается список узлов
* smth, nil - просто записывается список узлов и инициализируется список дуг
* smth, smth - проверяется наличие узлов в дугах из списка дуг в списке узлов
*   если обнаруживается узел, которого нет в списке узлов, поднимается исключение
***********************************************************************************************}
constructor TBasicGraph.Create();
begin
  inherited Create();
end;

{**********************************************************************************************
* TBasicGraph.AddNode
* добавить узел в список узлов, возвращает индекс
***********************************************************************************************}
function TBasicGraph.AddNode( node: TBasicGraphNode ): integer;
begin
  if node = nil then
    raise EBasicGraphError.Create( 'Нельзя добавить пустой узел (nil).' );

  if FNodes = nil then
    raise EBasicGraphError.Create( 'Нельзя добавить узел, т.к. не был инициализирован список узлов.' );

  Result := Self.FindNode( node );
  if Result <> -1 then
    exit;

  Result := FNodes.Add( node );
end;

{**********************************************************************************************
* TBasicGraph.CreateAndAddNode
* создаёт и добавляет узел в список узлов
***********************************************************************************************}
function TBasicGraph.CreateAndAddNode( uid: Integer = -1; assignedObject: TObject = nil; flOwnsObject: Boolean = False;
                            edges: TList  = nil ): integer;
var
  node: TBasicGraphNode;
begin
  node := TBasicGraphNode.Create( uid, assignedObject, flOwnsObject, edges );

  Result := Self.FindNode( node );
  if Result <> -1 then
  begin
    FreeAndNil( node );
    Exit;
  end;

  Result := FNodes.Add( node );
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode
***********************************************************************************************}
procedure TBasicGraph.DeleteNode( node: TBasicGraphNode );
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByIndex
***********************************************************************************************}
procedure TBasicGraph.DeleteNode_ByIndex( index: Integer );
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByUniqueID
***********************************************************************************************}
procedure TBasicGraph.DeleteNode_ByUniqueID( uid: Integer );
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
end;

{**********************************************************************************************
* TBasicGraph.FindNode
***********************************************************************************************}
function TBasicGraph.FindNode( node: TBasicGraphNode ): Integer;
var
  index: integer;
begin
  Result := -1;
  for index := 0 to FNodes.Count - 1 do
    if node.NodesMatch( TBasicGraphNode( FNodes[ index ] ) ) then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
* TBasicGraph.FindNode_ByUniqueID
***********************************************************************************************}
function TBasicGraph.FindNode_ByUniqueID( uid: Integer ): Integer;
var
  index: Integer;
begin
  Result := -1;
  for index := 0 to FNodes.Count - 1 do
    if uid = TBasicGraphNode( FNodes[ index ] ).UniqueID then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
*  TBasicGraph.AddEdge
***********************************************************************************************}
function TBasicGraph.AddEdge( edge: TBasicGraphEdge ): integer;
begin
  if edge = nil then
    raise EBasicGraphError.Create( 'Нельзя добавить пустую дугу (nil).' );

  if FEdges = nil then
    raise EBasicGraphError.Create( 'Нельзя добавить дугу, т.к. список дуг не был инициализирован.' );

  Result := Self.FindEdge( edge );
  if Result <> -1 then
    exit;

  Result := FEdges.Add( edge );
end;

{**********************************************************************************************
* TBasicGraph.CreateAndAddEdge
***********************************************************************************************}
function TBasicGraph.CreateAndAddEdge( nodeFrom, nodeTo: TBasicGraphNode; uid: integer = -1; AObject: TObject = nil;
                                        flOwnsObject: Boolean = false; weight: Double = 1.0;
                                        direction: TBasicGraphEdgeDirection = bgedBidirected ): integer;
var
  edge: TBasicGraphEdge;
begin
  edge := TBasicGraphEdge.Create( nodeFrom, nodeTo, uid, AObject, flOwnsObject, weight, direction );

  Result := Self.FindEdge( edge );
  if Result <> -1 then
  begin
    FreeAndNil( edge );
    Exit;
  end;

  Result := FEdges.Add( edge );
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge
* удалить дугу из списка дуг
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge( edge: TBasicGraphEdge );
var
  index: integer;
begin
  if edge = nil then
    exit;

  if FEdges = nil then
    raise EBasicGraphError.Create( 'Нельзя удалить дугу из графа, т.к. не инициализирован список дуг.' );

  try
    index := FEdges.IndexOf( edge );
    if index >= 0 then
      FEdges.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение об ошибке:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
*  TBasicGraph.DeleteEdge_ByIndex
* удалить дугу из списка, найдя по индексу
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByIndex( index: Integer );
begin    
  if FEdges = nil then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
  try
    if ( index >= 0 ) and ( index < FEdges.Count ) then
      FEdges.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByUniqueID
* удалить дугу из списка, найдя по Unique ID
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByUniqueID( uid: integer );
var
  index: integer;

  function IndexOf_ByUniqueID( EdgesList: TList; uid: integer ): integer;
  begin
    Result := 0;
    //если нет элементов, вернуть -1
    if EdgesList.Count = 0 then
    begin
      Result := -1;
      Exit;
    end;

    while ( Result < EdgesList.Count ) and ( TBasicGraphNode( EdgesList[Result] ).UniqueID <> uid ) do
      Inc( Result );

    //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
    if EdgesList.Count = Result then
      Result := -1;
  end;

begin       
  if FEdges = nil then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
  try
    index := IndexOf_ByUniqueID( self.Edges, uid );
    if index >= 0 then
      FEdges.Delete( index );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.FindEdge
***********************************************************************************************}
function TBasicGraph.FindEdge( edge: TBasicGraphEdge ): integer; 
var
  index: integer;
begin
  Result := -1;
  
  if edge = nil then
    raise EBasicGraphError.Create( 'Невозможно обнаружить пустую дугу (nil).' );

  if FEdges = nil then
    raise EBasicGraphError.Create( 'Невозможно найти дугу, т.к. пуст список дуг (nil).' );

  for index := 0 to FEdges.Count - 1 do
    if edge.EdgesMatch( TBasicGraphEdge( FEdges[ index ] ) ) then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
*TBasicGraph.FindEdge_ByUniqueID
***********************************************************************************************}
function TBasicGraph.FindEdge_ByUniqueID( uid: integer ): Integer;
var
  index: Integer;
begin
  Result := -1;
  for index := 0 to FEdges.Count - 1 do
    if uid = TBasicGraphEdge( FEdges[ index ] ).UniqueID then
    begin
      Result := index;
      Exit;
    end;
end;

end.
