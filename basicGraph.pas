unit basicGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes;

type
  EBasicGraphError = class(Exception);

  TBasicGraphDirection = ( bgdFrom = 0, bgdTo = 1 );

  TBasicGraphDirectionArray = set of TBasicGraphDirection;

  TBasicGraphDeletionMode = ( bgdmAllowOnlyTarget = 0, bgdmAllowTargetAndEdges = 1 );

  TBasicGraphEdge = class;

  TEdgeStruct = class
  private
    FEdge: TBasicGraphEdge;
    FCost: Double;
  public

    constructor Create( AEdge: TBasicGraphEdge; ACost: Double ); reintroduce;

    property Edge: TBasicGraphEdge read FEdge write FEdge;
    property Cost: Double read FCost write FCost;
  end;

  TEdgesQueueWithPriority = class
  private
    FQueue: TList;
  public
    constructor Create();
    function IsEmpty(): Boolean;
    function GetElement(): TEdgeStruct;
    //Добавить элемент в сортированый список
    procedure AddElement( AEdgeRecord: TEdgeStruct );
    destructor Destroy; override;
  end;

  //БАЗОВЫЙ КЛАСС - ОПИСАНИЕ УЗЛА ГРАФА
  TBasicGraphNode = class( TObject )
  private
    FIndex: Integer;
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

    //вспомогательные процедуры для удаления дуги и связанных с ней соседей
    //удаляет входящую дугу и её узел From, если нужно
    procedure DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
    //удаляет вsходящую дугу и её узел To, если нужно
    procedure DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
    //удалить дугу из обоих списков дуг
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //удалить дугу из обоих списков дуг, найдя её по UID
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //удалить дугу из обоих списков дуг, найдя её по объекту
    procedure DeleteEdge_ByObject( AObject: TObject );

    //индекс узла-соседа в списке соседей To/From по UID
    function IndexOfNeighbour_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
    //индекс узла-соседа в списке соседей To/From по объекту
    function IndexOfNeighbour_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;

  public
    constructor Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //ПРОВЕРКА НАЛИЧИЯ УЗЛА-СОСЕДА
    //проверить наличие узла-соседа в списках соседей From/To/From&To
    function HasNeighbour( ANode: TBasicGraphNode; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие узла-соседа в списках соседей From/To/From&To по UID
    function HasNeighbour_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие узла-соседа в списках соседей From/To/From&To по ассоциированному объекту
    function HasNeighbour_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;

    //РАБОТА С УЗЛАМИ-СОСЕДЯМИ
    //возвращает количество узлов-соседей в списке From/To
    function GetNeighboursCount( AListDirection: TBasicGraphDirection ): Integer;
    //возвращает дугу по её индексу в списке From/To
    function GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetNeighbour_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetNeighbour_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TBasicGraphNode;

    //----------------------------------------------------------------------------------------------------------------

    //индекс дуги в списке дуг To/From по UID
    function IndexOfEdge_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
    //индекс дуги в списке дуг To/From по объекту
    function IndexOfEdge_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;

    //ПРОВЕРКА НАЛИЧИЯ ДУГИ
    //проверить наличие дуги в списках дуг From/To/From&To
    function HasEdge( AEdge: TBasicGraphEdge; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие дуги в списках дуг From/To/From&To по UID
    function HasEdge_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //проверить наличие дуги в списках дуг From/To/From&To по ассоциированному объекту
    function HasEdge_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;

    //РАБОТА С ДУГАМИ
    //возвращает количество дуг в списке From/To
    function GetEdgesCount( AListDirection: TBasicGraphDirection ): Integer;
    //возвращает дугу по её индексу в списке From/To
    function GetEdge_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
    //возвращает дугу по её UID, ищет в списках From/To/From&To
    function GetEdge_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
    //возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
    function GetEdge_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;

    //----------------------------------------------------------------------------------------------------------------

    //сеттеры
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeIndex: Integer read FIndex;
  end;

  //БАЗОВЫЙ КЛАСС - ОПИСАНИЕ ДУГИ ГРАФА
  TBasicGraphEdge = class( TObject )
  private
    FIndex: Integer;
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
    constructor Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
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
    property EdgeIndex: Integer read FIndex;
  end;

  TBasicGraph = class( TObject )
  private
    FHighestNodeIndexValue: Integer;
    FHIghestEdgeIndexValue: Integer;
    //Хэш-контейнер, хранящий узел по UID. Обладает своими объектами.
    FNodes_ByUID: THashContainer;
    //Хэш-контейнер, хранящий узел по ассоциированному объекту. Не обладает своими объектами.
    FNodes_ByObject: THashContainer;
    //Хэш-контейнер, хранящий дугу по UID. Обладает своими объектами.
    FEdges_ByUID: THashContainer;
    //Хэш-контейнер, хранящий дугу по ассоциированному объекту. Обладает своими объектами.
    FEdges_ByObject: THashContainer;

  public
    constructor Create(); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //ДОБАВЛЕНИЕ УЗЛА
    //Добавить узел в список узлов, возвращает объект
    function AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;

    //УДАЛЕНИЕ УЗЛА
    //Удалить узел. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteNode( ANode: TBasicGraphNode;
                          ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //Удалить узел по UID. Возвращает True при удачном удалении, False при невозможности удаления
    function DeleteNode_ByUID( AUID: Int64;
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
    function AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AWeight: Double = 1.0;
                      AObject: TObject = nil; AflOwnsObject: Boolean = false;
                      AflBiDirected: Boolean = false ): TBasicGraphEdge;

    //УДАЛЕНИЕ ДУГИ
    //Удалить дугу.
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //Удалить дугу по UID.
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //Удалить дугу по объекту.
    procedure DeleteEdge_ByObject( AObject: TObject );

    //ПОИСК ДУГИ
    //получние дуги по её UID
    function GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
    //получение дуги по её объекту
    function GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;

    //------------------------------------------------------------------------------------------------------------------

    //добавить узел на дугу (SPLIT)
    function AddNode_ToEdge( AEdge: TBasicGraphEdge; ANode: TBasicGraphNode; AEdge_NewWeight: Double = 1.0;
                              ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                              ANewEdge_flOwnsObject: Boolean = false ):
                              Boolean;

    //delete and merge работает только для узлов с 1 входящей дугой и 1 выходящей дугой
    //удалить узел и объединить входящую и выходящую дуги в одну.
    function DeleteNode_AndMergeEdges( ANode: TBasicGraphNode ): Boolean;
    //Удалить узел по UID и объединить входящую и выходящую дуги в одну.
    function DeleteNode_AndMergeEdges_ByUID( AUID: Int64 ): Boolean;
    //Удалить узел по объекту и объединить входящую и выходящую дуги в одну.
    function DeleteNode_AndMergeEdges_ByObject( AObject: TObject ): Boolean;

    //найти путь из точки nodeFrom в точку nodeTo
    function FindPath(  nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
  end;

implementation

constructor TEdgeStruct.Create( AEdge: TBasicGraphEdge; ACost: Double );
begin
  FEdge := AEdge;
  FCost := ACost;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Create
***********************************************************************************************}
constructor TEdgesQueueWithPriority.Create();
begin
  FQueue := TList.Create();
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.IsEmpty
***********************************************************************************************}
function TEdgesQueueWithPriority.IsEmpty(): Boolean;
begin
  Result := false;
  if ( FQueue.Count = 0 ) then
  begin
    Result := true;
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.GetElement
***********************************************************************************************}
function TEdgesQueueWithPriority.GetElement(): TEdgeStruct;
begin
  Result := nil;
  if FQueue.Count > 0 then
  begin
    Result := FQueue[ 0 ];
    FQueue.Delete( 0 );
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.AddElement
* Добавить элемент в сортированый список
***********************************************************************************************}
procedure TEdgesQueueWithPriority.AddElement( AEdgeRecord: TEdgeStruct );
var
  startIndex, endIndex, position: integer;
  edgeRecord: TEdgeStruct;
begin
  if ( FQueue.Count = 0 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  // Если самый маленький
  edgeRecord:= FQueue[0];
  if ( edgeRecord.Cost >= AEdgeRecord.Cost ) then
  begin
    FQueue.Insert( 0, AEdgeRecord );
    Exit;
  end;

  // Если 1 элемент в списке
  if ( FQueue.Count = 1 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  startIndex := 0;
  endIndex := FQueue.Count - 1;
  while ( ( endIndex - startIndex ) > 1 ) do
  begin
    position := ( endIndex + startIndex ) div 2; //<---
    edgeRecord := FQueue[ position ];
    if ( edgeRecord.Cost = AEdgeRecord.Cost ) then
    begin
      FQueue.Insert( position, AEdgeRecord );
      Exit;
    end;

    if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
      endIndex:= position;
    if ( edgeRecord.Cost < AEdgeRecord.Cost ) then
      startIndex:= position;
  end;
  edgeRecord:= FQueue[ startIndex ];
  if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
    FQueue.Insert( startIndex, AEdgeRecord )
  else
    FQueue.Insert( endIndex, AEdgeRecord );
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Destroy
***********************************************************************************************}
destructor TEdgesQueueWithPriority.Destroy();
begin
  FreeAndNil( FQueue );
end;

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
constructor TBasicGraphNode.Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );
begin
  inherited Create();

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;

  FEdgesList_To := TList.Create();
  FEdgesList_From := TList.Create();
  FNeighboursList_To := TList.Create();
  FNeighboursList_From := TList.Create();
end;

{**********************************************************************************************
* TBasicGraphNode.Destroy
***********************************************************************************************}
destructor TBasicGraphNode.Destroy();
begin
  FreeAndNil( FEdgesList_To );
  FreeAndNil( FEdgesList_From );
  FreeAndNil( FNeighboursList_To );
  FreeAndNil( FNeighboursList_From );

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
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

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;

  FEdgesList_To.Delete(index);
  flDeleteNode := true;

  //теперь надо проверить, перестал ли быть соседом узел, из которого исходила дуга
  for index := 0 to FEdgesList_To.Count - 1 do
    if TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom = NodeFrom then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_To.IndexOf( NodeFrom );
    if index >= 0 then
      FNeighboursList_To.Delete( index );
  end;
end;

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;
    
  FEdgesList_From.Delete(index);
  flDeleteNode := true;

  //теперь надо проверить, перестал ли быть соседом узел, в который входила дуга
  for index := 0 to FEdgesList_From.Count - 1 do
    if TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo = NodeTo then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_From.IndexOf( NodeTo );
    if index >= 0 then
      FNeighboursList_From.Delete( index );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge( AEdge: TBasicGraphEdge );
var
  index: Integer;
begin
  if AEdge = nil then
    raise EBasicGraphError.Create( 'Невозможно удалить дугу, поскольку она нулевая (nil).' );

  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
  try
    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, входящих в узел
    index := FEdgesList_To.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, AEdge.NodeFrom );

    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, выходящих из узла
    index := FEdgesList_From.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, AEdge.NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение об ошибке:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByUID
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByUID( AUID: Int64 );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
    
  try
    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, входящих в узел
    index := IndexOfEdge_ByUID( bgdTo, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, выходящих из узла
    index := IndexOfEdge_ByUID( bgdFrom, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByObject
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByObject( AObject: TObject );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( 'Не инициализирован список дуг, в которые входит узел.' );
    
  try
    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, входящих в узел
    index := IndexOfEdge_ByObject( bgdTo, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //найти и удалить дугу (а при необходимости, удалить и узлы-соседи) в списке дуг, выходящих из узла
    index := IndexOfEdge_ByObject( bgdFrom, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить дугу из списка дуг.' + sLineBreak + 'Сообщение ошибки:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour
* проверить наличие узла-соседа в списках соседей From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour( ANode: TBasicGraphNode; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( FNeighboursList_To.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( FNeighboursList_From.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = bgdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = bgdFrom then
    aNeighboursList := FNeighboursList_From;

  //если нет элементов или список пуст, вернуть -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = bgdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = bgdFrom then
    aNeighboursList := FNeighboursList_From;

  //если нет элементов или список пуст, вернуть -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByUID
*проверить наличие узла-соседа в списках соседей From/To/From&To по UID
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfNeighbour_ByUID( bgdTo, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByUID( bgdFrom, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByObject
* проверить наличие узла-соседа в списках соседей From/To/From&To по ассоциированному объекту
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
    raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfNeighbour_ByObject( bgdTo, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByObject( bgdFrom, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighboursCount
* возвращает количество узлов-соседей в списке From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighboursCount( AListDirection: TBasicGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = bgdTo then
    Result := FNeighboursList_To.Count
  else if AListDirection = bgdFrom then
    Result := FNeighboursList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByIndex
* возвращает дугу по её индексу в списке From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = bgdTo )
      AND ( AIndex >= FNeighboursList_To.Count ) )
  OR ( ( AListDirection = bgdFrom )
      AND ( AIndex >= FNeighboursList_From.Count ) ) then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ AIndex ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByUID
* возвращает дугу по её UID, ищет в списках From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByObject   
* возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge
* проверить наличие дуги в списках дуг From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasEdge( AEdge: TBasicGraphEdge; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( FEdgesList_To.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( FEdgesList_From.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = bgdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = bgdFrom then
    aEdgesList := FEdgesList_From;

  //если нет элементов или список пуст, вернуть -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = bgdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = bgdFrom then
    aEdgesList := FEdgesList_From;

  //если нет элементов или список пуст, вернуть -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //если дошли до конца и не нашли грань (result = count), тоже вернуть -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByUID
* проверить наличие дуги в списках дуг From/To/From&To по UID
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfEdge_ByUID( bgdTo, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfEdge_ByUID( bgdFrom, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByObject
* проверить наличие дуги в списках дуг From/To/From&To по ассоциированному объекту
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( 'Не инициализированы списки соседей узла (nil).' );

  //если в списке есть направление "To", то проверим наличие узла в списке NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfEdge_ByObject( bgdTo, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //если в списке есть направление "From", то проверим наличие узла в списке NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfEdge_ByObject( bgdFrom, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdgesCount
* возвращает количество дуг в списке From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdgesCount( AListDirection: TBasicGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = bgdTo then
    Result := FEdgesList_To.Count
  else if AListDirection = bgdFrom then
    Result := FEdgesList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByIndex
* возвращает дугу по её индексу в списке From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = bgdTo )
      AND ( AIndex >= FEdgesList_To.Count ) )
  OR ( ( AListDirection = bgdFrom )
      AND ( AIndex >= FEdgesList_From.Count ) ) then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ AIndex ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByUID
* возвращает дугу по её UID, ищет в списках From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByObject
* возвращает дугу по ассоциированному с ней объекту, ищет в списках From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

//----------------------------------------------------------------------------------------------------------------------
//СЕКЦИЯ ОПИСАНИЯ ДУГИ
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
procedure TBasicGraphEdge.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  Self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeFrom( ANewNodeFrom: TBasicGraphNode );
begin
  Self.FNodeFrom := ANewNodeFrom;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeTo( ANewNodeTo: TBasicGraphNode );
begin
  Self.FNodeTo := ANewNodeTo;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetDirection
***********************************************************************************************}
procedure TBasicGraphEdge.SetflBiDirected( AflBiDirected: Boolean );
begin
  Self.FflBiDirected := AflBiDirected;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetWeight
***********************************************************************************************}
procedure TBasicGraphEdge.SetWeight( AWeight: double );
begin
  Self.FWeight := AWeight;
end;

{**********************************************************************************************
* TBasicGraphEdge.Create
***********************************************************************************************}
constructor TBasicGraphEdge.Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1;
                                    AObject: TObject = nil; AflOwnsObject: Boolean = false;
                                    AflBiDirected: Boolean = false; AWeight: Double = 1.0 );
begin
  inherited Create();

  if ( ANodeFrom = nil ) or ( ANodeTo = nil ) then
    raise EBasicGraphError.Create( 'Оба узла дуги должны существовать (не nil).' );

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;
  FNodeFrom := ANodeFrom;
  FNodeTo := ANodeTo;
  FWeight := AWeight;
  FflBiDirected := AflBiDirected;

  //метод AddEdge сам определит куда и как надо добавить дугу
  FNodeFrom.AddEdge( Self );
  FNodeTo.AddEdge( Self );
end;

{**********************************************************************************************
* TBasicGraphEdge.Destroy
***********************************************************************************************}
destructor TBasicGraphEdge.Destroy();
begin
  FNodeTo := nil;
  FNodeFrom := nil;

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
end;

//----------------------------------------------------------------------------------------------------------------------
//СЕКЦИЯ ОПИСАНИЯ ГРАФА
{**********************************************************************************************
* TBasicGraph.Create
***********************************************************************************************}
constructor TBasicGraph.Create();
begin
  inherited Create();

  FHighestNodeIndexValue := 0;
  FHIghestEdgeIndexValue := 0;

  FNodes_ByUID := THashContainer.Create( True );
  FNodes_ByObject := THashContainer.Create( );
  FEdges_ByUID := THashContainer.Create( True );
  FEdges_ByObject := THashContainer.Create( );
end;

destructor TBasicGraph.Destroy();
begin
  FreeAndNil( FNodes_ByUID );
  FreeAndNil( FNodes_ByObject );
  FreeAndNil( FEdges_ByUID );
  FreeAndNil( FEdges_ByObject );

  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraph.AddNode
* добавить узел в список узлов, возвращает индекс
***********************************************************************************************}
function TBasicGraph.AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;
var
  node: TBasicGraphNode;
begin
  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EBasicGraphError.Create( 'Невозможно добавить узел в несуществующий (nil) список узлов.' );

  try
    node := TBasicGraphNode.Create( AUID, FHighestNodeIndexValue, AObject, AflOwnsObject );
    FHighestNodeIndexValue := FHighestNodeIndexValue + 1;

    FNodes_ByUID.AddObject( node.UID, node, true );
    if AObject <> nil then
      FNodes_ByObject.AddObject( Integer( node.Object_ ), node, true );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось добавить узел. Сообщение ошибки:' + sLineBreak + E.Message );
  end;

  Result := node;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode
***********************************************************************************************}
function TBasicGraph.DeleteNode( ANode: TBasicGraphNode;
                                 ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
  Result := false;

  if ( ANode.GetEdgesCount( bgdTo ) + ANode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //удаляем из графа дуги, входящие в узел
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //удаляем из графа дуги, выходящие из узла
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //удаляем из графа узел
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByUID
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByUID( AUID: Int64;
                                       ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
  Result := false;

  aNode := GetNode_ByUID( AUID );

  if ( aNode.GetEdgesCount( bgdTo ) + aNode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //удаляем из графа дуги, входящие в узел
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //удаляем из графа дуги, выходящие из узла
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //удаляем из графа узел
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByObject
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByObject( AObject: TObject;
                                       ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //раз удаляется узел, то наверно надо удалить и дуги, в которые он входит
  Result := false;

  aNode := GetNode_ByObject( AObject );

  if ( aNode.GetEdgesCount( bgdTo ) + aNode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //удаляем из графа дуги, входящие в узел
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //удаляем из графа дуги, выходящие из узла
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //удаляем из графа узел
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetNode_ByUID
* получние узла по его UID
***********************************************************************************************}
function TBasicGraph.GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
begin
  Result := TBasicGraphNode( FNodes_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.FindNode_ByUniqueID
***********************************************************************************************}
function TBasicGraph.GetNode_ByObject( AObject: TObject ): TBasicGraphNode;
begin
  Result := TbasicGraphNode( FNodes_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
*  TBasicGraph.AddEdge
***********************************************************************************************}
function TBasicGraph.AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AWeight: Double = 1.0;
                      AObject: TObject = nil; AflOwnsObject: Boolean = false;
                      AflBiDirected: Boolean = false ): TBasicGraphEdge;
var
  edge: TBasicGraphEdge;
begin
  if ( FEdges_ByUID = nil )
  OR ( FEdges_ByObject = nil) then
    raise EBasicGraphError.Create( 'Невозможно добавить дугу в несуществующий (nil) список дуг.' );

  try
    edge := TBasicGraphEdge.Create( ANodeFrom, ANodeTo, AUID, FHighestEdgeIndexValue, AObject, AflOwnsObject, AflBiDirected, AWeight );
    FHighestEdgeIndexValue := FHighestEdgeIndexValue + 1;

    FEdges_ByUID.AddObject( edge.UID, edge, true );
    if AObject <> nil then
      FEdges_ByObject.AddObject( Integer( edge.Object_ ), edge, true );
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось добавить дугу. Сообщение ошибки:' + sLineBreak + E.Message );
  end;

  Result := edge;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge
* удалить дугу из списка дуг
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge( AEdge: TBasicGraphEdge );
begin
  try
    //удаление дуги из списков дуг, связанных с узлами To и From этой дуги
    AEdge.NodeTo.DeleteEdge( AEdge );
    AEdge.NodeFrom.DeleteEdge( AEdge );

    //удаление дуги из списков дуг графа
    FEdges_ByUID.DelObject( AEdge.UID, AEdge );
    FEdges_ByObject.DelObject( Integer( AEdge.Object_ ), AEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByUID
* удалить дугу из списка, найдя по Unique ID
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByUID( AUID: Int64 );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByUID( AUID );

  try
    //удаление дуги из списков дуг, связанных с узлами To и From этой дуги
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //удаление дуги из списков дуг графа
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByObject
* Удалить дугу по объекту
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByObject( AObject: TObject );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByObject( AObject );

  try
    //удаление дуги из списков дуг, связанных с узлами To и From этой дуги
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //удаление дуги из списков дуг графа
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( 'Не удалось удалить узел. Сообщение ошибки: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByUID
* получние дуги по её UID
***********************************************************************************************}
function TBasicGraph.GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByObject
* получение дуги по её объекту
***********************************************************************************************}
function TBasicGraph.GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
* TBasicGraph.AddNode_ToEdge
***********************************************************************************************}
function TBasicGraph.AddNode_ToEdge( AEdge: TBasicGraphEdge; ANode: TBasicGraphNode; AEdge_NewWeight: Double = 1.0;
                                      ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                                      ANewEdge_flOwnsObject: Boolean = false ):
                                      Boolean;
var
  newEdge: TBasicGraphEdge;
  newEdge_Weight: Double;
begin
  Result := False;

  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EBasicGraphError.Create( 'Невозможно добавить узел в несуществующий (nil) список узлов.' );

  newEdge_Weight := AEdge.Weight - AEdge_NewWeight;
  if ( newEdge_Weight >= AEdge.Weight )
  OR ( newEdge_Weight <= 0.0 ) then
    Exit;

  try
    AEdge.NodeTo.DeleteEdge( AEdge );
    newEdge := AddEdge( ANode, AEdge.NodeTo, ANewEdge_UID, newEdge_Weight, ANewEdge_Object, ANewEdge_flOwnsObject,
                        AEdge.FlBiDirected );
    AEdge.NodeTo := ANode;
    ANode.AddEdge( AEdge );    
    AEdge.Weight := AEdge_NewWeight;
  except
    on E:Exception do
      raise EBasicGraphError.Create( 'Не удалось добавить узел. Сообщение ошибки:' + sLineBreak + E.Message );
  end;

  Result := True;

end;

{**********************************************************************************************
* удалить узел и объединить входящую и выходящую дуги в одну.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges( ANode: TBasicGraphNode ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
begin
  Result := false;

  //если у узла не 1 входящая или не 1 выходящая или взодящая = выходящая дуга, то выйти и вернуть false
  if ( ANode.GetEdgesCount( bgdTo ) <> 1 )
    or ( ANode.GetEdgesCount( bgdFrom ) <> 1 )
    or ( ANode.GetEdge_ByIndex( 0, bgdTo ) = ANode.GetEdge_ByIndex(0, bgdFrom ) )
  then
    Exit;

  //получаем левую и правую дуги
  edgeTo := ANode.GetEdge_ByIndex(0, bgdTo);
  edgeFrom := ANode.getEdge_ByIndex(0, bgdFrom);
  //изменяем параметры левой дуги, в том числе меняем её nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //удалим данные о дуге из удаляемого узла, чтобы он её не удалил
  ANode.DeleteEdge( edgeTo );
  //удалим узел - он удалит выходящую дугу самостоятельно
  DeleteNode( ANode );
  //добавляем левую дугу в список в правом узле
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
* Удалить узел по UID и объединить входящую и выходящую дуги в одну.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges_ByUID( AUID: Int64 ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
  aNode: TBasicGraphNode;
begin
  Result := false;

  aNode := GetNode_ByUID( AUID );

  //если у узла не 1 входящая или не 1 выходящая или взодящая = выходящая дуга, то выйти и вернуть false
  if ( aNode.GetEdgesCount( bgdTo ) <> 1 )
    or ( aNode.GetEdgesCount( bgdFrom ) <> 1 )
    or ( aNode.GetEdge_ByIndex( 0, bgdTo ) = aNode.GetEdge_ByIndex(0, bgdFrom ) )
  then
    Exit;

  //получаем левую и правую дуги
  edgeTo := aNode.GetEdge_ByIndex(0, bgdTo);
  edgeFrom := aNode.getEdge_ByIndex(0, bgdFrom);
  //изменяем параметры левой дуги, в том числе меняем её nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //удалим данные о дуге из удаляемого узла, чтобы он её не удалил
  aNode.DeleteEdge( edgeTo );
  //удалим узел - он удалит выходящую дугу самостоятельно
  DeleteNode( aNode );
  //добавляем левую дугу в список в правом узле
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
* Удалить узел по объекту и объединить входящую и выходящую дуги в одну.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges_ByObject( AObject: TObject ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
  aNode: TBasicGraphNode;
begin
  Result := false;

  aNode := GetNode_ByObject( AObject );

  //если у узла не 1 входящая или не 1 выходящая или взодящая = выходящая дуга, то выйти и вернуть false
  if ( aNode.GetEdgesCount( bgdTo ) <> 1 )
    or ( aNode.GetEdgesCount( bgdFrom ) <> 1 )
    or ( aNode.GetEdge_ByIndex( 0, bgdTo ) = aNode.GetEdge_ByIndex(0, bgdFrom ) )
  then
    Exit;

  //получаем левую и правую дуги
  edgeTo := aNode.GetEdge_ByIndex(0, bgdTo);
  edgeFrom := aNode.getEdge_ByIndex(0, bgdFrom);
  //изменяем параметры левой дуги, в том числе меняем её nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //удалим данные о дуге из удаляемого узла, чтобы он её не удалил
  aNode.DeleteEdge( edgeTo );
  //удалим узел - он удалит выходящую дугу самостоятельно
  DeleteNode( aNode );
  //добавляем левую дугу в список в правом узле
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
* TBasicGraph.FindPath
***********************************************************************************************}
function TBasicGraph.FindPath( nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
var
  distanceFromSource: array of Double;
  incomingEdge: array of TBasicGraphEdge;
  queuedEdges: TEdgesQueueWithPriority;
  index: integer;
  currentNode: TBasicGraphNode;
  queuedEdge, currentEdge: TBasicGraphEdge;
  edgeStruct: TEdgeStruct;
  currentDistanceFromSource: Double;

  procedure AddStartEdgesToArray( startNode: TBasicGraphNode );
  var
    index: Integer;
    edge: TBasicGraphEdge;
  begin
    for index := 0 to startNode.GetEdgesCount( bgdFrom ) - 1 do
    begin
      edge := startNode.GetEdge_ByIndex( index, bgdFrom );
      distanceFromSource[ edge.NodeTo.NodeIndex ] := edge.Weight;
      incomingEdge[ edge.NodeTo.NodeIndex ] := edge;
      queuedEdges.AddElement( TEdgeStruct.Create( edge, edge.Weight ) );
    end;
  end;

begin
  if nodeFrom = nodeTo then
  begin
    Result := 0;
    exit;
  end;

  Result := -1;
  try
    SetLength( distanceFromSource, FNodes_ByUID.ObjectCount );
    SetLength( incomingEdge, FNodes_ByUID.ObjectCount );
    queuedEdges := TEdgesQueueWithPriority.Create();
    //-------------------------------------------------------------------------------------------------------------------
    //ИНИЦИАЛИЗАЦИЯ
    for index := 0 to FNodes_ByUID.ObjectCount - 1 do
    begin
      distanceFromSource[ index ] := -1.0;
      incomingEdge[ index ] := nil;
    end;

    //расстояние от Source до Source = 0
    //список упорядочиваем, на первое место ставим элемент, ближайший к Source - сейчас это он сам
    distanceFromSource[ nodeFrom.NodeIndex ] := 0;
    incomingEdge[ nodeFrom.NodeIndex ] := nil;
    AddStartEdgesToArray( nodeFrom );

    //-------------------------------------------------------------------------------------------------------------------
    //ПРЯМОЙ ХОД АЛГОРИТМА
    while ( not queuedEdges.IsEmpty() ) do
    begin
      edgeStruct := queuedEdges.GetElement();
      if ( edgeStruct.Cost <> distanceFromSource[ edgeStruct.Edge.NodeTo.NodeIndex ] ) then
      begin
        edgeStruct.Edge := nil;
        FreeAndNil( edgeStruct );
        continue;
      end;

      queuedEdge := edgeStruct.Edge;
      currentNode := queuedEdge.NodeTo;
      for index := 0 to currentNode.GetEdgesCount( bgdFrom ) - 1 do
      begin
        currentEdge := currentNode.GetEdge_ByIndex( index, bgdFrom );
        currentDistanceFromSource := distanceFromSource[ currentNode.NodeIndex ] + currentEdge.Weight;
        if ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] < 0 )
          OR ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] > currentDistanceFromSource ) then
        begin
          distanceFromSource[ currentEdge.NodeTo.NodeIndex ] := currentDistanceFromSource;
          incomingEdge[ currentEdge.NodeTo.NodeIndex ] := currentEdge;
          queuedEdges.AddElement( TEdgeStruct.Create( currentEdge, currentDistanceFromSource ) );
        end;
      end;    
      FreeAndNil( edgeStruct );
    end;
    FreeAndNil( queuedEdges );

    //-------------------------------------------------------------------------------------------------------------------
    //ОБРАТНЫЙ ХОД АЛГОРИТМА
    currentEdge := incomingEdge[ nodeTo.NodeIndex ];
    if currentEdge <> nil then
      Result := 0;
      
    while currentEdge <> nil do
    begin
      path.Insert( 0, currentEdge );
      Result := Result + currentEdge.Weight;
      currentEdge := incomingEdge[ currentEdge.NodeFrom.NodeIndex ];
    end;
  finally
    FreeAndNil( queuedEdges );
  end;
end;

end.
