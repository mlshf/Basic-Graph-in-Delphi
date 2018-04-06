unit mgtGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes, basicGraph;

type
  //характеристики дуги
  TMgtGraphEdgeCharacteristic = ( mgtgecHasBus = 0, mgtgecHasTram = 1, mgtgecHasTrolley = 2, mgtgecIsCompensatory = 3 );

  TMgtGraphEdgeCharacteristicArray = set of TMgtGraphEdgeCharacteristic;

  //для порядка вводим новый класс МГТ-узла, но он точно такой же, как и базовый
  TMgtGraphNode = class( TBasicGraphNode );

  //
  TMgtGraphEdge = class( TBasicGraphEdge )
  private
    //массив из 4х элементов,
    FCharacteristics: Array of Boolean;
  end;
  

implementation

end.
