unit mgtGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes, basicGraph;

type
  //�������������� ����
  TMgtGraphEdgeCharacteristic = ( mgtgecHasBus = 0, mgtgecHasTram = 1, mgtgecHasTrolley = 2, mgtgecIsCompensatory = 3 );

  TMgtGraphEdgeCharacteristicArray = set of TMgtGraphEdgeCharacteristic;

  //��� ������� ������ ����� ����� ���-����, �� �� ����� ����� ��, ��� � �������
  TMgtGraphNode = class( TBasicGraphNode );

  //
  TMgtGraphEdge = class( TBasicGraphEdge )
  private
    //������ �� 4� ���������,
    FCharacteristics: Array of Boolean;
  end;
  

implementation

end.
