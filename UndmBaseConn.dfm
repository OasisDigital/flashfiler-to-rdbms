object dmBaseConn: TdmBaseConn
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Left = 192
  Top = 117
  Height = 207
  Width = 372
  object ffClient: TffClient
    Left = 22
    Top = 40
  end
  object ffDB: TffDatabase
    SessionName = '[automatic]'
    Left = 176
    Top = 36
  end
  object ffSess: TffSession
    ClientName = '[automatic]'
    Left = 106
    Top = 40
  end
  object ffTRequest: TffTable
    FieldDefs = <>
    ReadOnly = True
    SessionName = '[automatic]'
    Left = 200
    Top = 120
  end
end
