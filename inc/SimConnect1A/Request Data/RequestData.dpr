{ 08/02/2007 14:08:36 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:08:35 (GMT+0:00) > [Akadamia] checked in   }
{ 11/01/2007 15:18:15 (GMT+0:00) > [ken.adam on GLKC2353537] checked in   }
{ 11/01/2007 15:14:14 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
{ 11/01/2007 15:14:10 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
program RequestData;

uses
  Forms,
  RDU in 'RDU.pas' {RequestDataForm},
  SimConnect in '..\HeaderRT\SimConnect.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRequestDataForm, RequestDataForm);
  Application.Run;
end.
