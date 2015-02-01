{ 25/05/2007 11:14:27 (GMT+1:00) > [Akadamia] checked in   }
{ 08/02/2007 14:10:57 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:10:56 (GMT+0:00) > [Akadamia] checked in   }
{ 15/01/2007 13:29:11 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 15/01/2007 13:29:08 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 11/01/2007 15:18:15 (GMT+0:00) > [ken.adam on GLKC2353537] checked in   }
{ 11/01/2007 15:14:14 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
{ 11/01/2007 15:14:10 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
program FacilitiesData;

uses
  Forms,
  FDU in 'FDU.pas' {FacilitiesDataForm},
  SimConnectSupport in '..\HeaderRT\SimConnectSupport.pas',
  SimConnect in '..\HeaderRT\SimConnect.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFacilitiesDataForm, FacilitiesDataForm);
  Application.Run;
end.
