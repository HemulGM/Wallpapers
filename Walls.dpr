program Walls;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Wall.Main in 'Wall.Main.pas' {FormMain},
  REST.Handler in '..\RESTHandler\REST.Handler.pas',
  Wall.AlphaCoders.API in '..\Wall.AlphaCoders.API\Wall.AlphaCoders.API.pas',
  Wall.View.Image in 'Wall.View.Image.pas' {FrameImage: TFrame};

{$R *.res}

begin
  //GlobalUseGPUCanvas := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
