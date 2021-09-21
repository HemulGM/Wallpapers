program Walls;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Wall.Main in 'Wall.Main.pas' {FormMain},
  Wall.View.Image in 'Wall.View.Image.pas' {FrameImage: TFrame},
  REST.Handler in 'REST.Handler.pas',
  Wall.AlphaCoders.API in 'Wall.AlphaCoders.API.pas';

{$R *.res}

begin
  //GlobalUseDX := False;
  GlobalUseGDIPlusClearType := True;
  //GlobalUseDirect2D := True;
  //GlobalUseGPUCanvas := True;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
