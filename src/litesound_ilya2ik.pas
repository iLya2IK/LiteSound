{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit litesound_ilya2ik;

{$warn 5023 off : no warning about unused units}
interface

uses
  OGLSoundLite, OGLRIFFWaveWrapper, PlayerControls, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('litesound_ilya2ik', @Register);
end.
