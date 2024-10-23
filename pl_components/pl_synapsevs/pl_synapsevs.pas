{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_synapsevs;

{$warn 5023 off : no warning about unused units}
interface

uses
  allsynapsevsregister, vsAuthentication, vsComPort, vsComPortbase, 
  vsComPortsetup, vsExecCGI, vsFileLogger, vsFtpServer, vsHttpServer, 
  vsPastella, vsping, vsSmtpServer, vsTcpServer, vsTypeDef, 
  vsVisualServerBase, vswebclient, vswebsocket, vsXMailer, vsGeoIP, 
  vsGeoIPBase, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allsynapsevsregister', @allsynapsevsregister.Register);
end;

initialization
  RegisterPackage('pl_synapsevs', @Register);
end.
