unit librarytestusbmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ctutils,
  ExtCtrls, ComCtrls, LibUsbDyn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure Button1Click(Sender: TObject);
  private
    procedure TestLibFunctions;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

Function PathLibrary:string;
begin
{$IFDEF Windows}
  Result := ctGetRuntimesCPUOSDir(true);
{$ELSE}
  Result := '';
{$ENDIF}
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
 var xOK:boolean;
     ss:String;
begin

 Memo1.Clear;

  ss:='';
  xOK:=USBLib_Init;

  if xOK=false then
  begin
    ss:=PathLibrary+cnlibusbFile;
    xOK:=USBLib_Init(PathLibrary+cnlibusbFile);
  end;

  if xOK=false then
  begin
   Label1.Caption:='ERROR: '+ss+' NOT Loaded';
   exit;
  end else
  begin
   Label1.Caption:=ss+' Loaded OK';
  end;

  TestLibFunctions;

end;

procedure TForm1.TestLibFunctions;  
//................................................
procedure _writetostrings(const isok:boolean;const astr:string);
begin
  case RadioGroup1.ItemIndex of
   0:begin
      Memo1.Lines.add(astr);
      exit;
   end;
  1: begin
     if isok=false then
       Memo1.Lines.add(astr);
     exit;
   end;

   2:begin
     if isok=True then
       Memo1.Lines.add(astr);
     exit;
     end;

  end;
end;  
//.................................................
procedure _CheckVar(aaa:Pointer; Const aname:string);
 begin
  if aaa=nil then
    _writetostrings(false,aname+'          is NIL ????????') else
    _writetostrings(true ,aname+'          Is OK');
 end;
//.................................................
begin
  Memo1.Lines.add('');
                             
  _CheckVar(libusb_init,'libusb_init');
  _CheckVar(libusb_exit,'libusb_exit');
  _CheckVar(libusb_set_debug,'libusb_set_debug');
  _CheckVar(libusb_get_version,'libusb_get_version');
  _CheckVar(libusb_has_capability,'libusb_has_capability');
  _CheckVar(libusb_error_name,'libusb_error_name');
  _CheckVar(libusb_get_device_list,'libusb_get_device_list');
  _CheckVar(libusb_free_device_list,'libusb_free_device_list');
  _CheckVar(libusb_ref_device,'libusb_ref_device');
  _CheckVar(libusb_unref_device,'libusb_unref_device');
  _CheckVar(libusb_get_configuration,'libusb_get_configuration');
  _CheckVar(libusb_get_device_descriptor,'libusb_get_device_descriptor');
  _CheckVar(libusb_get_active_config_descriptor,'libusb_get_active_config_descriptor');
  _CheckVar(libusb_get_config_descriptor,'libusb_get_config_descriptor');
  _CheckVar(libusb_get_config_descriptor_by_value,'libusb_get_config_descriptor_by_value');
  _CheckVar(libusb_free_config_descriptor,'libusb_free_config_descriptor');
  _CheckVar(libusb_get_bus_number,'libusb_get_bus_number');
  _CheckVar(libusb_get_port_number,'libusb_get_port_number');
  _CheckVar(libusb_get_parent,'libusb_get_parent');
  _CheckVar(libusb_get_port_path,'libusb_get_port_path');
  _CheckVar(libusb_get_device_address,'libusb_get_device_address');
  _CheckVar(libusb_get_device_speed,'libusb_get_device_speed');
  _CheckVar(libusb_get_max_packet_size,'libusb_get_max_packet_size');
  _CheckVar(libusb_get_max_iso_packet_size,'libusb_get_max_iso_packet_size');
  _CheckVar(libusb_open,'libusb_open');
  _CheckVar(libusb_close,'libusb_close');
  _CheckVar(libusb_get_device,'libusb_get_device');
  _CheckVar(libusb_set_configuration,'libusb_set_configuration');
  _CheckVar(libusb_claim_interface,'libusb_claim_interface');
  _CheckVar(libusb_release_interface,'libusb_release_interface');
  _CheckVar(libusb_open_device_with_vid_pid,'libusb_open_device_with_vid_pid');
  _CheckVar(libusb_set_interface_alt_setting,'libusb_set_interface_alt_setting');
  _CheckVar(libusb_clear_halt,'libusb_clear_halt');
  _CheckVar(libusb_reset_device,'libusb_reset_device');
  _CheckVar(libusb_kernel_driver_active,'libusb_kernel_driver_active');
  _CheckVar(libusb_detach_kernel_driver,'libusb_detach_kernel_driver');
  _CheckVar(libusb_attach_kernel_driver,'libusb_attach_kernel_driver');
  _CheckVar(libusb_alloc_transfer,'libusb_alloc_transfer');
  _CheckVar(libusb_submit_transfer,'libusb_submit_transfer');
  _CheckVar(libusb_cancel_transfer,'libusb_cancel_transfer');
  _CheckVar(libusb_free_transfer,'libusb_free_transfer');
  _CheckVar(libusb_control_transfer,'libusb_control_transfer');
  _CheckVar(libusb_bulk_transfer,'libusb_bulk_transfer');
  _CheckVar(libusb_interrupt_transfer,'libusb_interrupt_transfer');
  _CheckVar(libusb_get_string_descriptor_ascii,'libusb_get_string_descriptor_ascii');
  _CheckVar(libusb_try_lock_events,'libusb_try_lock_events');
  _CheckVar(libusb_lock_events,'libusb_lock_events');
  _CheckVar(libusb_unlock_events,'libusb_unlock_events');
  _CheckVar(libusb_event_handling_ok,'libusb_event_handling_ok');
  _CheckVar(libusb_event_handler_active,'libusb_event_handler_active');
  _CheckVar(libusb_lock_event_waiters,'libusb_lock_event_waiters');
  _CheckVar(libusb_unlock_event_waiters,'libusb_unlock_event_waiters');
  _CheckVar(libusb_wait_for_event,'libusb_wait_for_event');
  _CheckVar(libusb_handle_events_timeout,'libusb_handle_events_timeout');
  _CheckVar(libusb_handle_events_timeout_completed,'libusb_handle_events_timeout_completed');
  _CheckVar(libusb_handle_events,'libusb_handle_events');
  _CheckVar(libusb_handle_events_completed,'libusb_handle_events_completed');
  _CheckVar(libusb_handle_events_locked,'libusb_handle_events_locked');
  _CheckVar(libusb_pollfds_handle_timeouts,'libusb_pollfds_handle_timeouts');
  _CheckVar(libusb_get_next_timeout,'libusb_get_next_timeout');
  _CheckVar(libusb_get_pollfds,'libusb_get_pollfds');
  _CheckVar(libusb_set_pollfd_notifiers,'libusb_set_pollfd_notifiers');

end;


end.

