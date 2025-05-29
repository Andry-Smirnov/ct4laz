{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************} 

program test2;


{$APPTYPE CONSOLE}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  vulkanlib;

var
  applicationInfo: TVkApplicationInfo;
  instanceInfo: TVkInstanceCreateInfo;
  instance: TVkInstance;

  result: TVkResult;

  deviceCount: TVkInt32;
  physicalDevices: Array of TVkPhysicalDevice;
  deviceProperties: TVkPhysicalDeviceProperties;

  i: TVkInt32;

begin
  vkAPIInitialize;

  // Filling out application description:
  // sType is mandatory
  applicationInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  // pNext is mandatory
  applicationInfo.pNext := nil;
  // The name of our application
  applicationInfo.pApplicationName := 'Test 2';
  // The name of the engine (e.g: Game engine name)
  applicationInfo.pEngineName := nil;
  // The version of the engine
  applicationInfo.engineVersion := 1;
  // The version of Vulkan we're using for this application
  applicationInfo.apiVersion := VK_API_VERSION;

  // Filling out instance description:
  // sType is mandatory
  instanceInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  // pNext is mandatory
  instanceInfo.pNext := nil;
  // flags is mandatory
  instanceInfo.flags := 0;
  // The application info structure is then passed through the instance
  instanceInfo.pApplicationInfo := @applicationInfo;
  // Don't enable and layer
  instanceInfo.enabledLayerCount := 0;
  instanceInfo.ppEnabledLayerNames := nil;
  // Don't enable any extensions
  instanceInfo.enabledExtensionCount := 0;
  instanceInfo.ppEnabledExtensionNames := nil;

  // Now create the desired instance
  result := vkCreateInstance(@instanceInfo, nil, @instance);
  if (result <> VK_SUCCESS) then
  begin
    Writeln('Failed to create instance: ',Integer(result));
    Exit;
  end;

  // Query how many devices are present in the system
  deviceCount := 0;
  result := vkEnumeratePhysicalDevices(instance, @deviceCount, nil);
  if (result <> VK_SUCCESS) then
  begin
    Writeln('Failed to query the number of physical devices present: ', Integer(result));
    Exit;
  end;

  // There has to be at least one device present
  if (deviceCount = 0) then
  begin
    Writeln('Couldn''t detect any device present with Vulkan support: ', Integer(result));
    Exit;
  end;

  // Get the physical devices
  SetLength(physicalDevices,deviceCount);
  result := vkEnumeratePhysicalDevices(instance, @deviceCount, @physicalDevices[0]);
  if (result <> VK_SUCCESS) then
  begin
    Writeln('Faied to enumerate physical devices present: ', Integer(result));
    Exit;
  end;

  // Enumerate all physical devices
  for i:=0 to deviceCount - 1 do
  begin
    FillChar(deviceProperties,sizeof(deviceProperties),0);
    vkGetPhysicalDeviceProperties(physicalDevices[i],@deviceProperties);
    Writeln('Driver Version: ',deviceProperties.driverVersion);
    Writeln('Device Name:    ',deviceProperties.deviceName);
    Writeln('Device Type:    ',Integer(deviceProperties.deviceType));
    Writeln('API Version:    ',vkGetVersionMajor(deviceProperties.apiVersion),'.',
                               vkGetVersionMinor(deviceProperties.apiVersion),'.',
                               vkGetVersionPatch(deviceProperties.apiVersion));
  end;


  // Never forget to free resources
  vkDestroyInstance(instance, nil);

  SetLength(physicalDevices,0);

  Writeln('');
  Writeln('Press any key to exit...');
  Readln;
end.

