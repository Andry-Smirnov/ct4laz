{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************} 

program test3;

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

  i,j: TVkInt32;

  queueFamilyCount: TVkInt32;
  familyProperties: Array of TVkQueueFamilyProperties;

begin
  vkAPIInitialize;

  // Filling out application description:
  // sType is mandatory
  applicationInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  // pNext is mandatory
  applicationInfo.pNext := nil;
  // The name of our application
  applicationInfo.pApplicationName := 'Test 3';
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

    queueFamilyCount := 0;
    vkGetPhysicalDeviceQueueFamilyProperties(physicalDevices[i], @queueFamilyCount, nil);
    SetLength(familyProperties,queueFamilyCount);
    vkGetPhysicalDeviceQueueFamilyProperties(physicalDevices[i], @queueFamilyCount, @familyProperties[0]);
    // Print the families
    for j:=0 to queueFamilyCount - 1 do
    begin
      Writeln('Count of Queues: ', familyProperties[j].queueCount);
      Writeln('Supported operationg on this queue:');
      if (VK_QUEUE_GRAPHICS_BIT in familyProperties[j].queueFlags) then
        Writeln(#9#9' Graphics');
      if (VK_QUEUE_COMPUTE_BIT in familyProperties[j].queueFlags) then
        Writeln(#9#9' Compute');
      if (VK_QUEUE_TRANSFER_BIT in familyProperties[j].queueFlags) then
        Writeln(#9#9' Transfer');
      if (VK_QUEUE_SPARSE_BINDING_BIT in familyProperties[j].queueFlags) then
        Writeln(#9#9' Sparse Binding');
    end;
    SetLength(familyProperties,0);
  end;

  //====Check some API functions ============
  Writeln('');
  // Test some InstanceFunctions
  if vkDestroyInstance=nil then Writeln('vkDestroyInstance is NULL ?????');
  if vkGetDeviceProcAddr=nil then Writeln('vkGetDeviceProcAddr is NULL ?????');
  if vkCreateDisplayPlaneSurfaceKHR=nil then Writeln('vkCreateXcbSurfaceKHR is NULL ?????');
  if vkCreateDevice=nil then Writeln('vkCreateDevice is NULL ?????');
  if vkCreateGraphicsPipelines=nil then Writeln('vkCreateGraphicsPipelines is NULL ?????');
  if vkEndCommandBuffer=nil then Writeln('vkEndCommandBuffer is NULL ?????');
  if vkCmdExecuteCommands=nil then Writeln('vkCmdExecuteCommands is NULL ?????');

  // Test Device Functions
  if vkCreateDebugReportCallbackEXT=nil then Writeln('vkCreateDebugReportCallbackEXT is NULL ?????');
  if vkDestroyDebugReportCallbackEXT=nil then Writeln('vkDestroyDebugReportCallbackEXT is NULL ?????');
  if vkDebugReportMessageEXT=nil then Writeln('vkDebugReportMessageEXT is NULL ?????');
  if vkDebugMarkerSetObjectNameEXT=nil then Writeln('vkDebugMarkerSetObjectNameEXT is NULL ?????');
  if vkDebugMarkerSetObjectTagEXT=nil then Writeln('vkDebugMarkerSetObjectTagEXT is NULL ?????');
  if vkCmdDebugMarkerBeginEXT=nil then Writeln('vkCmdDebugMarkerBeginEXT is NULL ?????');
  if vkCmdDebugMarkerEndEXT=nil then Writeln('vkCmdDebugMarkerEndEXT is NULL ?????');
  if vkCmdDebugMarkerInsertEXT=nil then Writeln('vkCmdDebugMarkerInsertEXT is NULL ?????');
  if vkGetPhysicalDeviceExternalImageFormatPropertiesNV=nil then Writeln('vkGetPhysicalDeviceExternalImageFormatPropertiesNV is NULL ?????');

  //===========================================
  // Never forget to free resources
  vkDestroyInstance(instance, nil);

  SetLength(physicalDevices,0); 

  Writeln('');
  Writeln('Press any key to exit...');
  Readln;
end.

 
