{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************} 

program obj_test3;

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
  InstanceFunctions:TVkInstanceFunctions;

  result: TVkResult;

  deviceCount: TVkInt32;
  physicalDevices: Array of TVkPhysicalDevice;
  deviceProperties: TVkPhysicalDeviceProperties;

  i,j: TVkInt32;

  queueFamilyCount: TVkInt32;
  familyProperties: Array of TVkQueueFamilyProperties;

begin
  vkoAPIInitialize;

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
  result := vkoCreateInstance(@instanceInfo, nil, @instance);
  if (result <> VK_SUCCESS) then
  begin
    Writeln('Failed to create instance: ',Integer(result));
    Exit;
  end;

  //Load Vulkan Instance Commands
  InstanceFunctions:=vkoLoadInstanceFunctions(instance);

  // Query how many devices are present in the system
  deviceCount := 0;
  result := InstanceFunctions.vkEnumeratePhysicalDevices(instance, @deviceCount, nil);
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
  result := InstanceFunctions.vkEnumeratePhysicalDevices(instance, @deviceCount, @physicalDevices[0]);
  if (result <> VK_SUCCESS) then
  begin
    Writeln('Faied to enumerate physical devices present: ', Integer(result));
    Exit;
  end;

  // Enumerate all physical devices
  for i:=0 to deviceCount - 1 do
  begin
    FillChar(deviceProperties,sizeof(deviceProperties),0);
    InstanceFunctions.vkGetPhysicalDeviceProperties(physicalDevices[i],@deviceProperties);
    Writeln('Driver Version: ',deviceProperties.driverVersion);
    Writeln('Device Name:    ',deviceProperties.deviceName);
    Writeln('Device Type:    ',Integer(deviceProperties.deviceType));
    Writeln('API Version:    ',vkGetVersionMajor(deviceProperties.apiVersion),'.',
                               vkGetVersionMinor(deviceProperties.apiVersion),'.',
                               vkGetVersionPatch(deviceProperties.apiVersion));

    queueFamilyCount := 0;
    InstanceFunctions.vkGetPhysicalDeviceQueueFamilyProperties(physicalDevices[i], @queueFamilyCount, nil);
    SetLength(familyProperties,queueFamilyCount);
    InstanceFunctions.vkGetPhysicalDeviceQueueFamilyProperties(physicalDevices[i], @queueFamilyCount, @familyProperties[0]);
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
  if InstanceFunctions.vkDestroyInstance=nil then Writeln('vkDestroyInstance is NULL ?????');  
  if InstanceFunctions.vkGetDeviceProcAddr=nil then Writeln('vkGetDeviceProcAddr is NULL ?????'); 
  if InstanceFunctions.vkCreateDisplayPlaneSurfaceKHR=nil then Writeln('vkCreateXcbSurfaceKHR is NULL ?????');
  if InstanceFunctions.vkCreateDevice=nil then Writeln('vkCreateDevice is NULL ?????'); 
  if InstanceFunctions.vkCreateGraphicsPipelines=nil then Writeln('vkCreateGraphicsPipelines is NULL ?????');
  if InstanceFunctions.vkEndCommandBuffer=nil then Writeln('vkEndCommandBuffer is NULL ?????');
  if InstanceFunctions.vkCmdExecuteCommands=nil then Writeln('vkCmdExecuteCommands is NULL ?????');

  // Test Device Functions
  if InstanceFunctions.vkCreateDebugReportCallbackEXT=nil then Writeln('vkCreateDebugReportCallbackEXT is NULL ?????');
  if InstanceFunctions.vkDestroyDebugReportCallbackEXT=nil then Writeln('vkDestroyDebugReportCallbackEXT is NULL ?????');
  if InstanceFunctions.vkDebugReportMessageEXT=nil then Writeln('vkDebugReportMessageEXT is NULL ?????');
  if InstanceFunctions.vkDebugMarkerSetObjectNameEXT=nil then Writeln('vkDebugMarkerSetObjectNameEXT is NULL ?????');
  if InstanceFunctions.vkDebugMarkerSetObjectTagEXT=nil then Writeln('vkDebugMarkerSetObjectTagEXT is NULL ?????');
  if InstanceFunctions.vkCmdDebugMarkerBeginEXT=nil then Writeln('vkCmdDebugMarkerBeginEXT is NULL ?????');
  if InstanceFunctions.vkCmdDebugMarkerEndEXT=nil then Writeln('vkCmdDebugMarkerEndEXT is NULL ?????');
  if InstanceFunctions.vkCmdDebugMarkerInsertEXT=nil then Writeln('vkCmdDebugMarkerInsertEXT is NULL ?????');
  if InstanceFunctions.vkGetPhysicalDeviceExternalImageFormatPropertiesNV=nil then Writeln('vkGetPhysicalDeviceExternalImageFormatPropertiesNV is NULL ?????');

  //===========================================
  // Never forget to free resources
  InstanceFunctions.vkDestroyInstance(instance, nil);

  SetLength(physicalDevices,0); 

  Writeln('');
  Writeln('Press any key to exit...');
  Readln;
end.

 
