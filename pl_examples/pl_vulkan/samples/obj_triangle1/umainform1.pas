{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************} 

unit umainform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  vulkanlib, vulkanobjects,vulkanutils;

type
  TCustomAllocHandler = class(TvkObj_AllocationHandler)
  protected
    function AllocateMemory(const aSize: VkSize; const aAlignment: VkSize; const aScope: TVkSystemAllocationScope): PVkVoid; override;
    function ReallocateMemory(const aOriginal: PVkVoid; const aSize: VkSize; const aAlignment: VkSize; const aScope: TVkSystemAllocationScope): PVkVoid; override;
    procedure FreeMemory(const aMemory: PVkVoid); override;
  end;

type
  TUniforms = packed record
    ProjectionMat: TvkuMatrix4f;
    ModelMatrix: TvkuMatrix4f;
    ViewMatrix: TvkuMatrix4f;
  end;

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    RenderPanel: TPanel;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fGraphicsQueueFamilyIndex: VkUint32;
    fGraphicsQueueNodeIndex: VkUint32;
    fPresentQueueNodeIndex: VkUint32;
    fColorFormat: TVkFormat;
    fSurfaceColorSpace: TVkColorSpaceKHR;
    fDepthFormat: TVkFormat;

    fCamera: TvkuCamera;
    fAllocHandler: TCustomAllocHandler;
    fDebugReporter: TvkObj_ConsoleDebugReporter;
    fInstance: TvkObj_Instance;
    fPhyDevice: TvkObj_PhysicalDevice;
    fDevice: TvkObj_Device;
    fQueue: TvkObj_Queue;
    fSurface: TvkObj_Surface;
    fSwapChain: TvkObj_SwapChain;
    fImages: array of TvkObj_Image;
    fImageViews: array of TvkObj_ImageView;
    fRenderPass: TvkObj_RenderPass;
    fFrameBuffers: array of TvkObj_FrameBuffer;
    fDescSetLayout: TvkObj_DescriptorSetLayout;
    fPipelineLayout: TvkObj_PipelineLayout;
    fVertexShader: TvkObj_ShaderModule;
    fFragmentShader: TvkObj_ShaderModule;
    fPipeline: TvkObj_GraphicsPipeline;
    fCommandPool: TvkObj_CommandPool;
    fDrawCommandBuffers: array of TvkObj_CommandBuffer;
    fPostPresentCommandBuffer: TvkObj_CommandBuffer;
    fDepthStencilImage: TvkObj_Image;
    fDepthStencilImageView: TvkObj_ImageView;
    fPipelineCache: TvkObj_PipelineCache;
    fDescriptorPool: TvkObj_DescriptorPool;
    fDescriptorSet: TvkObj_DescriptorSet;
    fVertex: packed record
      DataMemory: TvkObj_DeviceMemory;
      DataBuffer: TvkObj_Buffer;
      IndexMemory: TvkObj_DeviceMemory;
      IndexBuffer: TvkObj_Buffer;
    end;
    fUniform: packed record
      Values: TUniforms;
      Memory: TvkObj_DeviceMemory;
      Buffer: TvkObj_Buffer;
    end;

    procedure UpdateUniformBuffers;
    procedure Render;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

const
  VERTEX_BUFFER_BIND_ID = 0;

function TCustomAllocHandler.AllocateMemory(const aSize: VkSize; const aAlignment: VkSize; const aScope: TVkSystemAllocationScope): PVkVoid;
begin
  result := inherited AllocateMemory(aSize, aAlignment, aScope);
  WriteLn(Format('    MEM - %d bytes of memory allocated at %p', [ aSize, result ]));
end;

function TCustomAllocHandler.ReallocateMemory(const aOriginal: PVkVoid; const aSize: VkSize; const aAlignment: VkSize; const aScope: TVkSystemAllocationScope): PVkVoid;
var p: PVkVoid;
begin
  p := aOriginal;
  result := inherited ReallocateMemory(aOriginal, aSize, aAlignment, aScope);
  WriteLn(Format('    MEM - %d bytes of memory (from %p) reallocated at %p', [ aSize, p, result ]));
end;

procedure TCustomAllocHandler.FreeMemory(const aMemory: PVkVoid);
begin
  WriteLn(Format('    MEM - free memory at %p', [ aMemory ]));
  inherited FreeMemory(aMemory);
end;

procedure TMainForm.FormCreate(Sender: TObject);
type
  TVertex = packed record
    position: array[0..2] of VkFloat;
    color:    array[0..2] of VkFloat;
  end;
  TVertexData = array[0..2] of TVertex;
  TIndexData = array[0..2] of VkUint32;

const
  ClearColor:         TVkClearValue = ( color:        ( float32: ( 0.0, 0.0, 0.0, 1.0 ) ) );
  ClearDepthStencil:  TVkClearValue = ( depthStencil: ( depth: 1.0; stencil: 0 ) );
  VertexData: TVertexData = (
    ( position: ( 1.0, 1.0, 0.0 ); color: ( 1.0, 0.0, 0.0 ) ),
    ( position: (-1.0, 1.0, 0.0 ); color: ( 0.0, 1.0, 0.0 ) ),
    ( position: ( 0.0,-1.0, 0.0 ); color: ( 0.0, 0.0, 1.0 ) )
  );
  IndexData: TIndexData = (
    0, 1, 2
  );

var
  PhyDevices: TVkPhysicalDeviceArr;
  InstanceFactory: TvkObj_InstanceFactory;
  DeviceFactory: TvkObj_DeviceFactory;
  SurfaceFactory: TvkObj_SurfaceFactory;
  CommandPoolFactory: TvkObj_CommandPoolFactory;
  SwapChainFactory: TvkObj_SwapChainFactory;
  ImageFactory: TvkObj_ImageFactory;
  ImageViewFactory: TvkObj_ImageViewFactory;
  RenderPassFactory: TvkObj_RenderPassFactory;
  FrameBufferFactory: TvkObj_FrameBufferFactory;
  DescSetLayoutFactory: TvkObj_DescriptorSetLayoutFactory;
  PipelineLayoutFactory: TvkObj_PipelineLayoutFactory;
  ShaderModuleFactory: TvkObj_ShaderModuleFactory;
  PipelineFactory: TvkObj_GraphicsPipelineFactory;
  PipelineCacheFactory: TvkObj_PipelineCacheFactory;
  BufferFactory: TvkObj_BufferFactory;
  DescriptorPoolFactory: TvkObj_DescriptorPoolFactory;

  SetupCommandBuffer: TvkObj_CommandBuffer;
  QueueFamProps: TVkQueueFamilyPropertiesArr;
  f: TVkFormat;
  i: Integer;
  p: PVkVoid;
  SrfFormats: TVkSurfaceFormatArr;
  SurfaceCapabilities: TVkSurfaceCapabilitiesKHR;
  PresentModes: TVkPresentModeArr;
  pm: TVkPresentModeKHR;
  Images: TVkImageArr;
  AllocCallbacks: TVkAllocationCallbacks;
  ImageMemoryBarrier: TVkImageMemoryBarrier;
  CommandBuffers: TVkCommandBufferArr;
  MemReq: TVkMemoryRequirements;
  vp: TVkViewport;
  r2D: TVkRect2D;
begin
  try
    SetupCommandBuffer  := nil;
    fCamera             := TvkuCamera.Create;
    fAllocHandler       := TCustomAllocHandler.Create;
    AllocCallbacks      := fAllocHandler.GetStructure;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create instance: ');
    InstanceFactory := TvkObj_InstanceFactory.Create;
    try
      InstanceFactory.AllocHandler := fAllocHandler;
      InstanceFactory.EnabledExtensionNames.Add(VK_KHR_SURFACE_EXTENSION_NAME);
      InstanceFactory.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
      InstanceFactory.EnabledExtensionNames.Add(TvkObj_Surface.GetPlatformSurfaceExtensionName);
      fInstance := InstanceFactory.CreateInstance;
      WriteLn('  handle: ', PtrUInt(fInstance.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(InstanceFactory);
    end;

    fDebugReporter := TvkObj_ConsoleDebugReporter.Create(fInstance.InstanceFunctions, fAllocHandler);
  //  if fDebugReporter=nil then   
  //    WriteLn('ERROR : TvkObj_ConsoleDebugReporter.Create');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('find suitable physical device');
    PhyDevices := fInstance.GetPhysicalDevices;
    if (Length(PhyDevices) <= 0) then
      raise TvkObj_Exception.Create('unable to get physical device');
    fPhyDevice := TvkObj_PhysicalDevice.Create(PhyDevices[0], fInstance.InstanceFunctions);
    WriteLn('  handle: ', PtrUInt(fPhyDevice.Handle));

    Write('  find suitable graphics queue index... ');
    QueueFamProps             := fPhyDevice.GetQueueFamilyProperties;
    fGraphicsQueueFamilyIndex := high(fGraphicsQueueFamilyIndex);
    for i := low(QueueFamProps) to high(QueueFamProps) do begin
      if (VK_QUEUE_GRAPHICS_BIT in QueueFamProps[i].queueFlags) then begin
        fGraphicsQueueFamilyIndex := i;
        WriteLn(fGraphicsQueueFamilyIndex);
        break;
      end;
    end;
    if (fGraphicsQueueFamilyIndex = high(fGraphicsQueueFamilyIndex)) then
      raise TvkObj_Exception.Create('unable to find graphics queue index');

    Write('  find suitable depth format... ');
    f := VK_FORMAT_UNDEFINED;
    for f in [ VK_FORMAT_D24_UNORM_S8_UINT, VK_FORMAT_D16_UNORM_S8_UINT, VK_FORMAT_D16_UNORM ] do begin
      if (VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT in fPhyDevice.GetFormatProperties(f).optimalTilingFeatures) then begin
        fDepthFormat := f;
        break;
      end;
    end;
    WriteLn(fDepthFormat);
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create device');
    DeviceFactory := TvkObj_DeviceFactory.Create(fInstance);
    try
      DeviceFactory.AllocHandler := fAllocHandler;
      DeviceFactory.EnabledExtensionNames.Add('VK_KHR_swapchain');
      DeviceFactory.QueueCreateInfos.Length := 1;
      with DeviceFactory.QueueCreateInfos[0] do begin
        Flags                   := [];
        QueueFamilyIndex        := fGraphicsQueueFamilyIndex;
        QueuePriorities.Length  := 1;
        QueuePriorities[0]      := 1.0;
      end;
      fDevice := DeviceFactory.CreateDevice(fPhyDevice.Handle);
      WriteLn('  handle: ', PtrUInt(fDevice.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(DeviceFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('find suitable queue');
    fQueue := TvkObj_Queue.Create(fDevice.GetQueue(fGraphicsQueueFamilyIndex, 0), fDevice.DeviceFunctions);
    WriteLn('  handle: ', PtrUInt(fQueue.Handle));
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create surface: ');
    SurfaceFactory := TvkObj_SurfaceFactory.Create(fInstance);
    try
      SurfaceFactory.AllocHandler := fAllocHandler;
      SurfaceFactory.WinControl   := RenderPanel;
      fSurface := SurfaceFactory.CreateSurface;
      WriteLn('  handle: ', PtrUInt(fSurface.Handle));

      WriteLn('  find suitable graphics and present queue node index');
      fGraphicsQueueNodeIndex := high(fGraphicsQueueNodeIndex);
      fPresentQueueNodeIndex  := high(fPresentQueueNodeIndex);
      QueueFamProps           := fPhyDevice.GetQueueFamilyProperties;
      for i := low(QueueFamProps) to high(QueueFamProps) do begin
        if (VK_QUEUE_GRAPHICS_BIT in QueueFamProps[i].queueFlags) then begin
          if (fGraphicsQueueNodeIndex = high(fGraphicsQueueNodeIndex)) then
            fGraphicsQueueNodeIndex := i;

          if fPhyDevice.GetSurfaceSupport(i, fSurface.Handle) then begin
            fGraphicsQueueNodeIndex := i;
            fPresentQueueNodeIndex  := i;
            break;
          end;
        end;
      end;
      if (fPresentQueueNodeIndex = high(fPresentQueueNodeIndex)) then begin
        for i := low(QueueFamProps) to high(QueueFamProps) do begin
          if fPhyDevice.GetSurfaceSupport(i, fSurface.Handle) then begin
            fPresentQueueNodeIndex := i;
            break;
          end;
        end;
      end;
      WriteLn('  graphics queue node index: ', fGraphicsQueueNodeIndex);
      WriteLn('  present queue node index:  ', fPresentQueueNodeIndex);
      if (fGraphicsQueueNodeIndex = high(fGraphicsQueueNodeIndex)) or
         (fPresentQueueNodeIndex  = high(fPresentQueueNodeIndex)) or
         (fPresentQueueNodeIndex <> fGraphicsQueueNodeIndex) then
           raise TvkObj_Exception.Create('different graphic and present queue node index is not supported yet');

      WriteLn('  find suitable image format');
      SrfFormats := fPhyDevice.GetSurfaceFormats(fSurface.Handle);
      if (Length(SrfFormats) = 0) then
        raise TvkObj_Exception.Create('no suitable formats found');
      if (Length(SrfFormats) = 1) and (SrfFormats[0].format = VK_FORMAT_UNDEFINED) then begin
        fColorFormat     := VK_FORMAT_B8G8R8A8_UNORM;
        fSurfaceColorSpace := VK_COLORSPACE_SRGB_NONLINEAR_KHR;
      end else begin
        fColorFormat     := SrfFormats[0].format;
        fSurfaceColorSpace := SrfFormats[0].colorSpace;
      end;
      WriteLn('  format:      ', fColorFormat);
      WriteLn('  color space: ', fSurfaceColorSpace);

      WriteLn('  done');
    finally
      FreeAndNil(SurfaceFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create command pool');
    CommandPoolFactory := TvkObj_CommandPoolFactory.Create(fDevice);
    try
      CommandPoolFactory.Flags            := [ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ];
      CommandPoolFactory.QueueFamilyIndex := fGraphicsQueueNodeIndex;
      fCommandPool := CommandPoolFactory.CreateCommandPool;
      WriteLn('  handle: ', PtrUInt(fCommandPool.Handle));

      WriteLn('  alloc setup command buffer');
      SetupCommandBuffer := TvkObj_CommandBuffer.Create(
        fCommandPool.AllocateCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY),
        true, fCommandPool.Handle, fDevice.DeviceFunctions);
      SetupCommandBuffer.BeginCommand;
      WriteLn('  handle: ', PtrUInt(SetupCommandBuffer.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(CommandPoolFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create swap chain');
    SwapChainFactory := TvkObj_SwapChainFactory.Create(fDevice);
    try
      SwapChainFactory.AllocHandler      := fAllocHandler;
      SwapChainFactory.Surface           := fSurface.Handle;
      SwapChainFactory.ImageUsage        := [ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ];
      SwapChainFactory.ImageArrayLayers  := 1;
      SwapChainFactory.ImageSharingMode  := VK_SHARING_MODE_EXCLUSIVE;
      SwapChainFactory.Clipped           := true;
      SwapChainFactory.ComposideAlpha    := [ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR ];
      SwapChainFactory.ImageFormat       := fColorFormat;
      SwapChainFactory.ImageColorSpace   := fSurfaceColorSpace;

      WriteLn('  get surface capabilities');
      SurfaceCapabilities := fPhyDevice.GetSurfaceCapabilities(fSurface.Handle);
      if (SurfaceCapabilities.currentExtent.width = High(VkUint32)) then begin
        SurfaceCapabilities.currentExtent.width  := ClientWidth;
        SurfaceCapabilities.currentExtent.height := ClientHeight;
      end else begin
        ClientWidth  := SurfaceCapabilities.currentExtent.width;
        ClientHeight := SurfaceCapabilities.currentExtent.height;
      end;
      WriteLn('    width: ',  SurfaceCapabilities.currentExtent.width);
      WriteLn('    height: ', SurfaceCapabilities.currentExtent.height);
      SwapChainFactory.ImageExtent := SurfaceCapabilities.currentExtent;

      Write('  set min image count... ');
      SwapChainFactory.MinImageCount := SurfaceCapabilities.minImageCount + 1;
      if (SurfaceCapabilities.maxImageCount > 0) and
         (SwapChainFactory.MinImageCount > SurfaceCapabilities.maxImageCount) then
        SwapChainFactory.MinImageCount := SurfaceCapabilities.maxImageCount;
      WriteLn(SwapChainFactory.MinImageCount);

      WriteLn('  set pre transform...');
      SwapChainFactory.PreTransform := SurfaceCapabilities.currentTransform;
      if (VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR in SurfaceCapabilities.supportedTransforms) then
        SwapChainFactory.PreTransform := [ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ];

      Write('  find suitable present mode... ');
      PresentModes := fPhyDevice.GetSurfacePresentModes(fSurface.Handle);
      if (Length(PresentModes) = 0) then
        raise TvkObj_Exception.Create('unable to get present modes');
      for pm in PresentModes do begin
        if (pm = VK_PRESENT_MODE_MAILBOX_KHR) then begin
          SwapChainFactory.PresentMode := pm;
          break;
        end;
        if      (SwapChainFactory.PresentMode <> VK_PRESENT_MODE_MAILBOX_KHR)
            and (pm = VK_PRESENT_MODE_IMMEDIATE_KHR)
        then
          SwapChainFactory.PresentMode := pm;
      end;
      WriteLn(SwapChainFactory.PresentMode);

      fSwapChain := SwapChainFactory.CreateSwapchain;
      WriteLn('  handle: ', PtrUInt(fSwapChain.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(SwapChainFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create images and image views');
    Images := fSwapChain.GetImages;
    SetLength(fImages,     Length(Images));
    SetLength(fImageViews, Length(Images));
    ImageViewFactory := TvkObj_ImageViewFactory.Create(fDevice);
    try
      ImageViewFactory.AllocHandler := fAllocHandler;
      ImageViewFactory.Format       := fColorFormat;
      ImageViewFactory.ViewType     := VK_IMAGE_VIEW_TYPE_2D;
      with ImageViewFactory.SubresourceRange do begin
        AspectMask     := [ VK_IMAGE_ASPECT_COLOR_BIT ];
        BaseMipLevel   := 0;
        LevelCount     := 1;
        BaseArrayLayer := 0;
        LayerCount     := 1;
      end;
      with ImageViewFactory.Components do begin
        r := VK_COMPONENT_SWIZZLE_R;
        g := VK_COMPONENT_SWIZZLE_G;
        b := VK_COMPONENT_SWIZZLE_B;
        a := VK_COMPONENT_SWIZZLE_A;
      end;
      for i := Low(fImageViews) to High(fImageViews) do begin
        fImages[i] := TvkObj_Image.Create(Images[i], false, fDevice.DeviceFunctions, @AllocCallbacks);
        WriteLn('  image handle: ', PtrUInt(fImages[i].Handle));
        ImageMemoryBarrier := fImages[i].GetMemoryBarrier(
          [ VK_IMAGE_ASPECT_COLOR_BIT ],
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);
        SetupCommandBuffer.PipelineBarrier(
          [ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ],
          [ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ],
          [],
          [],
          [],
          [ ImageMemoryBarrier ]);

        ImageViewFactory.Image := fImages[i].Handle;
        fImageViews[i] := ImageViewFactory.CreateImageView;
        WriteLn('  view handle: ', PtrUInt(fImageViews[i].Handle));
      end;
      WriteLn('  done');
    finally
      FreeAndNil(ImageViewFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create draw command buffers');
    SetLength(fDrawCommandBuffers, Length(fImageViews));
    CommandBuffers := fCommandPool.AllocateCommandBuffers(
      VK_COMMAND_BUFFER_LEVEL_PRIMARY,
      Length(fDrawCommandBuffers));
    for i := low(CommandBuffers) to high(CommandBuffers) do begin
      fDrawCommandBuffers[i] := TvkObj_CommandBuffer.Create(
        CommandBuffers[i],
        true,
        fCommandPool.Handle,
        fDevice.DeviceFunctions);
      WriteLn('  handle: ', PtrUInt(fDrawCommandBuffers[i].Handle));
    end;
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create post present command buffers');
    fPostPresentCommandBuffer := TvkObj_CommandBuffer.Create(
      fCommandPool.AllocateCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY),
      true,
      fCommandPool.Handle,
      fDevice.DeviceFunctions);
    WriteLn('  handle: ', PtrUInt(fPostPresentCommandBuffer.Handle));
    WriteLn('  done');


    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create render pass');
    RenderPassFactory := TvkObj_RenderPassFactory.Create(fDevice);
    try
      RenderPassFactory.Attachments.Length := 2;
      with RenderPassFactory.Attachments[0] do begin
        Format          := fColorFormat;
        Samples         := [ VK_SAMPLE_COUNT_1_BIT ];
    	  LoadOp          := VK_ATTACHMENT_LOAD_OP_CLEAR;
    	  StoreOp         := VK_ATTACHMENT_STORE_OP_STORE;
    	  StencilLoadOp   := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    	  StencilStoreOp  := VK_ATTACHMENT_STORE_OP_DONT_CARE;
    	  InitialLayout   := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
    	  FinalLayout     := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      end;

      with RenderPassFactory.Attachments[1] do begin
        Format          := fDepthFormat;
        Samples         := [ VK_SAMPLE_COUNT_1_BIT ];
    	  LoadOp          := VK_ATTACHMENT_LOAD_OP_CLEAR;
    	  StoreOp         := VK_ATTACHMENT_STORE_OP_STORE;
    	  StencilLoadOp   := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    	  StencilStoreOp  := VK_ATTACHMENT_STORE_OP_DONT_CARE;
    	  InitialLayout   := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
    	  FinalLayout     := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
      end;

      RenderPassFactory.Subpasses.Length := 1;
      with RenderPassFactory.Subpasses[0] do begin
        PipelineBindPoint                     := VK_PIPELINE_BIND_POINT_GRAPHICS;

    	  ColorAttachments.Length               := 1;
        ColorAttachments[0].Attachment        := 0;
        ColorAttachments[0].Layout            := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

        DepthStencilAttachments.Length        := 1;
        DepthStencilAttachments[0].Attachment := 1;
        DepthStencilAttachments[0].Layout     := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
      end;

      RenderPassFactory.AllocHandler := fAllocHandler;
      fRenderPass := RenderPassFactory.CreateRenderPass;

      WriteLn('  handle: ', PtrUInt(fRenderPass.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(RenderPassFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create pipeline cache');
    PipelineCacheFactory := TvkObj_PipelineCacheFactory.Create(fDevice);
    try
      fPipelineCache := PipelineCacheFactory.CreatePipelineCache;
      WriteLn('  handle: ', PtrUInt(fPipelineCache.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(PipelineCacheFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create frame buffers');
    FrameBufferFactory := TvkObj_FrameBufferFactory.Create(fDevice);
    try
      SetLength(fFrameBuffers, Length(fImageViews));

      FrameBufferFactory.AllocHandler       := fAllocHandler;
      FrameBufferFactory.RenderPass         := fRenderPass.Handle;
	    FrameBufferFactory.Width              := SurfaceCapabilities.currentExtent.width;
	    FrameBufferFactory.Height             := SurfaceCapabilities.currentExtent.height;
	    FrameBufferFactory.Layers             := 1;
      FrameBufferFactory.Attachments.Length := 2;

      for i := low(fFrameBuffers) to high(fFrameBuffers) do begin
        FrameBufferFactory.Attachments[0] := fImageViews[i].Handle;
        fFrameBuffers[i] := FrameBufferFactory.CreateFrameBuffer;
        WriteLn('  handle: ', PtrUInt(fFrameBuffers[i].Handle));
      end;
      WriteLn('  done');
    finally
      FreeAndNil(FrameBufferFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('flush setup command buffer');
    SetupCommandBuffer.EndCommand;
    fQueue.Submit([], [], [ SetupCommandBuffer.Handle ], [], VK_INVALID_NDP_HANDLE);
    fQueue.WaitIdle;
    FreeAndNil(SetupCommandBuffer);
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('recreate setup command buffer');
    SetupCommandBuffer := TvkObj_CommandBuffer.Create(
      fCommandPool.AllocateCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY),
      true, fCommandPool.Handle, fDevice.DeviceFunctions);
    SetupCommandBuffer.BeginCommand;
    WriteLn('  handle: ', PtrUInt(SetupCommandBuffer.Handle));
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    BufferFactory := TvkObj_BufferFactory.Create(fDevice);
    try
      WriteLn('create vertex data buffer');
      BufferFactory.Size  := SizeOf(VertexData);
      BufferFactory.Usage := [ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT ];
      fVertex.DataBuffer  := BufferFactory.CreateBuffer;
      WriteLn('  buffer handle: ', PtrUInt(fVertex.DataBuffer.Handle));

      MemReq := fVertex.DataBuffer.GetMemoryRequirements;
      fVertex.DataMemory := TvkObj_DeviceMemory.Create(
        fDevice.AllocateMemory(
          MemReq.size,
          vkuGetMemoryTypeIndex(
            fPhyDevice.GetMemoryProperties,
            MemReq.memoryTypeBits,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
          @AllocCallbacks),
        true,
        fDevice.DeviceFunctions,
        @AllocCallbacks);
      WriteLn('  memory handle: ', PtrUInt(fVertex.DataMemory.Handle));
      p := fVertex.DataMemory.Map(0, MemReq.size, 0);
      try
        Move(VertexData, p^, SizeOf(VertexData));
      finally
        fVertex.DataMemory.Unmap;
      end;
      fVertex.DataBuffer.BindMemory(fVertex.DataMemory.Handle, 0);
      WriteLn('  done');

      WriteLn('create vertex index buffer');
      BufferFactory.Size  := SizeOf(IndexData);
      BufferFactory.Usage := [ VK_BUFFER_USAGE_INDEX_BUFFER_BIT ];
      fVertex.IndexBuffer := BufferFactory.CreateBuffer;
      WriteLn('  buffer handle: ', PtrUInt(fVertex.IndexBuffer.Handle));

      MemReq := fVertex.IndexBuffer.GetMemoryRequirements;
      fVertex.IndexMemory := TvkObj_DeviceMemory.Create(
        fDevice.AllocateMemory(
          MemReq.size,
          vkuGetMemoryTypeIndex(
            fPhyDevice.GetMemoryProperties,
            MemReq.memoryTypeBits,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
          @AllocCallbacks),
        true,
        fDevice.DeviceFunctions,
        @AllocCallbacks);
      WriteLn('  memory handle: ', PtrUInt(fVertex.IndexMemory.Handle));
      p := fVertex.IndexMemory.Map(0, MemReq.size, 0);
      try
        Move(IndexData, p^, SizeOf(IndexData));
      finally
        fVertex.IndexMemory.Unmap;
      end;
      fVertex.IndexBuffer.BindMemory(fVertex.IndexMemory.Handle, 0);
      WriteLn('  done');

      WriteLn('create uniform buffer');
      BufferFactory.Size  := SizeOf(IndexData);
      BufferFactory.Usage := [ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT ];
      fUniform.Buffer     := BufferFactory.CreateBuffer;
      WriteLn('  buffer handle: ', PtrUInt(fUniform.Buffer.Handle));

      MemReq := fUniform.Buffer.GetMemoryRequirements;
      fUniform.Memory := TvkObj_DeviceMemory.Create(
        fDevice.AllocateMemory(
          MemReq.size,
          vkuGetMemoryTypeIndex(
            fPhyDevice.GetMemoryProperties,
            MemReq.memoryTypeBits,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
          @AllocCallbacks),
        true,
        fDevice.DeviceFunctions,
        @AllocCallbacks);
      WriteLn('  memory handle: ', PtrUInt(fUniform.Memory.Handle));
      fUniform.Buffer.BindMemory(fUniform.Memory.Handle, 0);
      UpdateUniformBuffers;
      WriteLn('  done');
    finally
      FreeAndNil(BufferFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create descriptor set layout');
    DescSetLayoutFactory := TvkObj_DescriptorSetLayoutFactory.Create(fDevice);
    try
      DescSetLayoutFactory.Bindings.Length := 1;
      with DescSetLayoutFactory.Bindings[0] do begin
        Binding           := 0; // binding 0 (Uniform Buffer/Vertex Shader)
        DescriptorType    := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		    DescriptorCount   := 1;
        StageFlags        := [ VK_SHADER_STAGE_VERTEX_BIT ];
      end;

      DescSetLayoutFactory.AllocHandler := fAllocHandler;

      fDescSetLayout := DescSetLayoutFactory.CreateDescriptorSetLayout;
      WriteLn('  handle: ', PtrUInt(fDescSetLayout.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(DescSetLayoutFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create pipeline layout');
    PipelineLayoutFactory := TvkObj_PipelineLayoutFactory.Create(fDevice);
    try
      PipelineLayoutFactory.AllocHandler      := fAllocHandler;
      PipelineLayoutFactory.SetLayouts.Length := 1;
      PipelineLayoutFactory.SetLayouts[0]     := fDescSetLayout.Handle;
      fPipelineLayout := PipelineLayoutFactory.CreatePipelineLayout;
      WriteLn('  handle: ', PtrUInt(fPipelineLayout.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(PipelineLayoutFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create shader modules');
    ShaderModuleFactory := TvkObj_ShaderModuleFactory.Create(fDevice);
    try
      ShaderModuleFactory.AllocHandler := fAllocHandler;

      ShaderModuleFactory.LoadSpvCode(pathMedia + 'triangle.vert.spv');
      fVertexShader := ShaderModuleFactory.CreateShaderModule;
      WriteLn('  vertex handle: ', PtrUInt(fVertexShader.Handle));

      ShaderModuleFactory.LoadSpvCode(pathMedia + 'triangle.frag.spv');
      fFragmentShader := ShaderModuleFactory.CreateShaderModule;
      WriteLn('  fragment handle: ', PtrUInt(fFragmentShader.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(ShaderModuleFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create graphics pipeline');
    PipelineFactory := TvkObj_GraphicsPipelineFactory.Create(fDevice);
    try
      PipelineFactory.AllocHandler := fAllocHandler;
      PipelineFactory.Stages.Length := 2;
      with PipelineFactory.Stages[0] do begin
        Stage  := [ VK_SHADER_STAGE_VERTEX_BIT ];
        Module := fVertexShader.Handle;
        Name   := 'main';
      end;
      with PipelineFactory.Stages[1] do begin
        Stage  := [ VK_SHADER_STAGE_FRAGMENT_BIT ];
        Module := fFragmentShader.Handle;
        Name   := 'main';
      end;
      with PipelineFactory.VertexInputState do begin
        VertexBindingDescriptions.Length := 1;
        VertexBindingDescriptions[0].Binding   := VERTEX_BUFFER_BIND_ID;
        VertexBindingDescriptions[0].Stride    := SizeOf(TVertex);
        VertexBindingDescriptions[0].InputRate := VK_VERTEX_INPUT_RATE_VERTEX;

        VertexAttributeDescriptions.Length := 2;
        // Location 0: Position
        VertexAttributeDescriptions[0].Binding  := VERTEX_BUFFER_BIND_ID;
        VertexAttributeDescriptions[0].Location := 0;
        VertexAttributeDescriptions[0].Format   := VK_FORMAT_R32G32B32_SFLOAT;
        VertexAttributeDescriptions[0].Offset   := 0;
        VertexAttributeDescriptions[0].Binding  := 0;
        // Location 1: Color
        VertexAttributeDescriptions[1].Binding  := VERTEX_BUFFER_BIND_ID;
        VertexAttributeDescriptions[1].Location := 1;
        VertexAttributeDescriptions[1].Format   := VK_FORMAT_R32G32B32_SFLOAT;
        VertexAttributeDescriptions[1].Offset   := 3 * SizeOf(VkFloat);
        VertexAttributeDescriptions[1].Binding  := 0;
      end;
      with PipelineFactory.InputAssemblyState do begin
        Topology := VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
      end;
      with PipelineFactory.RasterizationState do begin
        PolygonMode             := VK_POLYGON_MODE_FILL;
        CullMode                := [VK_CULL_MODE_NONE];
        FrontFace               := VK_FRONT_FACE_COUNTER_CLOCKWISE;
        DepthClampEnable        := false;
        RasterizerDiscardEnable := false;
        DepthBiasEnable         := false;
      end;
      with PipelineFactory.ColorBlendState do begin
        Attachments.Length := 1;
        Attachments[0].ColorWriteMask := [
          VK_COLOR_COMPONENT_R_BIT,
          VK_COLOR_COMPONENT_G_BIT,
          VK_COLOR_COMPONENT_B_BIT,
          VK_COLOR_COMPONENT_A_BIT];
        Attachments[0].BlendEnable := false;
      end;
      with PipelineFactory.ViewportState do begin
        Viewports.Length := 1;
        Scissors.Length  := 1;
      end;
      with PipelineFactory.DynamicState do begin
        DynamicStates.Length := 2;
        DynamicStates[0] := VK_DYNAMIC_STATE_VIEWPORT;
        DynamicStates[1] := VK_DYNAMIC_STATE_SCISSOR;
      end;
      with PipelineFactory.DepthStencilState do begin
        DepthTestEnable       := true;
        DepthWriteEnable      := true;
        DepthCompareOp        := VK_COMPARE_OP_LESS_OR_EQUAL;
        DepthBoundsTestEnable := false;
        StencilTestEnable     := false;
        Back.failOp           := VK_STENCIL_OP_KEEP;
        Back.passOp           := VK_STENCIL_OP_KEEP;
        Back.compareOp        := VK_COMPARE_OP_ALWAYS;
        Front                 := Back;
      end;
      with PipelineFactory.MultiSampleState do begin
        RasterizationSamples := [ VK_SAMPLE_COUNT_1_BIT ];
      end;
      PipelineFactory.Layout     := fPipelineLayout.Handle;
      PipelineFactory.RenderPass := fRenderPass.Handle;
      PipelineFactory.SubPass    := 0;
      fPipeline := PipelineFactory.CreateGraphicsPipeline(VK_INVALID_NDP_HANDLE);
      WriteLn('  handle: ', PtrUInt(fPipeline.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(PipelineFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('create descriptor pool');
    DescriptorPoolFactory := TvkObj_DescriptorPoolFactory.Create(fDevice.DeviceFunctions);
    try
      DescriptorPoolFactory.MaxSets                      := 1;
      DescriptorPoolFactory.PoolSizes.Length             := 1;
      DescriptorPoolFactory.PoolSizes[0].DescriptorType  := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
      DescriptorPoolFactory.PoolSizes[0].DescriptorCount := 1;
      fDescriptorPool := DescriptorPoolFactory.CreateDescriptorPool;
      WriteLn('  handle: ', PtrUInt(fDescriptorPool.Handle));
      WriteLn('  done');
    finally
      FreeAndNil(DescriptorPoolFactory);
    end;

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('setup/update descriptor sets');
    fDescriptorSet := TvkObj_DescriptorSet.Create(
      fDescriptorPool.AllocateDescriptorSet(fDescSetLayout.Handle),
      true,
      fDescriptorPool.Handle,
      fDevice.DeviceFunctions);
    WriteLn('  handle: ', PtrUInt(fDescriptorSet.Handle));
    WriteLn('  update descriptor set');

    with fDevice.UpdateDescriptorSet do
    try
      WriteDescriptorSets.Length := 1;
      WriteDescriptorSets[0].DstSet                 := fDescriptorSet.Handle;
      WriteDescriptorSets[0].DstBinding             := 0;
      WriteDescriptorSets[0].DescriptorType         := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
      WriteDescriptorSets[0].BufferInfos.Length     := 1;
      WriteDescriptorSets[0].BufferInfos[0].Buffer  := fUniform.Buffer.Handle;
      WriteDescriptorSets[0].BufferInfos[0].Offset  := 0;
      WriteDescriptorSets[0].BufferInfos[0].Range   := SizeOf(fUniform.Values);
      Execute;
    finally
      Free;
    end;
    WriteLn('  done');

    //////////////////////////////////////////////////////////////////////////////
    WriteLn('build command buffers');
    for i := low(fDrawCommandBuffers) to high(fDrawCommandBuffers) do begin
      WriteLn('  command buffer: ', PtrUInt(fDrawCommandBuffers[i].Handle));
      with fDrawCommandBuffers[i] do begin
        BeginCommand;
        with BeginRenderPass do
        try
          RenderPassBeginInfo.RenderPass               := fRenderPass.Handle;
          RenderPassBeginInfo.Framebuffer              := fFrameBuffers[i].Handle;
          RenderPassBeginInfo.RenderArea.offset.x      := 0;
          RenderPassBeginInfo.RenderArea.offset.y      := 0;
          RenderPassBeginInfo.RenderArea.extent.width  := ClientWidth;
          RenderPassBeginInfo.RenderArea.extent.height := ClientHeight;
          RenderPassBeginInfo.ClearValues.Length       := 2;
          RenderPassBeginInfo.ClearValues[0]           := ClearColor;
          RenderPassBeginInfo.ClearValues[1]           := ClearDepthStencil;
          Execute(VK_SUBPASS_CONTENTS_INLINE);
        finally
          Free;
        end;
        FillByte(vp, SizeOf(vp), 0);
          vp.height   := ClientHeight;
          vp.width    := ClientWidth;
          vp.minDepth := 0.0;
          vp.maxDepth := 1.0;
        SetViewport(0, [ vp ]);
        FillByte(r2D, SizeOf(r2D), 0);
          r2D.extent.width  := ClientWidth;
          r2D.extent.height := ClientHeight;
          r2D.offset.x      := 0;
          r2D.offset.y      := 0;
          SetScissors(1, [ r2D ]);
        BindDescriptorSet(
          VK_PIPELINE_BIND_POINT_GRAPHICS,
          fPipelineLayout.Handle,
          0,
          [ fDescriptorSet.Handle ],
          [ ]);
        BindPipeline(
          VK_PIPELINE_BIND_POINT_GRAPHICS,
          fPipeline.Handle);
        BindVertexBuffers(
          VERTEX_BUFFER_BIND_ID,
          [ fVertex.DataBuffer.Handle ],
          [ 0 ]);
        BindIndexBuffers(
          fVertex.IndexBuffer.Handle,
          0,
          VK_INDEX_TYPE_UINT32);
        DrawIndexed(
          3, 1, 0, 0, 1);

        FillByte(ImageMemoryBarrier, SizeOf(ImageMemoryBarrier), 0);
        with ImageMemoryBarrier do begin
          sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
          pNext := nil;
          srcAccessMask := [ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT ];
          dstAccessMask := [ ];
          oldLayout := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
          newLayout := VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
          srcQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
          dstQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
          subresourceRange.aspectMask     := [ VK_IMAGE_ASPECT_COLOR_BIT ];
          subresourceRange.baseMipLevel   := 0;
          subresourceRange.levelCount     := 1;
          subresourceRange.baseArrayLayer := 0;
          subresourceRange.layerCount     := 1;
        end;
        PipelineBarrier(
          [ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT ],
          [ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ],
          [],
          [],
          [],
          [ ImageMemoryBarrier ]);
        EndCommand;
      end;
    end;
    WriteLn('  done');

  finally
    FreeAndNil(SetupCommandBuffer);
  end;
end;
procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Render;
end;

procedure TMainForm.FormDestroy(Sender: TObject);

  procedure FreeAndNilFrameBuffers;
  var i: Integer;
  begin
    for i := Low(fFrameBuffers) to High(fFrameBuffers) do
      FreeAndNil(fFrameBuffers[i]);
    SetLength(fFrameBuffers, 0);
  end;

  procedure FreeAndNilImages;
  var i: Integer;
  begin
    for i := low(fImages) to high(fImages) do
      FreeAndNil(fImages[i]);
    SetLength(fImages, 0);
  end;

  procedure FreeAndNilImageViews;
  var i: Integer;
  begin
    for i := Low(fImageViews) to High(fImageViews) do
      FreeAndNil(fImageViews[i]);
    SetLength(fImageViews, 0);
  end;

  procedure FreeAndNilDrawCommandBuffers;
  var i: Integer;
  begin
    for i := Low(fDrawCommandBuffers) to High(fDrawCommandBuffers) do
      FreeAndNil(fDrawCommandBuffers[i]);
    SetLength(fDrawCommandBuffers, 0);
  end;

begin
  FreeAndNil(fDescriptorSet);
  FreeAndNil(fDescriptorPool);
  FreeAndNil(fPipeline);
  FreeAndNil(fFragmentShader);
  FreeAndNil(fVertexShader);
  FreeAndNil(fPipelineLayout);
  FreeAndNil(fDescSetLayout);
  FreeAndNil(fUniform.Buffer);
  FreeAndNil(fUniform.Memory);
  FreeAndNil(fVertex.DataBuffer);
  FreeAndNil(fVertex.DataMemory);
  FreeAndNil(fVertex.IndexBuffer);
  FreeAndNil(fVertex.IndexMemory);
  FreeAndNilFrameBuffers;
  FreeAndNil(fPipelineCache);
  FreeAndNil(fRenderPass);
  FreeAndNil(fDepthStencilImageView);
  FreeAndNil(fDepthStencilImage);
  FreeAndNil(fPostPresentCommandBuffer);
  FreeAndNilDrawCommandBuffers;
  FreeAndNilImageViews;
  FreeAndNilImages;
  FreeAndNil(fSwapChain);
  FreeAndNil(fCommandPool);
  FreeAndNil(fSurface);
  FreeAndNil(fQueue);
  FreeAndNil(fDevice);
  FreeAndNil(fPhyDevice);
  FreeAndNil(fDebugReporter);
  FreeAndNil(fInstance);
  FreeAndNil(fAllocHandler);
  FreeAndNil(fCamera);
end;

procedure TMainForm.UpdateUniformBuffers;
var p: PVkVoid;
begin
  fCamera.Perspective(45, ClientWidth/ClientHeight, 0.001, 1000);
  fCamera.Position := vkuMatrixIdentity * vkuMatrixTranslate(vkuVector3f(0, 0, -5));

  fUniform.Values.ProjectionMat := fCamera.ProjMatrix;
  fUniform.Values.ModelMatrix   := fCamera.Position;
  fUniform.Values.ViewMatrix    := vkuMatrixIdentity;

  p := fUniform.Memory.Map(0, SizeOf(fUniform.Values), 0);
  try
    move(fUniform.Values, p^, SizeOf(fUniform.Values));
  finally
    fUniform.Memory.Unmap;
  end;
end;

procedure TMainForm.Render;
var
  Semaphore: TvkObj_Semaphore;
  AllocCallbacks: TVkAllocationCallbacks;
  CurrentImage: VkUint32;
  PostPresentBarrier: TVkImageMemoryBarrier;
begin
  fDevice.WaitIdle;

  // present queue
  AllocCallbacks := fAllocHandler.GetStructure;
  Semaphore      := TvkObj_Semaphore.Create(
    TVkSemaphoreCreateFlags([ ]),
    fDevice.DeviceFunctions,
    @AllocCallbacks);
  try
    CurrentImage := fSwapChain.AcquireNextImage(High(VkUint64), Semaphore.Handle, VK_INVALID_NDP_HANDLE);
    fQueue.Submit(
      [ Semaphore.Handle ],
      [ ],
      [ fDrawCommandBuffers[CurrentImage].Handle ],
      [ ],
      VK_INVALID_NDP_HANDLE);
    fQueue.Present(
      [ ],
      [ fSwapChain.Handle ],
      [ CurrentImage ]);
  finally
    FreeAndNil(Semaphore);
  end;

  // post present image
  PostPresentBarrier := fImages[CurrentImage].GetMemoryBarrier(
    [ VK_IMAGE_ASPECT_COLOR_BIT ],
    VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
  PostPresentBarrier.srcAccessMask := [];
  PostPresentBarrier.dstAccessMask := [ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT ];

  fPostPresentCommandBuffer.BeginCommand;
  try
    fPostPresentCommandBuffer.PipelineBarrier(
      [ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT ],
      [ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ],
      [ ],
      [ ],
      [ ],
      [ PostPresentBarrier ]);
  finally
    fPostPresentCommandBuffer.EndCommand;
  end;

  // submit queue
  fQueue.Submit(
    [ ],
    [ ],
    [ fPostPresentCommandBuffer.Handle ],
    [ ],
    VK_INVALID_NDP_HANDLE);
  fQueue.WaitIdle;

  fDevice.WaitIdle;
end;

end.

