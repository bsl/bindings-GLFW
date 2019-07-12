{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include <GLFW/glfw3.h>

#ifdef ExposeNative
  #warning "You are compiling glfw using the native access functions. BEWARE."

  #if defined(_WIN32)

    #define GLFW_EXPOSE_NATIVE_WIN32
    #define GLFW_EXPOSE_NATIVE_WGL

  #elif defined(__APPLE__)

    #define GLFW_EXPOSE_NATIVE_COCOA
    #define GLFW_EXPOSE_NATIVE_NSGL

  #elif defined(__linux__)

    #define GLFW_EXPOSE_NATIVE_EGL

    #if defined(BINDINGS_GLFW_USE_X11)
      #define GLFW_EXPOSE_NATIVE_X11
      #define GLFW_EXPOSE_NATIVE_GLX
    #elif defined(BINDINGS_GLFW_USE_WAYLAND)
      #define GLFW_EXPOSE_NATIVE_WAYLAND
      #define GLFW_EXPOSE_NATIVE_EGL
    #endif

  #endif

  #include <GLFW/glfw3native.h>

#endif

--------------------------------------------------------------------------------

module Bindings.GLFW where

import Prelude (Eq, IO, Num, Show)
import Prelude (($), return, error, (++), undefined, div, take)

import Data.Data             (Data)
import Data.Int              (Int32)
import Data.Word             (Word32, Word64)
import Data.Typeable         (Typeable)
import Foreign.C.Types       (CChar, CUChar, CUShort)
import Foreign.C.Types       (CDouble(..), CFloat(..), CInt(..), CUInt(..))
import Foreign.C.String      (CString)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Foreign.Ptr           (FunPtr, nullFunPtr, Ptr, plusPtr)
import Foreign.Storable      (Storable(..))
--------------------------------------------------------------------------------

#num GLFW_VERSION_MAJOR
#num GLFW_VERSION_MINOR
#num GLFW_VERSION_REVISION

#num GLFW_RELEASE
#num GLFW_PRESS
#num GLFW_REPEAT

#num GLFW_KEY_UNKNOWN

#num GLFW_KEY_SPACE
#num GLFW_KEY_APOSTROPHE
#num GLFW_KEY_COMMA
#num GLFW_KEY_MINUS
#num GLFW_KEY_PERIOD
#num GLFW_KEY_SLASH
#num GLFW_KEY_0
#num GLFW_KEY_1
#num GLFW_KEY_2
#num GLFW_KEY_3
#num GLFW_KEY_4
#num GLFW_KEY_5
#num GLFW_KEY_6
#num GLFW_KEY_7
#num GLFW_KEY_8
#num GLFW_KEY_9
#num GLFW_KEY_SEMICOLON
#num GLFW_KEY_EQUAL
#num GLFW_KEY_A
#num GLFW_KEY_B
#num GLFW_KEY_C
#num GLFW_KEY_D
#num GLFW_KEY_E
#num GLFW_KEY_F
#num GLFW_KEY_G
#num GLFW_KEY_H
#num GLFW_KEY_I
#num GLFW_KEY_J
#num GLFW_KEY_K
#num GLFW_KEY_L
#num GLFW_KEY_M
#num GLFW_KEY_N
#num GLFW_KEY_O
#num GLFW_KEY_P
#num GLFW_KEY_Q
#num GLFW_KEY_R
#num GLFW_KEY_S
#num GLFW_KEY_T
#num GLFW_KEY_U
#num GLFW_KEY_V
#num GLFW_KEY_W
#num GLFW_KEY_X
#num GLFW_KEY_Y
#num GLFW_KEY_Z
#num GLFW_KEY_LEFT_BRACKET
#num GLFW_KEY_BACKSLASH
#num GLFW_KEY_RIGHT_BRACKET
#num GLFW_KEY_GRAVE_ACCENT
#num GLFW_KEY_WORLD_1
#num GLFW_KEY_WORLD_2

#num GLFW_KEY_ESCAPE
#num GLFW_KEY_ENTER
#num GLFW_KEY_TAB
#num GLFW_KEY_BACKSPACE
#num GLFW_KEY_INSERT
#num GLFW_KEY_DELETE
#num GLFW_KEY_RIGHT
#num GLFW_KEY_LEFT
#num GLFW_KEY_DOWN
#num GLFW_KEY_UP
#num GLFW_KEY_PAGE_UP
#num GLFW_KEY_PAGE_DOWN
#num GLFW_KEY_HOME
#num GLFW_KEY_END
#num GLFW_KEY_CAPS_LOCK
#num GLFW_KEY_SCROLL_LOCK
#num GLFW_KEY_NUM_LOCK
#num GLFW_KEY_PRINT_SCREEN
#num GLFW_KEY_PAUSE
#num GLFW_KEY_F1
#num GLFW_KEY_F2
#num GLFW_KEY_F3
#num GLFW_KEY_F4
#num GLFW_KEY_F5
#num GLFW_KEY_F6
#num GLFW_KEY_F7
#num GLFW_KEY_F8
#num GLFW_KEY_F9
#num GLFW_KEY_F10
#num GLFW_KEY_F11
#num GLFW_KEY_F12
#num GLFW_KEY_F13
#num GLFW_KEY_F14
#num GLFW_KEY_F15
#num GLFW_KEY_F16
#num GLFW_KEY_F17
#num GLFW_KEY_F18
#num GLFW_KEY_F19
#num GLFW_KEY_F20
#num GLFW_KEY_F21
#num GLFW_KEY_F22
#num GLFW_KEY_F23
#num GLFW_KEY_F24
#num GLFW_KEY_F25
#num GLFW_KEY_KP_0
#num GLFW_KEY_KP_1
#num GLFW_KEY_KP_2
#num GLFW_KEY_KP_3
#num GLFW_KEY_KP_4
#num GLFW_KEY_KP_5
#num GLFW_KEY_KP_6
#num GLFW_KEY_KP_7
#num GLFW_KEY_KP_8
#num GLFW_KEY_KP_9
#num GLFW_KEY_KP_DECIMAL
#num GLFW_KEY_KP_DIVIDE
#num GLFW_KEY_KP_MULTIPLY
#num GLFW_KEY_KP_SUBTRACT
#num GLFW_KEY_KP_ADD
#num GLFW_KEY_KP_ENTER
#num GLFW_KEY_KP_EQUAL
#num GLFW_KEY_LEFT_SHIFT
#num GLFW_KEY_LEFT_CONTROL
#num GLFW_KEY_LEFT_ALT
#num GLFW_KEY_LEFT_SUPER
#num GLFW_KEY_RIGHT_SHIFT
#num GLFW_KEY_RIGHT_CONTROL
#num GLFW_KEY_RIGHT_ALT
#num GLFW_KEY_RIGHT_SUPER
#num GLFW_KEY_MENU
#num GLFW_KEY_LAST

#num GLFW_MOD_SHIFT
#num GLFW_MOD_CONTROL
#num GLFW_MOD_ALT
#num GLFW_MOD_SUPER

#num GLFW_MOUSE_BUTTON_1
#num GLFW_MOUSE_BUTTON_2
#num GLFW_MOUSE_BUTTON_3
#num GLFW_MOUSE_BUTTON_4
#num GLFW_MOUSE_BUTTON_5
#num GLFW_MOUSE_BUTTON_6
#num GLFW_MOUSE_BUTTON_7
#num GLFW_MOUSE_BUTTON_8
#num GLFW_MOUSE_BUTTON_LAST
#num GLFW_MOUSE_BUTTON_LEFT
#num GLFW_MOUSE_BUTTON_RIGHT
#num GLFW_MOUSE_BUTTON_MIDDLE

#num GLFW_JOYSTICK_1
#num GLFW_JOYSTICK_2
#num GLFW_JOYSTICK_3
#num GLFW_JOYSTICK_4
#num GLFW_JOYSTICK_5
#num GLFW_JOYSTICK_6
#num GLFW_JOYSTICK_7
#num GLFW_JOYSTICK_8
#num GLFW_JOYSTICK_9
#num GLFW_JOYSTICK_10
#num GLFW_JOYSTICK_11
#num GLFW_JOYSTICK_12
#num GLFW_JOYSTICK_13
#num GLFW_JOYSTICK_14
#num GLFW_JOYSTICK_15
#num GLFW_JOYSTICK_16
#num GLFW_JOYSTICK_LAST

#num GLFW_NOT_INITIALIZED
#num GLFW_NO_CURRENT_CONTEXT
#num GLFW_INVALID_ENUM
#num GLFW_INVALID_VALUE
#num GLFW_OUT_OF_MEMORY
#num GLFW_API_UNAVAILABLE
#num GLFW_VERSION_UNAVAILABLE
#num GLFW_PLATFORM_ERROR
#num GLFW_FORMAT_UNAVAILABLE

#num GLFW_FOCUSED
#num GLFW_ICONIFIED
#num GLFW_RESIZABLE
#num GLFW_VISIBLE
#num GLFW_DECORATED

#num GLFW_RED_BITS
#num GLFW_GREEN_BITS
#num GLFW_BLUE_BITS
#num GLFW_ALPHA_BITS
#num GLFW_DEPTH_BITS
#num GLFW_STENCIL_BITS
#num GLFW_ACCUM_RED_BITS
#num GLFW_ACCUM_GREEN_BITS
#num GLFW_ACCUM_BLUE_BITS
#num GLFW_ACCUM_ALPHA_BITS
#num GLFW_AUX_BUFFERS
#num GLFW_STEREO
#num GLFW_SAMPLES
#num GLFW_SRGB_CAPABLE
#num GLFW_REFRESH_RATE

#num GLFW_CLIENT_API
#num GLFW_CONTEXT_VERSION_MAJOR
#num GLFW_CONTEXT_VERSION_MINOR
#num GLFW_CONTEXT_REVISION
#num GLFW_CONTEXT_ROBUSTNESS
#num GLFW_OPENGL_FORWARD_COMPAT
#num GLFW_OPENGL_DEBUG_CONTEXT
#num GLFW_OPENGL_PROFILE

#num GLFW_OPENGL_API
#num GLFW_OPENGL_ES_API

#num GLFW_NO_ROBUSTNESS
#num GLFW_NO_RESET_NOTIFICATION
#num GLFW_LOSE_CONTEXT_ON_RESET

#num GLFW_OPENGL_ANY_PROFILE
#num GLFW_OPENGL_CORE_PROFILE
#num GLFW_OPENGL_COMPAT_PROFILE

#num GLFW_CURSOR
#num GLFW_STICKY_KEYS
#num GLFW_STICKY_MOUSE_BUTTONS

#num GLFW_CURSOR_NORMAL
#num GLFW_CURSOR_HIDDEN
#num GLFW_CURSOR_DISABLED

#num GLFW_CONNECTED
#num GLFW_DISCONNECTED

#callback GLFWglproc , IO ()

#opaque_t GLFWmonitor
deriving instance Typeable C'GLFWmonitor
deriving instance Data     C'GLFWmonitor

#opaque_t GLFWwindow
deriving instance Typeable C'GLFWwindow
deriving instance Data     C'GLFWwindow

#callback GLFWerrorfun              , CInt -> Ptr CChar ->                                IO ()
#callback GLFWwindowposfun          , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWwindowsizefun         , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWwindowclosefun        , Ptr <GLFWwindow> ->                                 IO ()
#callback GLFWwindowrefreshfun      , Ptr <GLFWwindow> ->                                 IO ()
#callback GLFWwindowfocusfun        , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWwindowiconifyfun      , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWframebuffersizefun    , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWmousebuttonfun        , Ptr <GLFWwindow> -> CInt -> CInt -> CInt ->         IO ()
#callback GLFWcursorposfun          , Ptr <GLFWwindow> -> CDouble -> CDouble ->           IO ()
#callback GLFWcursorenterfun        , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWscrollfun             , Ptr <GLFWwindow> -> CDouble -> CDouble ->           IO ()
#callback GLFWkeyfun                , Ptr <GLFWwindow> -> CInt -> CInt -> CInt -> CInt -> IO ()
#callback GLFWcharfun               , Ptr <GLFWwindow> -> CUInt ->                        IO ()
#callback GLFWmonitorfun            , Ptr <GLFWmonitor> -> CInt ->                        IO ()
#callback GLFWwindowcontentscalefun , Ptr <GLFWwindow> -> CFloat -> CFloat ->             IO ()
#callback GLFWwindowmaximizefun     , Ptr <GLFWwindow> -> CInt ->                         IO ()

#starttype GLFWvidmode
#field width       , CInt
#field height      , CInt
#field redBits     , CInt
#field greenBits   , CInt
#field blueBits    , CInt
#field refreshRate , CInt
#stoptype

#starttype GLFWgammaramp
#field red   , Ptr CUShort
#field green , Ptr CUShort
#field blue  , Ptr CUShort
#field size  , CUInt
#stoptype

#ccall glfwInit                       ,                                                                       IO CInt
#ccall glfwTerminate                  ,                                                                       IO ()
#ccall glfwGetVersion                 , Ptr CInt -> Ptr CInt -> Ptr CInt ->                                   IO ()
#ccall glfwGetVersionString           ,                                                                       IO (Ptr CChar)
#ccall glfwSetErrorCallback           , <GLFWerrorfun> ->                                                     IO <GLFWerrorfun>
#ccall glfwGetMonitors                , Ptr CInt ->                                                           IO (Ptr (Ptr <GLFWmonitor>))
#ccall glfwGetPrimaryMonitor          ,                                                                       IO (Ptr <GLFWmonitor>)
#ccall glfwGetMonitorPos              , Ptr <GLFWmonitor> -> Ptr CInt -> Ptr CInt ->                          IO ()
#ccall glfwGetMonitorPhysicalSize     , Ptr <GLFWmonitor> -> Ptr CInt -> Ptr CInt ->                          IO ()
#ccall glfwGetMonitorName             , Ptr <GLFWmonitor> ->                                                  IO (Ptr CChar)
#ccall glfwSetMonitorCallback         , <GLFWmonitorfun>  ->                                                  IO <GLFWmonitorfun>
#ccall glfwGetVideoModes              , Ptr <GLFWmonitor> -> Ptr CInt ->                                      IO (Ptr <GLFWvidmode>)
#ccall glfwGetVideoMode               , Ptr <GLFWmonitor> ->                                                  IO (Ptr <GLFWvidmode>)
#ccall glfwSetGamma                   , Ptr <GLFWmonitor> -> CFloat ->                                        IO ()
#ccall glfwGetGammaRamp               , Ptr <GLFWmonitor> ->                                                  IO (Ptr <GLFWgammaramp>)
#ccall glfwSetGammaRamp               , Ptr <GLFWmonitor> -> Ptr <GLFWgammaramp> ->                           IO ()
#ccall glfwDefaultWindowHints         ,                                                                       IO ()
#ccall glfwWindowHint                 , CInt -> CInt ->                                                       IO ()
#ccall glfwCreateWindow               , CInt -> CInt -> Ptr CChar -> Ptr <GLFWmonitor> -> Ptr <GLFWwindow> -> IO (Ptr <GLFWwindow>)
#ccall glfwDestroyWindow              , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwWindowShouldClose          , Ptr <GLFWwindow> ->                                                   IO CInt
#ccall glfwSetWindowShouldClose       , Ptr <GLFWwindow> -> CInt ->                                           IO ()
#ccall glfwSetWindowTitle             , Ptr <GLFWwindow> -> Ptr CChar ->                                      IO ()
#ccall glfwGetWindowPos               , Ptr <GLFWwindow> -> Ptr CInt -> Ptr CInt ->                           IO ()
#ccall glfwSetWindowPos               , Ptr <GLFWwindow> -> CInt -> CInt ->                                   IO ()
#ccall glfwGetWindowSize              , Ptr <GLFWwindow> -> Ptr CInt -> Ptr CInt ->                           IO ()
#ccall glfwSetWindowSize              , Ptr <GLFWwindow> -> CInt -> CInt ->                                   IO ()
#ccall glfwGetFramebufferSize         , Ptr <GLFWwindow> -> Ptr CInt -> Ptr CInt ->                           IO ()
#ccall glfwIconifyWindow              , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwRestoreWindow              , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwShowWindow                 , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwHideWindow                 , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwGetWindowMonitor           , Ptr <GLFWwindow> ->                                                   IO (Ptr <GLFWmonitor>)
#ccall glfwGetWindowAttrib            , Ptr <GLFWwindow> -> CInt ->                                           IO CInt
#ccall glfwSetWindowUserPointer       , Ptr <GLFWwindow> -> Ptr () ->                                         IO ()
#ccall glfwGetWindowUserPointer       , Ptr <GLFWwindow> ->                                                   IO (Ptr ())
#ccall glfwSetWindowPosCallback       , Ptr <GLFWwindow> -> <GLFWwindowposfun> ->                             IO <GLFWwindowposfun>
#ccall glfwSetWindowSizeCallback      , Ptr <GLFWwindow> -> <GLFWwindowsizefun> ->                            IO <GLFWwindowsizefun>
#ccall glfwSetWindowCloseCallback     , Ptr <GLFWwindow> -> <GLFWwindowclosefun> ->                           IO <GLFWwindowclosefun>
#ccall glfwSetWindowRefreshCallback   , Ptr <GLFWwindow> -> <GLFWwindowrefreshfun> ->                         IO <GLFWwindowrefreshfun>
#ccall glfwSetWindowFocusCallback     , Ptr <GLFWwindow> -> <GLFWwindowfocusfun> ->                           IO <GLFWwindowfocusfun>
#ccall glfwSetWindowIconifyCallback   , Ptr <GLFWwindow> -> <GLFWwindowiconifyfun> ->                         IO <GLFWwindowiconifyfun>
#ccall glfwSetFramebufferSizeCallback , Ptr <GLFWwindow> -> <GLFWframebuffersizefun> ->                       IO <GLFWframebuffersizefun>
#ccall glfwPollEvents                 ,                                                                       IO ()
#ccall glfwWaitEvents                 ,                                                                       IO ()
#ccall glfwPostEmptyEvent             ,                                                                       IO ()
#ccall glfwGetInputMode               , Ptr <GLFWwindow> -> CInt ->                                           IO CInt
#ccall glfwSetInputMode               , Ptr <GLFWwindow> -> CInt -> CInt ->                                   IO ()
#ccall glfwGetKey                     , Ptr <GLFWwindow> -> CInt ->                                           IO CInt
#ccall glfwGetMouseButton             , Ptr <GLFWwindow> -> CInt ->                                           IO CInt
#ccall glfwGetCursorPos               , Ptr <GLFWwindow> -> Ptr CDouble -> Ptr CDouble ->                     IO ()
#ccall glfwSetCursorPos               , Ptr <GLFWwindow> -> CDouble -> CDouble ->                             IO ()
#ccall glfwSetKeyCallback             , Ptr <GLFWwindow> -> <GLFWkeyfun> ->                                   IO <GLFWkeyfun>
#ccall glfwSetCharCallback            , Ptr <GLFWwindow> -> <GLFWcharfun> ->                                  IO <GLFWcharfun>
#ccall glfwSetMouseButtonCallback     , Ptr <GLFWwindow> -> <GLFWmousebuttonfun> ->                           IO <GLFWmousebuttonfun>
#ccall glfwSetCursorPosCallback       , Ptr <GLFWwindow> -> <GLFWcursorposfun> ->                             IO <GLFWcursorposfun>
#ccall glfwSetCursorEnterCallback     , Ptr <GLFWwindow> -> <GLFWcursorenterfun> ->                           IO <GLFWcursorenterfun>
#ccall glfwSetScrollCallback          , Ptr <GLFWwindow> -> <GLFWscrollfun> ->                                IO <GLFWscrollfun>
#ccall glfwJoystickPresent            , CInt ->                                                               IO CInt
#ccall glfwGetJoystickAxes            , CInt -> Ptr CInt ->                                                   IO (Ptr CFloat)
#ccall glfwGetJoystickButtons         , CInt -> Ptr CInt ->                                                   IO (Ptr CUChar)
#ccall glfwGetJoystickName            , CInt ->                                                               IO (Ptr CChar)
#ccall glfwSetClipboardString         , Ptr <GLFWwindow> -> Ptr CChar ->                                      IO ()
#ccall glfwGetClipboardString         , Ptr <GLFWwindow> ->                                                   IO (Ptr CChar)
#ccall glfwGetTime                    ,                                                                       IO CDouble
#ccall glfwSetTime                    , CDouble ->                                                            IO ()
#ccall glfwMakeContextCurrent         , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwGetCurrentContext          ,                                                                       IO (Ptr <GLFWwindow>)
#ccall glfwSwapBuffers                , Ptr <GLFWwindow> ->                                                   IO ()
#ccall glfwSwapInterval               , CInt ->                                                               IO ()
#ccall glfwExtensionSupported         , Ptr CChar ->                                                          IO CInt
#ccall glfwGetProcAddress             , Ptr CChar ->                                                          IO <GLFWglproc>

--------------------------------------------------------------------------------
-- GLFW 3.1 additions
--------------------------------------------------------------------------------

#num GLFW_ARROW_CURSOR
#num GLFW_IBEAM_CURSOR
#num GLFW_CROSSHAIR_CURSOR
#num GLFW_HAND_CURSOR
#num GLFW_HRESIZE_CURSOR
#num GLFW_VRESIZE_CURSOR
#num GLFW_DONT_CARE
#num GLFW_DOUBLEBUFFER
#num GLFW_AUTO_ICONIFY
#num GLFW_FLOATING
#num GLFW_CONTEXT_RELEASE_BEHAVIOR
#num GLFW_ANY_RELEASE_BEHAVIOR
#num GLFW_RELEASE_BEHAVIOR_FLUSH
#num GLFW_RELEASE_BEHAVIOR_NONE

#starttype GLFWimage
#field width  , CInt
#field height , CInt
#field pixels , Ptr CUChar
#stoptype

#opaque_t GLFWcursor
deriving instance Typeable C'GLFWcursor
deriving instance Data     C'GLFWcursor

#callback GLFWdropfun , Ptr <GLFWwindow> -> CInt -> Ptr (Ptr CChar) -> IO ()
#callback GLFWcharmodsfun , Ptr <GLFWwindow> -> CUInt -> CInt -> IO ()

#ccall glfwCreateCursor , Ptr <GLFWimage> -> CInt -> CInt -> IO (Ptr <GLFWcursor>)
#ccall glfwCreateStandardCursor , CInt -> IO (Ptr <GLFWcursor>)
#ccall glfwSetCursor , Ptr <GLFWwindow> -> Ptr <GLFWcursor> -> IO ()
#ccall glfwDestroyCursor , Ptr <GLFWcursor> -> IO ()
#ccall glfwSetDropCallback , Ptr <GLFWwindow> -> <GLFWdropfun> -> IO <GLFWdropfun>
#ccall glfwSetCharModsCallback , Ptr <GLFWwindow> -> <GLFWcharmodsfun> -> IO <GLFWcharmodsfun>
#ccall glfwGetWindowFrameSize , Ptr <GLFWwindow> -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

--------------------------------------------------------------------------------
-- GLFW 3.2 additions
--------------------------------------------------------------------------------

#num GLFW_NO_API
#num GLFW_CONTEXT_CREATION_API
#num GLFW_NATIVE_CONTEXT_API
#num GLFW_EGL_CONTEXT_API
#num GLFW_CONTEXT_NO_ERROR
#num GLFW_TRUE
#num GLFW_FALSE
#num GLFW_MAXIMIZED

#ccall glfwFocusWindow , Ptr <GLFWwindow> -> IO ()
#ccall glfwMaximizeWindow , Ptr <GLFWwindow> -> IO ()
#ccall glfwSetWindowMonitor , Ptr <GLFWwindow> -> Ptr <GLFWmonitor> -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall glfwSetWindowIcon , Ptr <GLFWwindow> -> CInt -> Ptr <GLFWimage> -> IO ()
#ccall glfwWaitEventsTimeout , CDouble -> IO ()
#ccall glfwSetWindowSizeLimits , Ptr <GLFWwindow> -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall glfwSetWindowAspectRatio , Ptr <GLFWwindow> -> CInt -> CInt -> IO ()
#ccall glfwGetKeyName , CInt -> CInt -> IO CString
#ccall glfwGetTimerValue , IO Word64
#ccall glfwGetTimerFrequency , IO Word64

#callback GLFWjoystickfun , CInt -> CInt -> IO ()
#ccall glfwSetJoystickCallback , <GLFWjoystickfun> -> IO <GLFWjoystickfun>

#ccall glfwVulkanSupported , IO CInt
#ccall glfwGetRequiredInstanceExtensions , Ptr Word32 -> IO (Ptr CString)

-- GLFW prevents the declaration of some of the Vulkan functions in its header
-- glfw3.h if it cannot find the required Vulkan headers. As of now (4/2018),
-- these functions are still *defined* in vulkan.c, which is built regardless of
-- whether or not the Vulkan headers were found. Internally, the required types
-- are defined locally in order to make the build work, and h2c will still link
-- against them without a problem. Whether or not you have a valid Vulkan
-- implementation might not be clear though. Nothing is preventing the glfw3
-- authors from hiding the Vulkan functions without valid headers, which could
-- cause some pain in the future, so use the following functions with caution.
-- See commit @521d161af85047 for a way to deal with this more annoyingly.
#ccall glfwGetInstanceProcAddress               , Ptr vkInstance -> CString -> IO (FunPtr vkProc)
#ccall glfwGetPhysicalDevicePresentationSupport , Ptr vkInstance -> Ptr vkPhysicalDevice -> Word32 -> IO CInt
#ccall glfwCreateWindowSurface                  , Ptr vkInstance -> Ptr <GLFWwindow> -> Ptr vkAllocationCallbacks -> Ptr vkSurfaceKHR -> IO Int32

--------------------------------------------------------------------------------
-- GLFW 3.3 additions
--------------------------------------------------------------------------------

#num GLFW_NO_ERROR
#num GLFW_GAMEPAD_BUTTON_A
#num GLFW_GAMEPAD_BUTTON_B
#num GLFW_GAMEPAD_BUTTON_X
#num GLFW_GAMEPAD_BUTTON_Y
#num GLFW_GAMEPAD_BUTTON_LEFT_BUMPER
#num GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER
#num GLFW_GAMEPAD_BUTTON_BACK
#num GLFW_GAMEPAD_BUTTON_START
#num GLFW_GAMEPAD_BUTTON_GUIDE
#num GLFW_GAMEPAD_BUTTON_LEFT_THUMB
#num GLFW_GAMEPAD_BUTTON_RIGHT_THUMB
#num GLFW_GAMEPAD_BUTTON_DPAD_UP
#num GLFW_GAMEPAD_BUTTON_DPAD_RIGHT
#num GLFW_GAMEPAD_BUTTON_DPAD_DOWN
#num GLFW_GAMEPAD_BUTTON_DPAD_LEFT
#num GLFW_GAMEPAD_BUTTON_LAST
#num GLFW_GAMEPAD_BUTTON_CROSS
#num GLFW_GAMEPAD_BUTTON_CIRCLE
#num GLFW_GAMEPAD_BUTTON_SQUARE
#num GLFW_GAMEPAD_BUTTON_TRIANGLE
#num GLFW_GAMEPAD_AXIS_LEFT_X
#num GLFW_GAMEPAD_AXIS_LEFT_Y
#num GLFW_GAMEPAD_AXIS_RIGHT_X
#num GLFW_GAMEPAD_AXIS_RIGHT_Y
#num GLFW_GAMEPAD_AXIS_LEFT_TRIGGER
#num GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER
#num GLFW_GAMEPAD_AXIS_LAST

#num GLFW_HAT_CENTERED
#num GLFW_HAT_UP
#num GLFW_HAT_RIGHT
#num GLFW_HAT_DOWN
#num GLFW_HAT_LEFT
#num GLFW_HAT_RIGHT_UP
#num GLFW_HAT_RIGHT_DOWN
#num GLFW_HAT_LEFT_UP
#num GLFW_HAT_LEFT_DOWN

-- Init hints
#num GLFW_JOYSTICK_HAT_BUTTONS
#num GLFW_COCOA_CHDIR_RESOURCES
#num GLFW_COCOA_MENUBAR

-- Window hints
#num GLFW_TRANSPARENT_FRAMEBUFFER
#num GLFW_CENTER_CURSOR
#num GLFW_FOCUS_ON_SHOW
#num GLFW_SCALE_TO_MONITOR
#num GLFW_COCOA_RETINA_FRAMEBUFFER
#num GLFW_COCOA_FRAME_NAME
#num GLFW_COCOA_GRAPHICS_SWITCHING
#num GLFW_X11_CLASS_NAME

#starttype GLFWgamepadstate
#array_field buttons , CUChar
#array_field axes    , CFloat
#stoptype

#ccall glfwGetError                      , Ptr CString -> IO CInt
#ccall glfwUpdateGamepadMappings         , CString -> IO CInt
#ccall glfwJoystickIsGamepad             , CInt -> IO CInt
#ccall glfwGetJoystickGUID               , CInt -> IO CString
#ccall glfwGetGamepadName                , CInt -> IO CString
#ccall glfwGetGamepadState               , CInt -> Ptr <GLFWgamepadstate> -> IO CInt
#ccall glfwGetWindowContentScale         , Ptr <GLFWwindow> -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall glfwGetMonitorContentScale        , Ptr <GLFWmonitor> -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall glfwSetWindowContentScaleCallback , <GLFWwindowcontentscalefun> -> IO <GLFWwindowcontentscalefun>
#ccall glfwRequestWindowAttention        , Ptr <GLFWwindow> -> IO ()
#ccall glfwGetMonitorWorkarea            , Ptr <GLFWmonitor> -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall glfwGetKeyScancode                , CInt -> IO CInt
#ccall glfwSetWindowMaximizeCallback     , <GLFWwindowmaximizefun> -> IO <GLFWwindowmaximizefun>
#ccall glfwSetWindowAttrib               , Ptr <GLFWwindow> -> CInt -> CInt -> IO ()
#ccall glfwGetJoystickHats               , CInt -> Ptr CInt -> IO (Ptr CUChar)
#ccall glfwInitHint                      , CInt -> CInt -> IO ()
#ccall glfwWindowHintString              , CInt -> CString -> IO ()
#ccall glfwGetWindowOpacity              , Ptr <GLFWwindow> -> IO CFloat
#ccall glfwSetWindowOpacity              , Ptr <GLFWwindow> -> CFloat -> IO ()
#ccall glfwSetMonitorUserPointer         , Ptr <GLFWmonitor> -> Ptr () -> IO ()
#ccall glfwGetMonitorUserPointer         , Ptr <GLFWmonitor> -> IO (Ptr ())
#ccall glfwSetJoystickUserPointer        , CInt -> Ptr () -> IO ()
#ccall glfwGetJoystickUserPointer        , CInt -> IO (Ptr ())

--------------------------------------------------------------------------------
-- Native APIs
--------------------------------------------------------------------------------

#if defined(GLFW_EXPOSE_NATIVE_WIN32)
#ccall glfwGetWin32Adapter , Ptr <GLFWwindow> -> IO CString
#ccall glfwGetWin32Monitor , Ptr <GLFWwindow> -> IO CString
#ccall glfwGetWin32Window  , Ptr <GLFWwindow> -> IO (Ptr ())
#else

p'glfwGetWin32Adapter :: FunPtr (Ptr C'GLFWwindow -> IO CString)
p'glfwGetWin32Adapter = nullFunPtr

c'glfwGetWin32Adapter :: Ptr C'GLFWwindow -> IO CString
c'glfwGetWin32Adapter =
  error $ "c'glfwGetWin32Adapter undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetWin32Monitor :: FunPtr (Ptr C'GLFWwindow -> IO CString)
p'glfwGetWin32Monitor = nullFunPtr

c'glfwGetWin32Monitor :: Ptr C'GLFWwindow -> IO CString
c'glfwGetWin32Monitor =
  error $ "c'glfwGetWin32Monitor undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetWin32Window :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetWin32Window = nullFunPtr

c'glfwGetWin32Window ::  Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetWin32Window =
  error $ "c'glfwGetWin32Window undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_WGL)
#ccall glfwGetWGLContext , Ptr <GLFWwindow> -> IO (Ptr ())
#else
p'glfwGetWGLContext :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetWGLContext = nullFunPtr

c'glfwGetWGLContext :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetWGLContext =
  error $ "c'glfwGetWGLContext undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_COCOA)
#ccall glfwGetCocoaMonitor , Ptr <GLFWwindow> -> IO (Ptr Word32)
#ccall glfwGetCocoaWindow , Ptr <GLFWwindow> -> IO (Ptr ())
#else
p'glfwGetCocoaMonitor :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr Word32))
p'glfwGetCocoaMonitor = nullFunPtr

c'glfwGetCocoaMonitor :: Ptr C'GLFWwindow -> IO (Ptr Word32)
c'glfwGetCocoaMonitor =
  error $ "c'glfwGetCocoaMonitor undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetCocoaWindow :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetCocoaWindow = nullFunPtr

c'glfwGetCocoaWindow :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetCocoaWindow =
  error $ "c'glfwGetCocoaWindow undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_NSGL)
#ccall glfwGetNSGLContext , Ptr <GLFWwindow> -> IO (Ptr ())
#else
p'glfwGetNSGLContext :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetNSGLContext = nullFunPtr

c'glfwGetNSGLContext :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetNSGLContext =
  error $ "c'glfwGetNSGLContext undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_X11)
#ccall glfwGetX11Display , Ptr <GLFWwindow> -> IO (Ptr display)
#ccall glfwGetX11Adapter , Ptr <GLFWwindow> -> IO Word64
#ccall glfwGetX11Monitor , Ptr <GLFWwindow> -> IO Word64
#ccall glfwGetX11Window  , Ptr <GLFWwindow> -> IO Word64

--------------------------------------------------------------------------------
-- 3.3 Additions
--------------------------------------------------------------------------------
#ccall glfwSetX11SelectionString , CString -> IO ()
#ccall glfwGetX11SelectionString , IO CString

#else
p'glfwGetX11Display :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr display))
p'glfwGetX11Display = nullFunPtr

c'glfwGetX11Display :: Ptr C'GLFWwindow -> IO (Ptr display)
c'glfwGetX11Display =
  error $ "c'glfwGetX11Display undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetX11Adapter :: FunPtr (Ptr C'GLFWwindow -> IO Word64)
p'glfwGetX11Adapter = nullFunPtr

c'glfwGetX11Adapter :: Ptr C'GLFWwindow -> IO Word64
c'glfwGetX11Adapter =
  error $ "c'glfwGetX11Adapter undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetX11Monitor :: FunPtr (Ptr C'GLFWwindow -> IO Word64)
p'glfwGetX11Monitor = nullFunPtr

c'glfwGetX11Monitor :: Ptr C'GLFWwindow -> IO Word64
c'glfwGetX11Monitor =
  error $ "c'glfwGetX11Monitor undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetX11Window :: FunPtr (Ptr C'GLFWwindow -> IO Word64)
p'glfwGetX11Window = nullFunPtr

c'glfwGetX11Window ::  Ptr C'GLFWwindow -> IO Word64
c'glfwGetX11Window =
  error $ "c'glfwGetX11Window undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetX11SelectionString :: FunPtr (IO CString)
p'glfwGetX11SelectionString = nullFunPtr

c'glfwGetX11SelectionString :: IO CString
c'glfwGetX11SelectionString =
  error $ "c'glfwGetX11SelectionString undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwSetX11SelectionString :: FunPtr (CString -> IO ())
p'glfwSetX11SelectionString = nullFunPtr

c'glfwSetX11SelectionString :: CString -> IO ()
c'glfwSetX11SelectionString =
  error $ "c'glfwSetX11SelectionString undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_GLX)
#ccall glfwGetGLXContext , Ptr <GLFWwindow> -> IO (Ptr ())
#ccall glfwGetGLXWindow  , Ptr <GLFWwindow> -> IO Word64
#else
p'glfwGetGLXContext :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetGLXContext = nullFunPtr

c'glfwGetGLXContext :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetGLXContext =
  error $ "c'glfwGetGLXContext undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetGLXWindow :: FunPtr (Ptr C'GLFWwindow -> IO Word64)
p'glfwGetGLXWindow = nullFunPtr

c'glfwGetGLXWindow ::  Ptr C'GLFWwindow -> IO Word64
c'glfwGetGLXWindow =
  error $ "c'glfwGetGLXWindow undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_WAYLAND)
#ccall glfwGetWaylandDisplay , IO (Ptr wl_display)
#ccall glfwGetWaylandMonitor , Ptr <GLFWwindow> -> IO (Ptr wl_output)
#ccall glfwGetWaylandWindow , Ptr <GLFWwindow> -> IO (Ptr wl_surface)
#else
p'glfwGetWaylandDisplay :: FunPtr (IO (Ptr wl_display))
p'glfwGetWaylandDisplay = nullFunPtr

c'glfwGetWaylandDisplay :: IO (Ptr wl_display)
c'glfwGetWaylandDisplay =
  error $ "c'glfwGetWaylandDisplay undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetWaylandMonitor :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr wl_output))
p'glfwGetWaylandMonitor = nullFunPtr

c'glfwGetWaylandMonitor :: Ptr C'GLFWwindow -> IO (Ptr wl_output)
c'glfwGetWaylandMonitor =
  error $ "c'glfwGetWaylandMonitor undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetWaylandWindow :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr wl_surface))
p'glfwGetWaylandWindow = nullFunPtr

c'glfwGetWaylandWindow :: Ptr C'GLFWwindow -> IO (Ptr wl_surface)
c'glfwGetWaylandWindow =
  error $ "c'glfwGetWaylandWindow undefined! -- "
       ++ "Did you use the wrong glfw3native API?"
#endif

#if defined(GLFW_EXPOSE_NATIVE_EGL)
#ccall glfwGetEGLDisplay , IO (Ptr ())
#ccall glfwGetEGLContext , Ptr <GLFWwindow> -> IO (Ptr ())
#ccall glfwGetEGLSurface , Ptr <GLFWwindow> -> IO (Ptr ())
#else
p'glfwGetEGLDisplay :: FunPtr (IO (Ptr ()))
p'glfwGetEGLDisplay = nullFunPtr

c'glfwGetEGLDisplay :: IO (Ptr ())
c'glfwGetEGLDisplay =
  error $ "c'glfwGetEGLDisplay undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetEGLContext :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetEGLContext = nullFunPtr

c'glfwGetEGLContext :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetEGLContext =
  error $ "c'glfwGetEGLContext undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

p'glfwGetEGLSurface :: FunPtr (Ptr C'GLFWwindow -> IO (Ptr ()))
p'glfwGetEGLSurface = nullFunPtr

c'glfwGetEGLSurface :: Ptr C'GLFWwindow -> IO (Ptr ())
c'glfwGetEGLSurface =
  error $ "c'glfwGetEGLSurface undefined! -- "
       ++ "Did you use the wrong glfw3native API?"

#endif
