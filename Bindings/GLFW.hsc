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

    #define GLFW_EXPOSE_NATIVE_X11
    #define GLFW_EXPOSE_NATIVE_GLX

  #endif

  #include <GLFW/glfw3native.h>

#endif

--------------------------------------------------------------------------------

module Bindings.GLFW where

import Prelude (Eq, IO, Num, Show)
import Prelude (($), return)

import Data.Data        (Data)
import Data.Int         (Int32)
import Data.Word        (Word32)
import Data.Typeable    (Typeable)
import Foreign.C.Types  (CChar, CUChar, CUShort)
import Foreign.C.Types  (CDouble(..), CFloat(..), CInt(..), CUInt(..), CULong(..))
import Foreign.C.String (CString)
import Foreign.Ptr      (FunPtr, Ptr, plusPtr)
import Foreign.Storable (Storable(..))
--------------------------------------------------------------------------------

#num GL_FALSE
#num GL_TRUE

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

#callback GLFWerrorfun           , CInt -> Ptr CChar ->                                IO ()
#callback GLFWwindowposfun       , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWwindowsizefun      , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWwindowclosefun     , Ptr <GLFWwindow> ->                                 IO ()
#callback GLFWwindowrefreshfun   , Ptr <GLFWwindow> ->                                 IO ()
#callback GLFWwindowfocusfun     , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWwindowiconifyfun   , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWframebuffersizefun , Ptr <GLFWwindow> -> CInt -> CInt ->                 IO ()
#callback GLFWmousebuttonfun     , Ptr <GLFWwindow> -> CInt -> CInt -> CInt ->         IO ()
#callback GLFWcursorposfun       , Ptr <GLFWwindow> -> CDouble -> CDouble ->           IO ()
#callback GLFWcursorenterfun     , Ptr <GLFWwindow> -> CInt ->                         IO ()
#callback GLFWscrollfun          , Ptr <GLFWwindow> -> CDouble -> CDouble ->           IO ()
#callback GLFWkeyfun             , Ptr <GLFWwindow> -> CInt -> CInt -> CInt -> CInt -> IO ()
#callback GLFWcharfun            , Ptr <GLFWwindow> -> CUInt ->                        IO ()
#callback GLFWmonitorfun         , Ptr <GLFWmonitor> -> CInt ->                        IO ()

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

#ccall glfwCreateCursor , Ptr <GLFWimage> -> CInt -> CInt -> IO (Ptr <GLFWcursor>)
#ccall glfwCreateStandardCursor , CInt -> IO (Ptr <GLFWcursor>)
#ccall glfwSetCursor , Ptr <GLFWwindow> -> Ptr <GLFWcursor> -> IO ()
#ccall glfwDestroyCursor , Ptr <GLFWcursor> -> IO ()
#ccall glfwSetDropCallback , Ptr <GLFWwindow> -> <GLFWdropfun> -> IO <GLFWdropfun>


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

#ccall glfwFocusWindow , Ptr <GLFWwindow> -> IO ()
#ccall glfwMaximizeWindow , Ptr <GLFWwindow> -> IO ()
#ccall glfwSetWindowMonitor , Ptr <GLFWwindow> -> Ptr <GLFWmonitor> -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall glfwSetWindowIcon , Ptr <GLFWwindow> -> CInt -> Ptr <GLFWimage> -> IO ()
#ccall glfwWaitEventsTimeout , CDouble -> IO ()
#ccall glfwSetWindowSizeLimits , Ptr <GLFWwindow> -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall glfwSetWindowAspectRatio , Ptr <GLFWwindow> -> CInt -> CInt -> IO ()
#ccall glfwGetKeyName , CInt -> CInt -> IO CString
#ccall glfwGetTimerValue , IO (CULong)
#ccall glfwGetTimerFrequency , IO (CULong)

#ccall glfwVulkanSupported , IO CInt
#ccall glfwGetRequiredInstanceExtensions , Ptr Word32 -> IO (Ptr CString)
#ccall glfwGetInstanceProcAddress , Ptr vkInstance -> CString -> IO (FunPtr vkProc)
#ccall glfwGetPhysicalDevicePresentationSupport , Ptr vkInstance -> Ptr vkPhysicalDevice -> Word32 -> IO CInt
#ccall glfwCreateWindowSurface , Ptr vkInstance ->  Ptr <GLFWwindow> -> Ptr vkAllocationCallbacks -> Ptr vkSurfaceKHR -> IO Int32
