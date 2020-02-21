-- base
import Control.Concurrent    (threadDelay)
import Control.Monad         (forM, forM_, when)
import Data.Char             (isAscii)
import Data.List             (intercalate, isPrefixOf)
import Foreign.C.String      (peekCString, withCString)
import Foreign.C.Types       (CDouble(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           (Ptr, nullPtr, nullFunPtr)
import Foreign.Storable      (Storable(..))

-- HUnit
import Test.HUnit ((@?=), (@?), assertBool, assertFailure, assertEqual)

-- test-framework
import Test.Framework (Test, defaultMain, testGroup)

-- test-framework-hunit
import Test.Framework.Providers.HUnit (testCase)

-- bindings-GLFW
import Bindings.GLFW

--------------------------------------------------------------------------------

main :: IO ()
main = do
    c'glfwInitHint c'GLFW_COCOA_CHDIR_RESOURCES c'GLFW_FALSE
    _ <- c'glfwInit

    p'mon <- c'glfwGetPrimaryMonitor

    c'glfwWindowHint c'GLFW_VISIBLE c'GLFW_FALSE
    p'win <- withCString "bindings-GLFW test" $ \p'title ->
      c'glfwCreateWindow 100 100 p'title nullPtr nullPtr
    c'glfwMakeContextCurrent p'win

    -- Mostly check for compiling
    cmcb <- mk'GLFWcharmodsfun $ \win x y ->
        putStrLn $ "Got char mods callback! " ++ show (win, x, y)
    _ <- c'glfwSetCharModsCallback p'win cmcb
    jcb <- mk'GLFWjoystickfun $ \x y ->
        putStrLn $ "Got joystick callback! " ++ show (x, y)
    _ <- c'glfwSetJoystickCallback jcb
    wcscb <- mk'GLFWwindowcontentscalefun $ \win x y ->
        putStrLn $ "Got window content scale callback! " ++ show (win, x, y)
    _ <- c'glfwSetWindowContentScaleCallback p'win wcscb

    c'glfwGetError nullPtr
      >>= assertEqual "Got inititialization error!" c'GLFW_NO_ERROR

    defaultMain $ tests p'mon p'win

    -- TODO because of how defaultMain works, this code is not reached
    c'glfwDestroyWindow p'win
    c'glfwTerminate

--------------------------------------------------------------------------------

versionMajor, versionMinor, versionRevision :: Int
versionMajor    = 3
versionMinor    = 3
versionRevision = 2

giveItTime :: IO ()
giveItTime = threadDelay 500000

joysticks :: Num a => [a]
joysticks =
    [ c'GLFW_JOYSTICK_1
    , c'GLFW_JOYSTICK_2
    , c'GLFW_JOYSTICK_3
    , c'GLFW_JOYSTICK_4
    , c'GLFW_JOYSTICK_5
    , c'GLFW_JOYSTICK_6
    , c'GLFW_JOYSTICK_7
    , c'GLFW_JOYSTICK_8
    , c'GLFW_JOYSTICK_9
    , c'GLFW_JOYSTICK_10
    , c'GLFW_JOYSTICK_11
    , c'GLFW_JOYSTICK_12
    , c'GLFW_JOYSTICK_13
    , c'GLFW_JOYSTICK_14
    , c'GLFW_JOYSTICK_15
    , c'GLFW_JOYSTICK_16
    ]

between :: Ord a => a -> (a,a) -> Bool
between n (l,h) = n >= l && n <= h

videoModeLooksValid :: C'GLFWvidmode -> Bool
videoModeLooksValid vm = and
    [ c'GLFWvidmode'width       vm `between` (0,8192)
    , c'GLFWvidmode'height      vm `between` (0,8192)
    , c'GLFWvidmode'redBits     vm `between` (0,32)
    , c'GLFWvidmode'greenBits   vm `between` (0,32)
    , c'GLFWvidmode'blueBits    vm `between` (0,32)
    , c'GLFWvidmode'refreshRate vm `between` (0,240)
    ]

--------------------------------------------------------------------------------

glfwTest :: String -> IO () -> Test
glfwTest name test = testCase name $ do
  _ <- c'glfwGetError nullPtr  -- clear last error
  test
  alloca $ \p'errMsg -> do
    errResult <- c'glfwGetError p'errMsg
    errMsg <- if errResult == c'GLFW_NO_ERROR then return "" else do
      msg <- peek p'errMsg >>= peekCString
      return $ concat ["Test '", name, "' generated error: ", msg]
    assertEqual errMsg errResult c'GLFW_NO_ERROR

tests :: Ptr C'GLFWmonitor -> Ptr C'GLFWwindow -> [Test]
tests p'mon p'win =
    [ testGroup "Initialization and version information"
      [ testCase "glfwGetVersion"       test_glfwGetVersion
      , testCase "glfwGetVersionString" test_glfwGetVersionString
      , testCase "glfwGetError"         test_glfwGetError
      , testCase "glfwRawMouseMotionSupported" test_glfwRawMouseMotionSupported
      ]
    , testGroup "Monitor handling"
      [ glfwTest "glfwGetMonitors"              test_glfwGetMonitors
      , glfwTest "glfwGetPrimaryMonitor"        test_glfwGetPrimaryMonitor
      , glfwTest "glfwGetMonitorContentScale" $ test_glfwGetMonitorContentScale p'mon
      , glfwTest "glfwGetMonitorPos"          $ test_glfwGetMonitorPos p'mon
      , glfwTest "glfwGetMonitorPhysicalSize" $ test_glfwGetMonitorPhysicalSize p'mon
      , glfwTest "glfwGetMonitorName"         $ test_glfwGetMonitorName p'mon
      , glfwTest "glfwGetMonitorWorkarea"     $ test_glfwGetMonitorWorkarea p'mon
      , glfwTest "glfwGetVideoModes"          $ test_glfwGetVideoModes p'mon
      , glfwTest "glfwGetVideoMode"           $ test_glfwGetVideoMode p'mon
      , glfwTest "glfwGetGammaRamp"           $ test_glfwGetGammaRamp p'mon
      ]
    , testGroup "Window handling"
      [ glfwTest "glfwDefaultWindowHints"       test_glfwDefaultWindowHints
      , glfwTest "glfwGetWindowAttrib"        $ test_glfwGetWindowAttrib p'win
      , glfwTest "glfwSetWindowAttrib"        $ test_glfwSetWindowAttrib p'win
      , glfwTest "window close flag"          $ test_window_close_flag p'win
      , glfwTest "glfwSetWindowTitle"         $ test_glfwSetWindowTitle p'win
      , glfwTest "window pos"                 $ test_window_pos p'win
      , glfwTest "window size"                $ test_window_size p'win
      , glfwTest "glfwGetWindowContentSize"   $ test_glfwGetWindowContentScale p'win
      , glfwTest "glfwGetWindowFrameSize"     $ test_glfwGetWindowFrameSize p'win
      , glfwTest "glfwGetFramebufferSize"     $ test_glfwGetFramebufferSize p'win
      , glfwTest "iconification"              $ test_iconification p'win
      -- , glfwTest "show/hide"                  $ test_show_hide p'win
      , glfwTest "glfwGetWindowMonitor"       $ test_glfwGetWindowMonitor p'win p'mon
      , glfwTest "glfwSetWindowMonitor"       $ test_glfwSetWindowMonitor p'win p'mon
      , glfwTest "glfwSetWindowIcon"          $ test_glfwSetWindowIcon p'win
      , glfwTest "glfwSetWindowOpacity"       $ test_glfwSetWindowOpacity p'win
      , glfwTest "glfwMaximizeWindow"         $ test_glfwMaximizeWindow p'win
      , glfwTest "glfwSetWindowSizeLimits"    $ test_glfwSetWindowSizeLimits p'win
      , glfwTest "glfwSetWindowAspectRatio"   $ test_glfwSetWindowAspectRatio p'win
      , glfwTest "glfwFocusWindow"            $ test_glfwFocusWindow p'win
      , glfwTest "glfwRequestWindowAttention" $ test_glfwRequestWindowAttention p'win
      , glfwTest "cursor pos"                 $ test_cursor_pos p'win
      , glfwTest "glfwPollEvents"               test_glfwPollEvents
      , glfwTest "glfwWaitEvents"               test_glfwWaitEvents
      , glfwTest "glfwWaitEventsTimeout"        test_glfwWaitEventsTimeout
      ]
    , testGroup "Input handling"
      [ glfwTest "glfwJoystickPresent"    test_glfwJoystickPresent
      , glfwTest "glfwGetJoystickAxes"    test_glfwGetJoystickAxes
      , glfwTest "glfwGetJoystickButtons" test_glfwGetJoystickButtons
      , glfwTest "glfwGetJoystickHats"    test_glfwGetJoystickHats
      , glfwTest "glfwGetJoystickName"    test_glfwGetJoystickName
      , glfwTest "glfwGetJoystickGUID"    test_glfwGetJoystickGUID
      , glfwTest "glfwGetGamepadState"    test_glfwGetGamepadState
      , glfwTest "glfwGetKeyName"         test_glfwGetKeyName
      , glfwTest "glfwGetKeyScancode"     test_glfwGetKeyScancode
      ]
    , testGroup "Time"
      [ glfwTest "glfwGetTime"               test_glfwGetTime
      , glfwTest "glfwSetTime"               test_glfwSetTime
      , glfwTest "glfwGetTimerValue"         test_glfwGetTimerValue
      , glfwTest "glfwSetTimerFrequency"     test_glfwGetTimerFrequency
      ]
    , testGroup "Context"
      [ glfwTest "glfwGetCurrentContext"  $ test_glfwGetCurrentContext p'win
      , glfwTest "glfwSwapBuffers"        $ test_glfwSwapBuffers p'win
      , glfwTest "glfwSwapInterval"         test_glfwSwapInterval
      , glfwTest "glfwExtensionSupported"   test_glfwExtensionSupported
      ]
    , testGroup "Clipboard"
      [ glfwTest "clipboard" $ test_clipboard p'win
      ]
    , testGroup "Vulkan"
      [ glfwTest "glfwVulkanSupported"                          test_glfwVulkanSupported
      , glfwTest "glfwGetRequiredInstanceExtensions"            test_glfwGetRequiredInstanceExtensions
      , glfwTest "glfwGetInstanceProcAddress"                   test_glfwGetInstanceProcAddress
      , glfwTest "glfwGetPhysicalDevicePresentationSupport"     test_glfwGetPhysicalDevicePresentationSupport
      , glfwTest "glfwCreateWindowSurface"                    $ test_glfwCreateWindowSurface p'win
      ]
    ]

--------------------------------------------------------------------------------

test_glfwGetVersion :: IO ()
test_glfwGetVersion =
    alloca $ \p'v0 ->
    alloca $ \p'v1 ->
    alloca $ \p'v2 -> do
        c'glfwGetVersion p'v0 p'v1 p'v2
        v0 <- peek p'v0
        v1 <- peek p'v1
        v2 <- peek p'v2
        v0 @?= fromIntegral versionMajor
        v1 @?= fromIntegral versionMinor
        v2 @?= fromIntegral versionRevision

test_glfwGetVersionString :: IO ()
test_glfwGetVersionString = do
    p'vs <- c'glfwGetVersionString
    if p'vs == nullPtr
      then assertFailure ""
      else do
          vs <- peekCString p'vs
          assertBool "" $ v `isPrefixOf` vs
        where
          v = intercalate "." $ map show [versionMajor, versionMinor, versionRevision]

test_glfwGetError :: IO ()
test_glfwGetError =
  alloca $ \p'err ->
  c'glfwGetError p'err >>= assertEqual "Discovered GLFW error!" c'GLFW_NO_ERROR

test_glfwRawMouseMotionSupported :: IO ()
test_glfwRawMouseMotionSupported = c'glfwRawMouseMotionSupported >> return ()

--------------------------------------------------------------------------------

test_glfwGetMonitors :: IO ()
test_glfwGetMonitors =
    alloca $ \p'n -> do
        p'mons <- c'glfwGetMonitors p'n
        n <- peek p'n
        if p'mons == nullPtr || n <= 0
          then assertFailure ""
          else do
              mons <- peekArray (fromIntegral n) p'mons
              assertBool "" $ not $ null mons

test_glfwGetPrimaryMonitor :: IO ()
test_glfwGetPrimaryMonitor = do
    p'mon <- c'glfwGetPrimaryMonitor
    assertBool "" $ p'mon /= nullPtr

test_glfwGetMonitorContentScale :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetMonitorContentScale p'mon =
    alloca $ \p'x ->
    alloca $ \p'y -> do
        c'glfwGetMonitorContentScale p'mon p'x p'y
        x <- peek p'x
        y <- peek p'y
        assertBool "Monitor content scale x is defined" $ x > 0
        assertBool "Monitor content scale y is defined" $ y > 0

test_glfwGetMonitorPos :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetMonitorPos p'mon =
    alloca $ \p'x ->
    alloca $ \p'y -> do
        c'glfwGetMonitorPos p'mon p'x p'y
        x <- peek p'x
        y <- peek p'y
        assertBool "" $ x >= 0
        assertBool "" $ y >= 0

test_glfwGetMonitorPhysicalSize :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetMonitorPhysicalSize p'mon =
    alloca $ \p'w ->
    alloca $ \p'h -> do
        c'glfwGetMonitorPhysicalSize p'mon p'w p'h
        w <- peek p'w
        h <- peek p'h
        assertBool "" $ w `between` (0, 1000)
        assertBool "" $ h `between` (0,  500)

test_glfwGetMonitorName :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetMonitorName p'mon = do
    p'name <- c'glfwGetMonitorName p'mon
    if p'name == nullPtr
      then assertFailure ""
      else do
          name <- peekCString p'name
          assertBool "" $ length name `between` (0, 20)
          assertBool "" $ all isAscii name

test_glfwGetMonitorWorkarea :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetMonitorWorkarea p'mon =
    alloca $ \p'xpos ->
    alloca $ \p'ypos ->
    alloca $ \p'w ->
    alloca $ \p'h -> do
        c'glfwGetMonitorWorkarea p'mon p'xpos p'ypos p'w p'h
        xpos <- peek p'xpos
        ypos <- peek p'ypos
        w <- peek p'w
        h <- peek p'h
        assertBool "Workarea xpos not negative" $ xpos >= 0
        assertBool "Workarea ypos not negative" $ ypos >= 0
        assertBool "Workarea width is positive" $ w > 0
        assertBool "Workarea height is positive" $ h > 0

test_glfwGetVideoModes :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetVideoModes p'mon =
    alloca $ \p'n -> do
        p'vms <- c'glfwGetVideoModes p'mon p'n
        n <- fromIntegral `fmap` peek p'n
        if p'vms == nullPtr || n <= 0
          then assertFailure ""
          else do
              vms <- peekArray n p'vms
              assertBool "" $ all videoModeLooksValid vms

test_glfwGetVideoMode :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetVideoMode p'mon = do
    p'vm <- c'glfwGetVideoMode p'mon
    if p'vm == nullPtr
      then assertFailure ""
      else do
          vm <- peek p'vm
          assertBool "" $ videoModeLooksValid vm

test_glfwGetGammaRamp :: Ptr C'GLFWmonitor -> IO ()
test_glfwGetGammaRamp p'mon = do
    p'gr <- c'glfwGetGammaRamp p'mon
    if p'gr == nullPtr
      then assertFailure ""
      else do
          gr <- peek p'gr
          let p'rs = c'GLFWgammaramp'red   gr
              p'gs = c'GLFWgammaramp'green gr
              p'bs = c'GLFWgammaramp'blue  gr
              cn   = c'GLFWgammaramp'size  gr
              n    = fromIntegral cn
          if nullPtr `elem` [p'rs, p'gs, p'bs]
            then assertFailure ""
            else do
                rs <- peekArray n p'rs
                gs <- peekArray n p'gs
                bs <- peekArray n p'bs
                let rsl = length rs
                    gsl = length gs
                    bsl = length bs
                assertBool "" $ rsl > 0 && rsl == gsl && gsl == bsl

--------------------------------------------------------------------------------

test_glfwDefaultWindowHints :: IO ()
test_glfwDefaultWindowHints =
    c'glfwDefaultWindowHints

test_window_close_flag :: Ptr C'GLFWwindow -> IO ()
test_window_close_flag p'win = do
    r0 <- c'glfwWindowShouldClose p'win
    r0 @?= c'GLFW_FALSE

    c'glfwSetWindowShouldClose p'win c'GLFW_TRUE
    r1 <- c'glfwWindowShouldClose p'win
    r1 @?= c'GLFW_TRUE

    c'glfwSetWindowShouldClose p'win c'GLFW_FALSE
    r2 <- c'glfwWindowShouldClose p'win
    r2 @?= c'GLFW_FALSE

test_glfwSetWindowTitle :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowTitle p'win =
    withCString "some new title" $
      c'glfwSetWindowTitle p'win

-- This is a little strange. Depending on your window manager, etc, after
-- setting the window position to (x,y), the actual new window position might
-- be (x+5,y+5) due to borders. So we just check for consistency.
test_window_pos :: Ptr C'GLFWwindow -> IO ()
test_window_pos p'win = do
    let x = 17
        y = 37
        xoff = 53
        yoff = 149
    (x0, y0, dx0, dy0) <- setGet  x        y
    (x1, y1, dx1, dy1) <- setGet (x+xoff) (y+yoff)
    dx0 @?= dx1
    dy0 @?= dy1
    x1 - x0 @?= xoff
    y1 - y0 @?= yoff
  where
    setGet :: Int -> Int -> IO (Int, Int, Int, Int)
    setGet x0 y0 = do
        c'glfwSetWindowPos p'win (fromIntegral x0) (fromIntegral y0)
        c'glfwSwapBuffers p'win
        giveItTime
        alloca $ \p'x1 ->
          alloca $ \p'y1 -> do
              c'glfwGetWindowPos p'win p'x1 p'y1
              x1 <- fromIntegral `fmap` peek p'x1
              y1 <- fromIntegral `fmap` peek p'y1
              let (dx, dy) = (x0-x1, y0-y1)
              return (x1, y1, dx, dy)

test_window_size :: Ptr C'GLFWwindow -> IO ()
test_window_size p'win = do
    let w = 177
        h = 372
    c'glfwSetWindowSize p'win w h
    giveItTime
    alloca $ \p'w' ->
      alloca $ \p'h' -> do
          c'glfwGetWindowSize p'win p'w' p'h'
          w' <- fromIntegral `fmap` peek p'w'
          h' <- fromIntegral `fmap` peek p'h'
          w' @?= w
          h' @?= h

-- Really all we can say here is that we likely have a title bar, so just check
-- that the 'frame' around the top edge is > 0.
test_glfwGetWindowFrameSize :: Ptr C'GLFWwindow -> IO ()
test_glfwGetWindowFrameSize p'win =
    alloca $ \p'win_frame_top -> do
        c'glfwGetWindowFrameSize p'win nullPtr p'win_frame_top nullPtr nullPtr
        top <- peek p'win_frame_top
        assertBool "Window has no frame width up top!" $ top > 0

test_glfwGetFramebufferSize :: Ptr C'GLFWwindow -> IO ()
test_glfwGetFramebufferSize p'win =
    alloca $ \p'w  ->
    alloca $ \p'h  ->
    alloca $ \p'fw ->
    alloca $ \p'fh -> do
        c'glfwGetWindowSize      p'win p'w  p'h
        c'glfwGetFramebufferSize p'win p'fw p'fh
        w  <- peek p'w
        h  <- peek p'h
        fw <- peek p'fw
        fh <- peek p'fh
        ((fw `mod` w) == 0) @? "Framebuffer width multiple of window's"
        ((fh `mod` h) == 0) @? "Framebuffer height multiple of window's"

test_iconification :: Ptr C'GLFWwindow -> IO ()
test_iconification p'win = do
    c'glfwShowWindow p'win
    r0 <- c'glfwGetWindowAttrib p'win c'GLFW_ICONIFIED
    r0 @?= c'GLFW_FALSE

    c'glfwIconifyWindow p'win
    giveItTime

    r1 <- c'glfwGetWindowAttrib p'win c'GLFW_ICONIFIED
    r1 @?= c'GLFW_TRUE

    c'glfwRestoreWindow p'win
    c'glfwHideWindow p'win

-- test_show_hide :: Ptr C'GLFWwindow -> IO ()
-- test_show_hide p'win = do
--     v0 <- c'glfwGetWindowAttrib p'win c'GLFW_VISIBLE
--     v0 @?= c'GLFW_FALSE

--     c'glfwShowWindow p'win
--     giveItTime
--     v1 <- c'glfwGetWindowAttrib p'win c'GLFW_VISIBLE
--     v1 @?= c'GLFW_TRUE

--     c'glfwHideWindow p'win
--     giveItTime
--     v2 <- c'glfwGetWindowAttrib p'win c'GLFW_VISIBLE
--     v2 @?= c'GLFW_FALSE

test_glfwGetWindowContentScale :: Ptr C'GLFWwindow -> IO ()
test_glfwGetWindowContentScale p'win =
    alloca $ \p'x ->
    alloca $ \p'y -> do
        c'glfwGetWindowContentScale p'win p'x p'y
        x <- peek p'x
        y <- peek p'y
        assertBool "Window content scale x is defined" $ x > 0
        assertBool "Window content scale y is defined" $ y > 0

test_glfwGetWindowMonitor :: Ptr C'GLFWwindow -> Ptr C'GLFWmonitor -> IO ()
test_glfwGetWindowMonitor p'win _ = do
    p'mon <- c'glfwGetWindowMonitor p'win
    p'mon @?= nullPtr

test_glfwSetWindowMonitor :: Ptr C'GLFWwindow -> Ptr C'GLFWmonitor -> IO ()
test_glfwSetWindowMonitor p'win _ = do
    c'glfwSetWindowMonitor p'win nullPtr 0 0 100 100 60

test_glfwSetWindowIcon :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowIcon p'win = do
    c'glfwSetWindowIcon p'win 0 nullPtr

test_glfwSetWindowOpacity :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowOpacity p'win = do
    let desiredOpacity = 0.27
    c'glfwSetWindowOpacity p'win desiredOpacity
    newOpacity <- c'glfwGetWindowOpacity p'win
    assertBool "Opacity is roughly the same." $
        abs (desiredOpacity - newOpacity) < 0.01
    c'glfwSetWindowOpacity p'win 1.0

test_glfwSetWindowSizeLimits :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowSizeLimits p'win = do
    c'glfwSetWindowSizeLimits p'win 640 480 1024 768

test_glfwSetWindowAspectRatio :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowAspectRatio p'win = do
    c'glfwSetWindowAspectRatio p'win c'GLFW_DONT_CARE c'GLFW_DONT_CARE

test_glfwFocusWindow :: Ptr C'GLFWwindow -> IO ()
test_glfwFocusWindow = c'glfwFocusWindow

test_glfwRequestWindowAttention :: Ptr C'GLFWwindow -> IO ()
test_glfwRequestWindowAttention = c'glfwRequestWindowAttention

-- NOTE: This test seems to fail in X11. This might be due to the asynchronous
-- nature of focus events in X. We may be able to fix it by waiting for the focus
-- event before setting the cursor position.
test_cursor_pos :: Ptr C'GLFWwindow -> IO ()
test_cursor_pos p'win =
    alloca $ \p'w   ->
    alloca $ \p'h   ->
    alloca $ \p'cx' ->
    alloca $ \p'cy' -> do
        c'glfwShowWindow p'win
        c'glfwGetWindowSize p'win p'w p'h
        w <- peek p'w
        h <- peek p'h

        -- Make sure we use integral coordinates here so that we don't run into
        -- platform-dependent differences.
        let cx :: CDouble
            cy :: CDouble
            (cx, cy) = (fromIntegral $ w `div` 2, fromIntegral $ h `div` 2)

        -- !HACK! Poll events seems to be necessary on OS X,
        -- before /and/ after glfwSetCursorPos, otherwise, the
        -- windowing system likely never receives the cursor update. This is
        -- reflected in the C version of GLFW as well, we just call it here in
        -- order to have a more robust test.

        c'glfwPollEvents

        c'glfwSetCursorPos p'win cx cy

        c'glfwPollEvents -- !HACK! see comment above

        c'glfwGetCursorPos p'win p'cx' p'cy'
        cx' <- peek p'cx'
        cy' <- peek p'cy'
        cx' @?= cx
        cy' @?= cy
        c'glfwHideWindow p'win

test_glfwGetWindowAttrib :: Ptr C'GLFWwindow -> IO ()
test_glfwGetWindowAttrib p'win = do
    let pairs =
          [ ( c'GLFW_FOCUSED,    c'GLFW_FALSE        )
          , ( c'GLFW_ICONIFIED,  c'GLFW_FALSE        )
          , ( c'GLFW_RESIZABLE,  c'GLFW_TRUE         )
          , ( c'GLFW_DECORATED,  c'GLFW_TRUE         )
          , ( c'GLFW_CLIENT_API, c'GLFW_OPENGL_API )
          ]
    rs <- mapM (c'glfwGetWindowAttrib p'win . fst) pairs
    rs @?= map snd pairs

test_glfwSetWindowAttrib :: Ptr C'GLFWwindow -> IO ()
test_glfwSetWindowAttrib p'win = do
    c'glfwSetWindowAttrib p'win c'GLFW_RESIZABLE c'GLFW_FALSE
    norsz <- c'glfwGetWindowAttrib p'win c'GLFW_RESIZABLE
    norsz @?= c'GLFW_FALSE

    c'glfwSetWindowAttrib p'win c'GLFW_RESIZABLE c'GLFW_TRUE
    rsz <- c'glfwGetWindowAttrib p'win c'GLFW_RESIZABLE
    rsz @?= c'GLFW_TRUE

test_glfwMaximizeWindow :: Ptr C'GLFWwindow -> IO ()
test_glfwMaximizeWindow p'win = do
    c'glfwShowWindow p'win
    startsMaximized <- c'glfwGetWindowAttrib p'win c'GLFW_MAXIMIZED
    startsMaximized @?= c'GLFW_FALSE

    c'glfwMaximizeWindow p'win
    giveItTime

    isMaximized <- c'glfwGetWindowAttrib p'win c'GLFW_MAXIMIZED
    isMaximized @?= c'GLFW_TRUE
    c'glfwHideWindow p'win

test_glfwPollEvents :: IO ()
test_glfwPollEvents = c'glfwPollEvents

test_glfwWaitEvents :: IO ()
test_glfwWaitEvents = c'glfwPostEmptyEvent >> c'glfwWaitEvents

test_glfwWaitEventsTimeout :: IO ()
test_glfwWaitEventsTimeout =
    -- to not slow down the test too much we set the timeout to 0.001 second :
    c'glfwWaitEventsTimeout 0.001

--------------------------------------------------------------------------------

test_glfwJoystickPresent :: IO ()
test_glfwJoystickPresent = do
    _ <- c'glfwJoystickPresent c'GLFW_JOYSTICK_1
    r <- c'glfwJoystickPresent c'GLFW_JOYSTICK_16
    r @?= c'GLFW_FALSE

test_glfwGetJoystickAxes :: IO ()
test_glfwGetJoystickAxes =
    forM_ joysticks $ \js ->
      alloca $ \p'n -> do
          p'axes <- c'glfwGetJoystickAxes js p'n
          when (p'axes /= nullPtr) $ do
              n <- fromIntegral `fmap` peek p'n
              if n <= 0
                then assertFailure ""
                else do
                    axes <- peekArray n p'axes
                    length axes @?= n

test_glfwGetJoystickButtons :: IO ()
test_glfwGetJoystickButtons =
    forM_ joysticks $ \js ->
      alloca $ \p'n -> do
          p'buttons <- c'glfwGetJoystickButtons js p'n
          when (p'buttons /= nullPtr) $ do
              n <- fromIntegral `fmap` peek p'n
              if n <= 0
                then assertFailure ""
                else do
                    buttons <- peekArray n p'buttons
                    length buttons @?= n

test_glfwGetJoystickHats :: IO ()
test_glfwGetJoystickHats =
    forM_ joysticks $ \js ->
        alloca $ \p'n -> do
            p'hats <- c'glfwGetJoystickHats js p'n
            when (p'hats /= nullPtr) $ do
                n <- fromIntegral `fmap` peek p'n
                if n <= 0
                  then assertFailure "No joystick hats??"
                  else do
                      hats <- peekArray n p'hats
                      length hats @?= n
                      forM_ hats $ assertEqual "Hat is centered" c'GLFW_HAT_CENTERED

test_glfwGetJoystickName :: IO ()
test_glfwGetJoystickName =
    forM_ joysticks $ \js -> do
        p'name <- c'glfwGetJoystickName js
        when (p'name /= nullPtr) $ do
            name <- peekCString p'name
            assertBool "" $ not $ null name

test_glfwGetJoystickGUID :: IO ()
test_glfwGetJoystickGUID =
    forM_ joysticks $ \js -> do
        p'guid <- c'glfwGetJoystickGUID js
        when (p'guid /= nullPtr) $ do
            guid <- peekCString p'guid
            assertBool "" $ not $ null guid

test_glfwGetGamepadState :: IO ()
test_glfwGetGamepadState =
    forM_ joysticks $ \js ->
        alloca $ \p'gp -> do
            gotMapping <- c'glfwGetGamepadState js p'gp
            when (gotMapping == c'GLFW_TRUE) $ do
                assertBool "Gamepad state is valid" (p'gp /= nullPtr)

                c'glfwJoystickIsGamepad js
                  >>= assertEqual "Is gamepad" c'GLFW_TRUE

                c'glfwGetGamepadName js
                  >>= peekCString
                  >>= assertBool "Gamepad has name" . not . null

                gp <- peek p'gp
                forM_ (c'GLFWgamepadstate'buttons gp) $
                  assertEqual "Button not pressed" c'GLFW_RELEASE

test_glfwGetKeyName :: IO ()
test_glfwGetKeyName =
    forM_ [c'GLFW_KEY_SLASH, c'GLFW_KEY_PERIOD] $ \k -> do
        p'name <- c'glfwGetKeyName k 0
        when (p'name /= nullPtr) $ do
            name <- peekCString p'name
            assertBool "" $ not $ null name

test_glfwGetKeyScancode :: IO ()
test_glfwGetKeyScancode = do
    forM_ [c'GLFW_KEY_SLASH, c'GLFW_KEY_PERIOD] $ \k -> do
        sc <- c'glfwGetKeyScancode k
        assertBool (mconcat ["Key ", show k, " scancode not found."]) (sc > 0)

    -- According to the docs this should work but it returns 0. This is a GLFW
    -- bug (at least on OS X).
    -- c'glfwGetKeyScancode c'GLFW_KEY_UNKNOWN >>= assertEqual "" (-1)

--------------------------------------------------------------------------------

test_glfwGetTime :: IO ()
test_glfwGetTime = do
    t <- c'glfwGetTime
    assertBool "" $ t > 0

test_glfwSetTime :: IO ()
test_glfwSetTime = do
    let t = 37 :: Double
    c'glfwSetTime (realToFrac t)
    t' <- realToFrac `fmap` c'glfwGetTime
    assertBool "" $ t' `between` (t, t+10)

test_glfwGetTimerValue :: IO ()
test_glfwGetTimerValue = do
    val <- c'glfwGetTimerValue
    assertBool "" $ val > 0

test_glfwGetTimerFrequency :: IO ()
test_glfwGetTimerFrequency = do
    freq <- c'glfwGetTimerFrequency
    assertBool "" $ freq > 0

--------------------------------------------------------------------------------

test_glfwGetCurrentContext :: Ptr C'GLFWwindow -> IO ()
test_glfwGetCurrentContext p'win = do
    p'win' <- c'glfwGetCurrentContext
    p'win' @?= p'win

test_glfwSwapBuffers :: Ptr C'GLFWwindow -> IO ()
test_glfwSwapBuffers =
    c'glfwSwapBuffers

test_glfwSwapInterval :: IO ()
test_glfwSwapInterval =
    c'glfwSwapInterval 1

test_glfwExtensionSupported :: IO ()
test_glfwExtensionSupported = do
    let pairs =
          [ ( "GL_ARB_multisample", c'GLFW_TRUE  )
          , ( "bogus",              c'GLFW_FALSE )
          ]
    rs <- forM (map fst pairs) $ \ext ->
      withCString ext c'glfwExtensionSupported
    rs @?= map snd pairs

--------------------------------------------------------------------------------

test_clipboard :: Ptr C'GLFWwindow -> IO ()
test_clipboard p'win = do
    rs <- mapM setGet ss
    rs @?= ss
  where
    ss =
      [ "abc 123 ???"
      , "xyz 456 !!!"
      ]
    setGet s = do
        withCString s $ c'glfwSetClipboardString p'win
        threadDelay 100000  -- Give it a little time
        p's' <- c'glfwGetClipboardString p'win

        -- See if we generated a known error for the clipboard, which would
        -- indicate that the format is not supported.
        errResult <- c'glfwGetError nullPtr
        if errResult == c'GLFW_FORMAT_UNAVAILABLE
          then return s
          else if errResult == c'GLFW_NO_ERROR then do
            if p's' == nullPtr
              then return ""
              else peekCString p's'
          else do
            assertFailure "Unexpected error from clipboard"

--------------------------------------------------------------------------------

test_glfwVulkanSupported :: IO ()
test_glfwVulkanSupported =
    -- Just test that it doesn't error. If it does, then we have a problem, but
    -- some platforms (like OS X) don't actually support vulkan.
    c'glfwVulkanSupported >> return ()

test_glfwGetRequiredInstanceExtensions :: IO ()
test_glfwGetRequiredInstanceExtensions = do
    support <- c'glfwVulkanSupported
    when (support == c'GLFW_TRUE) $
        alloca $ \p'count -> do
            p'exts <- c'glfwGetRequiredInstanceExtensions p'count
            when (p'exts /= nullPtr) $ do
                count <- peek p'count
                assertBool "Got at least some extensions" $ count > 0

test_glfwGetInstanceProcAddress :: IO ()
test_glfwGetInstanceProcAddress = do
    support <- c'glfwVulkanSupported
    when (support == c'GLFW_TRUE) $ do
        shouldBeNull <- withCString "notafunction" $
                        \s -> c'glfwGetInstanceProcAddress nullPtr s
        shouldBeNull @?= nullFunPtr
        assertBool "Function pointer is defined!" $
          p'glfwGetInstanceProcAddress /= nullFunPtr

test_glfwGetPhysicalDevicePresentationSupport :: IO ()
test_glfwGetPhysicalDevicePresentationSupport = do
    -- We don't really have the proper types to test this function
    support <- c'glfwVulkanSupported
    when (support == c'GLFW_TRUE) $ do
        shouldBeFalse <-
            c'glfwGetPhysicalDevicePresentationSupport nullPtr nullPtr 0
        shouldBeFalse @?= c'GLFW_FALSE

        -- If we pass a nullptr for the instance here then we better get a
        -- GLFW_API_UNAVAILABLE error since we didn't create the instance with
        -- the proper extensions...
        alloca $ \p'errMsg ->
            c'glfwGetError p'errMsg >>=
                assertEqual "Got proper vulkan error" c'GLFW_API_UNAVAILABLE

        assertBool "Function pointer is defined!" $
          p'glfwGetPhysicalDevicePresentationSupport /= nullFunPtr

test_glfwCreateWindowSurface :: Ptr C'GLFWwindow -> IO ()
test_glfwCreateWindowSurface p'win = do
    -- We don't really have the proper types to test this function
    support <- c'glfwVulkanSupported
    when (support == c'GLFW_TRUE) $ do
        alloca $ \p'surface -> do
          let resPtr = p'surface :: Ptr ()
          shouldNotBeSuccessful <-
            c'glfwCreateWindowSurface nullPtr p'win nullPtr resPtr
          assertBool "c'glfwCreateSurface was successful??" $
            shouldNotBeSuccessful /= 0

        -- The window that we pass here was not created with GLFW_NO_API, so
        -- the proper error received here seems to be GLFW_INVALID_VALUE
        alloca $ \p'errMsg ->
            c'glfwGetError p'errMsg >>=
                assertEqual "Got proper vulkan error" c'GLFW_INVALID_VALUE

        assertBool "Function pointer is defined!" $
          p'glfwCreateWindowSurface /= nullFunPtr

{-# ANN module "HLint: ignore Use camelCase" #-}
