{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Windows where

import GHC.IO.Handle

import System.Win32.Types
import System.Win32.File
import qualified System.Win32.Comm as Comm

import Foreign.Marshal.Alloc

import Control.Monad (unless)

import Data.Typeable
import Data.Bits
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Unsafe as BU

import System.Hardware.Serialport.Types


data SerialPort = SerialPort
  { handle       :: HANDLE
  , portSettings :: SerialPortSettings
  } deriving (Show, Typeable)

-- |Open and configure a serial port returning a standard Handle.
hOpenSerial :: String -> SerialPortSettings -> IO Handle
hOpenSerial dev settings = do
  h <- hANDLEToHandle . handle =<< openSerial dev settings
  hSetBuffering h NoBuffering
  return h

-- | Open and configure a serial port
openSerial :: String      -- ^ Serial port, such as @COM5@ or @CNCA0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  h <- createFile ("\\\\.\\" ++ dev) access_mode share_mode security_attr create_mode file_attr template_file
  let serial_port = SerialPort h defaultSerialSettings
  setSerialSettings serial_port settings
  where
    access_mode = gENERIC_READ .|. gENERIC_WRITE
    share_mode = fILE_SHARE_NONE
    security_attr = Nothing
    create_mode = oPEN_EXISTING
    file_attr = fILE_ATTRIBUTE_NORMAL -- .|. fILE_FLAG_OVERLAPPED
    template_file = Nothing


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv port n = allocaBytes n $ \p -> do
  recv_cnt <- win32_ReadFile (handle port) p count overlapped
  B.packCStringLen (p, fromIntegral recv_cnt)
  where
    count = fromIntegral n
    overlapped = Nothing


-- |Send bytes
send :: SerialPort
        -> B.ByteString
        -> IO Int          -- ^ Number of bytes actually sent
send port msg = BU.unsafeUseAsCString msg $ \p ->
  fromIntegral `fmap` win32_WriteFile (handle port) p count overlapped
  where
    count = fromIntegral $ B.length msg
    overlapped = Nothing


-- |Flush buffers
flush :: SerialPort -> IO ()
flush port = flushFileBuffers (handle port) >> consumeIncomingChars
  where
    consumeIncomingChars = do
      ch <- recv port 1
      unless (ch == B.empty) consumeIncomingChars


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeHandle . handle


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR port True  =  Comm.escapeCommFunction (handle port) Comm.setDTR
setDTR port False =  Comm.escapeCommFunction (handle port) Comm.clrDTR


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS port True  = Comm.escapeCommFunction (handle port) Comm.setRTS
setRTS port False = Comm.escapeCommFunction (handle port) Comm.clrRTS


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings port new_settings = do
  Comm.setCommTimeouts (handle port) commTimeouts
  Comm.setCommState (handle port) new_settings
  return $ SerialPort (handle port) new_settings
  where
    commTimeouts = Comm.COMMTIMEOUTS
      { Comm.readIntervalTimeout = maxBound :: DWORD
      , Comm.readTotalTimeoutMultiplier = maxBound :: DWORD
      , Comm.readTotalTimeoutConstant = fromIntegral (timeout new_settings) * 100
      , Comm.writeTotalTimeoutMultiplier = 0
      , Comm.writeTotalTimeoutConstant = 0 
      }

-- |Get configuration from serial port
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings
