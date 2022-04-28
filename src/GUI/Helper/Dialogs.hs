{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-| Pop-up dialogs to inform errors or confirm operations such as save or open a project -}
module GUI.Helper.Dialogs (
  showError,
  createSaveDialog,
  createLoadDialog,
  createConfirmDialog
) where

import qualified GI.Gtk as Gtk
import           Data.GI.Base
import qualified Data.Text as T

-- show a error dialog with a custom message
showError :: Gtk.Window -> T.Text -> IO ()
showError window msg = do
  msgDialog <- new Gtk.MessageDialog [ #text := msg
                                , #messageType := Gtk.MessageTypeError
                                , #buttons := Gtk.ButtonsTypeOk
                                , #transientFor := window
                                , #destroyWithParent := True
                                ]
  Gtk.widgetShowAll msgDialog
  Gtk.dialogRun msgDialog
  Gtk.widgetDestroy msgDialog
  return ()

-- show a dialog to choose the save location of a file
createSaveDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createSaveDialog window = do
  saveD <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionSave
                                     , #createFolders := True
                                     , #doOverwriteConfirmation := True
                                     , #transientFor := window
                                     , #destroyWithParent := True
                                     ]
  Gtk.dialogAddButton saveD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
  Gtk.dialogAddButton saveD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeReject)
  return saveD

-- show a dialog to choose a file to open
createLoadDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createLoadDialog window = do
  loadD <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionOpen
                                     , #createFolders := False
                                     , #doOverwriteConfirmation := False
                                     , #transientFor := window
                                     , #destroyWithParent := True
                                     ]
  Gtk.dialogAddButton loadD "Open" (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
  Gtk.dialogAddButton loadD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeReject)
  return loadD

-- show a dialog to confirm a operation in case the project was not saved
createConfirmDialog :: Gtk.Window -> T.Text -> IO Gtk.ResponseType
createConfirmDialog window msg = do
  closeD <- new Gtk.MessageDialog
            [ #text := msg
            , #messageType := Gtk.MessageTypeWarning
            , #buttons := Gtk.ButtonsTypeNone
            , #transientFor := window
            , #destroyWithParent := True
            ]
  Gtk.dialogAddButton closeD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeYes)
  Gtk.dialogAddButton closeD "Don't save" (fromIntegral . fromEnum $ Gtk.ResponseTypeNo)
  Gtk.dialogAddButton closeD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeCancel)
  response <- Gtk.dialogRun closeD
  Gtk.widgetDestroy closeD
  return $ toEnum . fromIntegral $ response
