-- | This module is needed because the ids in WXCore.WxcDefs weren,t matching
-- | those on wxMac-2.8.10
module HPage.GUI.IDs where

wxId_OK, wxId_CANCEL :: Int
wxId_OK         = 5100
wxId_CANCEL     = 5101

wxId_BACKWARD, wxId_CLOSE, wxId_COPY, wxId_CUT, wxId_FIND, wxId_FORWARD,
        wxId_HELP, wxId_NEW, wxId_OPEN, wxId_PASTE, wxId_SAVE,
        wxId_SAVEAS :: Int
wxId_OPEN       = 5000
wxId_CLOSE      = 5001
wxId_NEW        = 5002
wxId_SAVE       = 5003
wxId_SAVEAS     = 5004
wxId_HELP       = 5009
wxId_CUT        = 5031
wxId_COPY       = 5032
wxId_PASTE      = 5033
wxId_FIND       = 5035
wxId_FORWARD    = 5106
wxId_BACKWARD   = 5107

wxId_REDO, wxId_UNDO :: Int
wxId_UNDO       = 5207
wxId_REDO       = 5208

wxId_REPLACE, wxId_REPLACE_ALL, wxId_PREFERENCES,
    wxId_CLOSE_ALL :: Int
wxId_REPLACE        = 5038
wxId_REPLACE_ALL    = 5039
wxId_PREFERENCES    = 5022
wxId_CLOSE_ALL      = 5018

wxId_HASK_LOAD, wxId_HASK_LOADNAME, wxId_HASK_LOAD_FAST, wxId_HASK_RELOAD,
    wxId_HASK_INTERPRET, wxId_HASK_NAVIGATE,
    wxId_HASK_ADD, wxId_HASK_LOAD_PKG, wxId_HASK_MENUELEM, wxId_HASK_BROWSE,
    wxId_HASK_COPY, wxId_HASK_COPY_TYPE, wxId_HASK_EXPLAIN :: Int
wxId_HASK_LOAD      = 5300
wxId_HASK_LOADNAME  = 5301
wxId_HASK_RELOAD    = 5302
wxId_HASK_INTERPRET = 5303
wxId_HASK_ADD       = 5304
wxId_HASK_LOAD_PKG  = 5305
wxId_HASK_LOAD_FAST = 5306
wxId_HASK_BROWSE    = 5307
wxId_HASK_COPY      = 5308
wxId_HASK_COPY_TYPE = 5309
wxId_HASK_EXPLAIN   = 5310
wxId_HASK_NAVIGATE  = 5311
wxId_HASK_MENUELEM  = 5320
