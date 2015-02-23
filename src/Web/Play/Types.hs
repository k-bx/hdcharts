{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.Play.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson hiding ((.=))
import           Data.Aeson.ByteString ()
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Data
import           Data.Default
import           GHC.Generics
import           Lucid
import           Lucid.Css as Css
import           Lucid.Js as Js
import           MVC
import           Web.Page
import           Web.Socket

{- type representing the state of controllers on a page -}
data PlayState =
    PlayState
    { _pPlaying    :: Bool        -- whether to compute and stream the next frame
    , _pSleep      :: Double      -- sleep time between checking pGoing state
    , _pSpeed      :: Double      -- add delay effect to next frame
    , _pFrame      :: Int         -- current frame position
    , _pTargetFrame :: Maybe Int  -- move to new frame
    , _pTotalFrames :: Maybe Int -- may not be known
    , _pDropOk      :: Bool      -- can the stream be not streamed on rewind and fast forward etc
    , _pRedraw      :: Bool      -- instruction to View as to whether to redraw effect (False can be used to avoid overloading the view etc).
    , _pStep        :: Maybe Int  -- counter to an automatic stop (Nothing means no counter, Just 0 will turn _pPlaying from Go to Stop
    , _pFast        :: Bool      -- ignore Speed
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

makeLenses ''PlayState

instance FromJSON PlayState
instance ToJSON PlayState

instance Default PlayState where
    def = defaultPlayState

defaultPlayState :: PlayState
defaultPlayState =
    PlayState False 0.2 1.0 0 Nothing Nothing True True Nothing False

data PlayCommand
    = Go   -- compute (unpausing)
    | Stop -- pause computing
    | Quit -- stop computing and close down computation stream
    | First -- go to first frame
    | Last -- go (skip and/or fast) to last frame
    | Speed Double -- change speed
    | Step Int -- Go for x frames (skip if possible)
    deriving (Show, Read, Eq, Typeable, Data, Generic)

instance FromJSON PlayCommand
instance ToJSON PlayCommand

data SocketMessage = ServerReady deriving (Show, Read, Eq, Typeable, Data, Generic)

instance FromJSON SocketMessage
instance ToJSON SocketMessage

cssPlay :: Css
cssPlay =
    ".play" ? do
        Css.fontSize (Css.px 10)
        Css.fontFamily ["Arial", "Helvetica"] [Css.sansSerif]
        Css.marginLeft (Css.px 30)
        Css.marginTop (Css.px 12)
        Css.marginBottom (Css.px 12)
        "#spinner" ?
            Css.marginLeft (Css.px 6)
        ".btn" ?
            Css.marginLeft (Css.px 6)
        "#buttons" ? do
            Css.marginTop (Css.px 2)
            Css.marginBottom (Css.px 6)
        "#btnGo.on" ?
            Css.color Css.green
        "#btnStop.on" ?
            Css.color Css.red
        ".slider" ? do
            Css.float Css.floatLeft
            Css.width (Css.px 100)
        ".box" ? do
            Css.width (Css.px 40)
            Css.textAlign Css.end
        ".label" ? do
            Css.position Css.relative
            Css.float Css.floatLeft
            Css.marginRight (Css.px 20)
            Css.width (Css.px 100)
        ".ctrl" ? do
            Css.marginTop (Css.px 8)
            Css.marginBottom (Css.px 8)
        "#paramSpeed" ? do
            Css.width (Css.px 60)
            Css.height (Css.px 6)
        "#textSpeed" ?
            Css.width (Css.px 30)
        "#textFrame" ? do
            Css.width (Css.px 30)
            Css.marginLeft (Css.px 6)
            Css.marginRight (Css.px 6)
        "#textTotalFrame" ? do
            Css.width (Css.px 30)
            Css.marginLeft (Css.px 6)
        "input" ? do
            Css.backgroundColor Css.transparent
            Css.border Css.solid (Css.px 0) Css.wheat
            -- Css.width (100::Css.Size Css.Rel)
        "#sliders, #framecount" ?
            Css.float Css.floatLeft

-- | dom element construction
data CtrButton =
    CtrButton
    { _cbId :: String
    , _cbValue :: Html ()
    , _cbDomName :: String
    , _cbOnClick :: JStat }
makeLenses ''CtrButton

data CtrText =
    CtrText
    { _ctId :: String
    , _ctLabel :: Html ()
    , _ctValue :: Int
    , _ctDomName :: String
    , _ctOnChange :: JExpr }
makeLenses ''CtrText

data CtrSlider =
    CtrSlider
    { _csId :: String
    , _csMin :: Double
    , _csMax :: Double
    , _csStep :: Double
    , _csInitValue :: Double
    , _csLabel :: Html ()
    , _csDomName :: String
    , _csOnChange :: JExpr } 
makeLenses ''CtrSlider

jsButtons :: [CtrButton] -> JStat
jsButtons buttons = mconcat (Prelude.map mkJsButton buttons)

jsTexts :: [CtrText] -> JStat
jsTexts texts = mconcat (Prelude.map mkJsText texts)

jsSliders :: [CtrSlider] -> JStat
jsSliders sliders = mconcat (Prelude.map mkJsSlider sliders)

mkJsSlider :: CtrSlider -> JStat
mkJsSlider cs =
    let hashParamName = lit ("#param" ++ cs ^. csDomName)
        hashTextName = lit ("#text" ++ cs ^. csDomName)
    in
    [jmacro|
     $(`(hashParamName)`).change(function () {
           var !avalue = $(`(hashParamName)`).val();
           $(`(hashTextName)`).value=avalue;
           `(ApplStat (cs ^. csOnChange) [r "avalue"])`;
     });
     $(`(hashTextName)`).change(function () { 
           var !avalue = $(`(hashTextName)`).val();
           $(`(hashParamName)`).value=avalue;
           `(ApplStat (cs ^. csOnChange) [r "avalue"])`;
     });
    |]

mkJsSlider' :: CtrSlider -> JStat
mkJsSlider' cs =
    (jq paramId $. "change") $$$ fun0
    (Js.var "avalue"
     <> ("avalue" =: jq paramId $. "val")
     <> ((jq textId $. "value") =: r "avalue")
     <> (onChange $$$ r "avalue"))
  where
    paramId = "#param" ++ cs ^. csDomName
    textId = "#text" ++ cs ^. csDomName
    onChange = cs^.csOnChange

mkJsButton :: CtrButton -> JStat
mkJsButton cb =
    let domName = cb ^. cbDomName
        onClick = cb ^. cbOnClick in
    [jmacro|
     $(`(domName)`).click( function() {
           `(onClick)`;
     });
 |]

mkJsButton' :: CtrButton -> JStat
mkJsButton' cb =
    jq buttonId $. "click" $$$ fun0 onClick
  where
    buttonId = cb ^. cbDomName
    onClick = cb ^. cbOnClick

mkJsText :: CtrText -> JStat
mkJsText cs =
    let hashTextName = "#text" ++ (cs^.ctDomName)
    in
    [jmacro|
     $(`(hashTextName)`).change(function () { 
           var !avalue = $(`(hashTextName)`).val();
           `(ApplStat (cs ^. ctOnChange) [r "avalue"])`;
     });
    |]

mkJsText' :: CtrText -> JStat
mkJsText' cs =
    jq textId $. "change" $$$ fun0
    ( Js.var "avalue"
      <> ("avalue" =: jq textId $. "val")
      <> onChange $$$ r "avalue")
  where
    textId = "#text" ++ (cs^.ctDomName)
    onChange = cs^.ctOnChange

jsSocket' :: JStat
jsSocket' =
    "play.makeSocket" =: fun
    [ "wshost"
    , "wsport"
    , "update"
    , "onOpen"
    , "onClose"
    ]
    ( Js.var "uri"
      <> ("uri" =: r "'ws://' + ':' + wsport + '/'")
      <> Js.var "ws"
      <> ("ws.onopen" =: fun ["event"]
          (  (r "console.log" $$$ lit "open signal received from server")
          <> (r "onOpen" $$$ r "event")))
      <> ("ws.onclose" =: fun ["event"]
          (  (r "console.log" $$$ lit "close signal received from server")
          <> (r "onClose" $$$ r "event")))
      <> ("ws.onmessage" =: fun ["event"]
          (r "update" $$$ r "event.data")))

jsSocket :: JStat
jsSocket = [jmacro|

     play.makeSocket = function(wshost,wsport,update,onOpen,onClose) {
        var uri = 'ws://' + wshost + ':' + wsport + '/';
        var ws = new WebSocket(uri);
        ws.onopen = function(event) {
            console.log('open signal received from server');
            onOpen(event);
        };
        ws.onclose = function(event) {
            console.log('close signal received from server');
            onClose(event)
        };
        ws.onmessage = function(event) {
            update(event.data);
        };
        return ws;
     };
|]

{-
jsApplyPlay' :: PlayState -> JStat
jsApplyPlay' p0 =
    ("play.state" =: (r . C.unpack . encode) p0)
    <> ("play.update" =: fun ["e"]
        ((r "d" =: (r "JSON.parse" $$ r "e"))
         <> switch (r "d.tag")
            [ (lit "PlayStateOut",
               ("play.state" =: r "d.contents")
              <> (r "play.setPlayEffects" $$$ (mempty::JExpr)))]
         (r "console.log" $$$ r "d.contents"))
       )

-}

{-    [jmacro|
     play.state = `((r . C.unpack . encode) p0)`;
     play.update = function(e) {
         d = JSON.parse(e);
         switch (d.tag) {
            case "PlayStateOut":
                play.state = d.contents;
                play.setPlayEffects();
                break;
            case "StreamOut":
                play.handleStream(d.contents);
                break;
            case "LogOut":
                console.log(d.contents);
                break;
            case "SocketOut":
                play.ws.close();
                break;
            default:
                console.log(d.contents);
         };
     };
     play.setPlayEffects = function() {
         if (play.state._pPlaying) {
             $('#btnGo').addClass("on");
             $('#btnStop').removeClass("on");
         } else {
             $('#btnGo').removeClass("on");
             $('#btnStop').addClass("on");
         };
         $("#paramSpeed").val(play.state._pSpeed);
         $("#textSpeed").val(play.state._pSpeed);
         $("#textFrame").val(play.state._pFrame);
         if (play.state._pTotalFrames == null) {
             $("#textTotalFrame").val("?");
         } else {
             $("#textTotalFrame").val(play.state._pTotalFrames);
         };
     };
     play.onOpen = function (e) {
         $('#spinner').addClass("icon-spin");
     };
     play.onClose = function(e) {
         $('#spinner').removeClass("icon-spin");
     };
|]
-}

jsApplyPlay :: PlayState -> JStat
jsApplyPlay p0 =
    [jmacro|
     play.state = `((r . C.unpack . encode) p0)`;
     play.update = function(e) {
         var d = JSON.parse(e);
         switch (d.tag) {
            case "PlayStateOut":
                play.state = d.contents;
                play.setPlayEffects();
                break;
            case "StreamOut":
                play.handleStream(d.contents);
                break;
            case "LogOut":
                console.log(d.contents);
                break;
            case "SocketOut":
                play.ws.close();
                break;
            default:
                console.log(d.contents);
         };
     };
     play.setPlayEffects = function() {
         if (play.state._pPlaying) {
             $('#btnGo').addClass("on");
             $('#btnStop').removeClass("on");
         } else {
             $('#btnGo').removeClass("on");
             $('#btnStop').addClass("on");
         };
         $("#paramSpeed").val(play.state._pSpeed);
         $("#textSpeed").val(play.state._pSpeed);
         $("#textFrame").val(play.state._pFrame);
         if (play.state._pTotalFrames == null) {
             $("#textTotalFrame").val("?");
         } else {
             $("#textTotalFrame").val(play.state._pTotalFrames);
         };
     };
     play.onOpen = function (e) {
         $('#spinner').addClass("icon-spin");
     };
     play.onClose = function(e) {
         $('#spinner').removeClass("icon-spin");
     };
|]

jsPlaySetup :: String -> Int -> JStat
jsPlaySetup host' port' =
    [jmacro|
     play.setup = function() {
         play.ws = play.makeSocket(`(host')`,`(port')`,play.update, play.onOpen, play.onClose);
         play.setPlayEffects();
     };
    |]

jsPlayOnLoad :: JStat
jsPlayOnLoad =
    [jmacro|
     window.onload = play.setup();
    |]


buttonsPlay :: [CtrButton]
buttonsPlay =
    [ CtrButton "btnQuit" (with i_ [class_ "icon-eject"] mempty) "#btnQuit"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Quit",contents:[]}}));
      |]
    , CtrButton "btnReset" (with i_ [class_ "icon-fast-backward"] mempty) "#btnReset"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"First",contents:[]}}));
      |]
    , CtrButton "btnStop" (with i_ [class_ "icon-stop"] mempty) "#btnStop"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Stop",contents:[]}}));
      |]
    , CtrButton "btnGo" (with i_ [class_ "icon-play"] mempty) "#btnGo"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Go",contents:[]}}));
      |]
    , CtrButton "btnStepForward" (with i_ [class_ "icon-step-forward"] mempty) "#btnStepForward"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Step",contents:1}}));
      |]
    , CtrButton "btnForward" (with i_ [class_ "icon-forward"] mempty) "#btnForward"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Step",contents:10}}));
      |]
    , CtrButton "btnFForward" (with i_ [class_ "icon-fast-forward"] mempty) "#btnFForward"
      [jmacro| 
       play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Last",contents:[]}}));
      |]
    ]

slidersPlay :: Double -> [CtrSlider]
slidersPlay s =
    [ CtrSlider "Speed" 0 5 0.1 s (with i_ [class_ "icon-fighter-jet icon-large"] mempty) "Speed"
      [jmacroE|
       function(d) {
           v = parseFloat(d);
           play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Speed",contents:v}}));
       }
      |]
    ]

textsPlay :: Int -> [CtrText]
textsPlay frame =
    [ CtrText "Frame" (with i_ [id_ "spinner", class_ "icon-spinner icon-large"] mempty) frame "Frame"
      [jmacroE|
       function(d) {
           var v = parseInt(d);
           play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"GoTo",contents:v}}));
       }
      |]
    , CtrText "TotalFrame" "of" frame "TotalFrame"
      [jmacroE|
       function(d) {
           return;
       }
      |]
    ]

sectionPlay :: [CtrButton] -> [CtrSlider] -> [CtrText] -> Html ()
sectionPlay buttons sliders texts =
    with div_ [class_ "play"] (mconcat 
        [ with div_ [id_ "buttons"] (mconcat ((\x -> with button_
                       [ id_ (pack $ x ^. cbId)
                       , Lucid.type_ "button"
                       ] (x^.cbValue)) <$> buttons))
        , with div_ [id_ "sliders"]
                       (mconcat ((\x ->
                        (x ^. csLabel) <>
                        with (input_ mempty)
                        [ id_ ("param" <> pack (x ^. csId))
                        , Lucid.type_ "range"
                        , name_ (pack $ x ^. csId)
                        , min_ (pack $ show (x ^. csMin))
                        , max_ (pack $ show (x ^. csMax))
                        , step_ (pack $ show (x ^. csStep))
                        , value_ (pack $ show (x ^. csInitValue))
                        ] <>
                        with (input_ mempty)
                        [ id_ ("text" <> pack (x ^. csId))
                        , Lucid.type_ "text"
                        , value_ (pack $ show $ x^.csInitValue)
                        ]) <$> sliders))
        , with div_ [id_ "framecount"] (mconcat ((\x ->
                 (x ^. ctLabel) <>
                  with (input_ mempty)
                  [ id_ ("text" <> pack (x ^. ctId))
                  , Lucid.type_ "text"
                  , value_ (pack $ show (x ^. ctValue))
                  ]) <$> texts))
        ])

jsStatementsPlay :: PlayState -> String -> Int -> JStat
jsStatementsPlay p port host =
    jsSocket <>
    jsApplyPlay p <>
    jsPlaySetup port host <>
    jsButtons buttonsPlay <>
    jsSliders (slidersPlay (p ^. pSpeed)) <>
    jsTexts (textsPlay 0)

jsGlobalPlay :: JStat
jsGlobalPlay = declObj "play"

jsOnloadPlay :: JStat -> PlayState -> SocketConfig -> JStat
jsOnloadPlay handleStream p sc =
    [jmacro|
         `(jsStatementsPlay p (sc^.cHost) (sc^.cPort))`;
         play.setup();
         `(handleStream)`;
    |]

play :: JStat -> PlayState -> SocketConfig -> Page
play handleStream initialPlay sc =
    chartLibs <> Page [] [] cssPlay
    jsGlobalPlay
    (jsOnloadPlay handleStream initialPlay sc)
    mempty
    (sectionPlay buttonsPlay (slidersPlay (initialPlay ^. pSpeed)) (textsPlay 0))

jsEcho :: JStat
jsEcho =
    [jmacro|
     play.handleStream = function(e) {
         console.log(e);
         play.ws.send(JSON.stringify({tag:"Echo", contents: e}));
     };
|]

testPage :: Html ()
testPage = renderPage (play jsEcho defaultPlayState defaultSocketConfig)

-- renderToFile "test.html" testPage

testJs :: IO ()
testJs = pageJsToFile "play.js" (play jsEcho defaultPlayState defaultSocketConfig)

