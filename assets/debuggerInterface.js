Elm.Native.Slider = {};
Elm.Native.Slider.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Slider = elm.Native.Slider || {};
    if (elm.Native.Slider.values) return elm.Native.Slider.values;

    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
    var newElement = Elm.Graphics.Element.make(elm).newElement;

    function renderSlider(model) {
        var node = newNode('input');
        node.type = 'range';

        node.min = model.styling.min;
        node.max = model.styling.max;
        node.step = model.styling.step;
        node.value = model.styling.value;

        if (!model.styling.horizontal) {
            node.orient = "vertical"; // FF
            node.style.webkitAppearance = "slider-vertical"; // webkit
            node.style.writingMode = "bt-lr"; // ie
        }

        if (model.styling.disabled) {
            node.disabled = true;
        }

        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        node.addEventListener('input', function() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.value));
        });
        return node;
    }

    function updateSlider(node, oldModel, newModel) {
        if (newModel.styling.disabled) {
            node.disabled = true;
        } else {
            node.disabled = false;
        }
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.min = newModel.styling.min;
        node.max = newModel.styling.max;
        node.step = newModel.styling.step;
        node.value = newModel.styling.value;
    }

    function slider(signal, handler, styling) {
        var width = styling.length;
        var height = 24;
        if (!styling.horizontal) {
            var temp = width;
            width = height;
            height = temp;
        }
        return A3(newElement, width, height, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider,
            update: updateSlider,
            model: { signal:signal, handler:handler, styling:styling }
        });
    }

    return elm.Native.Slider.values =
        { slider:F3(slider)
        }
}
Elm.DebuggerInterface = Elm.DebuggerInterface || {};
Elm.DebuggerInterface.make = function (_elm) {
   "use strict";
   _elm.DebuggerInterface = _elm.DebuggerInterface || {};
   if (_elm.DebuggerInterface.values)
   return _elm.DebuggerInterface.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "DebuggerInterface",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Native$Ports = Elm.Native.Ports.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Slider = Elm.Slider.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Window = Elm.Window.make(_elm);
   var noWatches = $Text.markdown("<div style=\"height:0;width:0;\">&nbsp;</div><h3><span style=\"font-family: Gotham; font-size: 12pt; color: rgb(170,170,170)\"> You don&#39;t have any watches! </span></h3>\n<p><span style=\"color: rgb(170,170,170)\">\n<span style=\"font-family: Gotham; font-size: 10pt; color: rgb(170,170,170)\">\nUse <a href=\"http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Debug#watch\"><span style=\"text-decoration:underline; color: rgb(170,170,170)\">Debug.watch</span></a>\nto show any value. <br>\n<code>watch : String -&gt; a -&gt; a</code></span></p>\n<p><span style=\"font-family: Gotham; font-size: 10pt; color: rgb(170,170,170)\">\nUse <a href=\"http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Debug#watchSummary\"><span style=\"text-decoration:underline; color: rgb(170,170,170)\">Debug.watchSummary</span></a> to show a <br>\nsummary or subvalue of any value. </span><br></p>\n<div style=\"height:0;width:0;\">&nbsp;</div>",
   "300:13");
   var roundedSquare = F3(function (side,
   radius,
   toForm) {
      return function () {
         var formedCircle = toForm($Graphics$Collage.circle(radius));
         var shortSide = side - 2 * radius;
         var xRect = toForm(A2($Graphics$Collage.rect,
         side,
         shortSide));
         var yRect = toForm(A2($Graphics$Collage.rect,
         shortSide,
         side));
         var circleOffset = shortSide / 2;
         var tl = $Graphics$Collage.move({ctor: "_Tuple2"
                                         ,_0: 0 - circleOffset
                                         ,_1: circleOffset})(formedCircle);
         var tr = $Graphics$Collage.move({ctor: "_Tuple2"
                                         ,_0: circleOffset
                                         ,_1: circleOffset})(formedCircle);
         var bl = $Graphics$Collage.move({ctor: "_Tuple2"
                                         ,_0: 0 - circleOffset
                                         ,_1: 0 - circleOffset})(formedCircle);
         var br = $Graphics$Collage.move({ctor: "_Tuple2"
                                         ,_0: circleOffset
                                         ,_1: 0 - circleOffset})(formedCircle);
         return $Graphics$Collage.group(_L.fromArray([xRect
                                                     ,yRect
                                                     ,tl
                                                     ,tr
                                                     ,bl
                                                     ,br]));
      }();
   });
   var startState = {_: {}
                    ,paused: false
                    ,scrubPosition: 0
                    ,totalEvents: 0};
   var step = F2(function (update,
   state) {
      return function () {
         switch (update.ctor)
         {case "Pause":
            return _U.replace([["paused"
                               ,update._0]],
              state);
            case "Restart":
            return startState;
            case "ScrubPosition":
            return _U.replace([["scrubPosition"
                               ,update._0]
                              ,["paused",true]],
              state);
            case "TotalEvents":
            return _U.replace([["totalEvents"
                               ,update._0]
                              ,["scrubPosition",update._0]],
              state);}
         _E.Case($moduleName,
         "between lines 257 and 267");
      }();
   });
   var watches = $Native$Ports.portIn("watches",
   $Native$Ports.incomingSignal(function (v) {
      return _U.isJSArray(v) ? _L.fromArray(v.map(function (v) {
         return _U.isJSArray(v) ? {ctor: "_Tuple2"
                                  ,_0: typeof v[0] === "string" || typeof v[0] === "object" && v[0] instanceof String ? v[0] : _E.raise("invalid input, expecting JSString but got " + v[0])
                                  ,_1: typeof v[1] === "string" || typeof v[1] === "object" && v[1] instanceof String ? v[1] : _E.raise("invalid input, expecting JSString but got " + v[1])} : _E.raise("invalid input, expecting JSArray but got " + v);
      })) : _E.raise("invalid input, expecting JSArray but got " + v);
   }));
   var eventCounter = $Native$Ports.portIn("eventCounter",
   $Native$Ports.incomingSignal(function (v) {
      return typeof v === "number" ? v : _E.raise("invalid input, expecting JSNumber but got " + v);
   }));
   var scrubInput = $Graphics$Input.input(0);
   var restartInput = $Graphics$Input.input({ctor: "_Tuple0"});
   var permitHotswapInput = $Graphics$Input.input(true);
   var pausedInput = $Graphics$Input.input(false);
   var permitHotswap = $Native$Ports.portOut("permitHotswap",
   $Native$Ports.outgoingSignal(function (v) {
      return v;
   }),
   permitHotswapInput.signal);
   var restart = $Native$Ports.portOut("restart",
   $Native$Ports.outgoingSignal(function (v) {
      return v;
   }),
   A2($Signal.lift,
   function (x) {
      return 0;
   },
   restartInput.signal));
   var scrubSlider = F2(function (_v4,
   state) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2":
            return function () {
                 var sliderLength = _v4._0;
                 var sliderStyle = _U.replace([["length"
                                               ,sliderLength]
                                              ,["max"
                                               ,$Basics.toFloat(state.totalEvents)]
                                              ,["value"
                                               ,$Basics.toFloat(state.scrubPosition)]],
                 $Slider.defaultSlider);
                 return A3($Graphics$Element.container,
                 sliderLength,
                 20,
                 $Graphics$Element.middle)(A3($Slider.slider,
                 scrubInput.handle,
                 $Basics.round,
                 sliderStyle));
              }();}
         _E.Case($moduleName,
         "between lines 118 and 126");
      }();
   });
   var restartButton = A5($Graphics$Input.customButton,
   restartInput.handle,
   {ctor: "_Tuple0"},
   A3($Graphics$Element.image,
   40,
   40,
   "/restart-button.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/restart-button-hover.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/restart-button-click.png"));
   var pauseButton = A5($Graphics$Input.customButton,
   pausedInput.handle,
   true,
   A3($Graphics$Element.image,
   40,
   40,
   "/pause-button.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/pause-button-hover.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/pause-button-click.png"));
   var playButton = A5($Graphics$Input.customButton,
   pausedInput.handle,
   false,
   A3($Graphics$Element.image,
   40,
   40,
   "/play-button.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/play-button-hover.png"),
   A3($Graphics$Element.image,
   40,
   40,
   "/play-button-click.png"));
   var darkGrey = A3($Color.rgb,
   74,
   74,
   74);
   var lightGrey = A3($Color.rgb,
   228,
   228,
   228);
   var dataStyle = F2(function (typefaces,
   height) {
      return function () {
         var myStyle = _U.replace([["typeface"
                                   ,typefaces]
                                  ,["color",lightGrey]
                                  ,["height"
                                   ,$Maybe.Just(height)]],
         $Text.defaultStyle);
         return function ($) {
            return $Text.style(myStyle)($Text.toText($));
         };
      }();
   });
   var textStyle = A2(dataStyle,
   _L.fromArray(["Gotham"
                ,"Futura"
                ,"Lucida Grande"
                ,"sans-serif"]),
   12);
   var watchStyle = A2(dataStyle,
   _L.fromArray(["Gotham"
                ,"Futura"
                ,"Lucida Grande"
                ,"sans-serif"]),
   14);
   var codeStyle = A2(dataStyle,
   _L.fromArray(["Menlo for Powerline"
                ,"monospace"]),
   12);
   var blue = A3($Color.rgb,
   28,
   129,
   218);
   var hotswapButton = function (permitHotswap) {
      return function () {
         var info = $Text.leftAligned(textStyle("hotswap"));
         var radius = 4;
         var hsWidth = 25;
         var bgButton = A3(roundedSquare,
         hsWidth,
         radius,
         $Graphics$Collage.filled(lightGrey));
         var trueButton = _L.fromArray([bgButton
                                       ,A3(roundedSquare,
                                       22,
                                       radius,
                                       $Graphics$Collage.filled(blue))]);
         var falseButtonClick = trueButton;
         var trueButtonHover = _L.fromArray([bgButton
                                            ,A3(roundedSquare,
                                            22,
                                            radius,
                                            $Graphics$Collage.filled(blue))
                                            ,$Graphics$Collage.alpha(0.1)(A3(roundedSquare,
                                            22,
                                            radius,
                                            $Graphics$Collage.filled(darkGrey)))]);
         var falseButton = _L.fromArray([bgButton
                                        ,A3(roundedSquare,
                                        22,
                                        radius,
                                        $Graphics$Collage.filled(darkGrey))]);
         var trueButtonClick = falseButton;
         var falseButtonHover = _L.fromArray([bgButton
                                             ,A3(roundedSquare,
                                             22,
                                             radius,
                                             $Graphics$Collage.filled(darkGrey))
                                             ,$Graphics$Collage.alpha(0.1)(A3(roundedSquare,
                                             22,
                                             radius,
                                             $Graphics$Collage.filled(blue)))]);
         var button = permitHotswap ? A5($Graphics$Input.customButton,
         permitHotswapInput.handle,
         false,
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         trueButton),
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         trueButtonHover),
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         trueButtonClick)) : A5($Graphics$Input.customButton,
         permitHotswapInput.handle,
         true,
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         falseButton),
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         falseButtonHover),
         A3($Graphics$Collage.collage,
         hsWidth,
         hsWidth,
         falseButtonClick));
         return A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([info
                      ,A2($Graphics$Element.spacer,
                      10,
                      1)
                      ,button]));
      }();
   };
   var panelWidth = 275;
   var textHeight = 20;
   var sliderMinMaxText = F2(function (w,
   state) {
      return function () {
         var sliderTotalEvents = A4($Graphics$Element.container,
         w,
         textHeight,
         $Graphics$Element.topRight,
         $Text.rightAligned(textStyle($String.show(state.totalEvents))));
         var sliderStartText = A4($Graphics$Element.container,
         w,
         textHeight,
         $Graphics$Element.topLeft,
         $Text.leftAligned(textStyle("0")));
         return A2($Graphics$Element.flow,
         $Graphics$Element.outward,
         _L.fromArray([sliderStartText
                      ,sliderTotalEvents]));
      }();
   });
   var sideMargin = 2 * 20;
   var sliderEventText = F2(function (w,
   state) {
      return function () {
         var text$ = $Text.centered(textStyle($String.show(state.scrubPosition)));
         var yPos = $Graphics$Element.absolute($Basics.round(textHeight / 2));
         var totalEvents = $Basics.toFloat(state.totalEvents);
         var scrubPosition = $Basics.toFloat(state.scrubPosition);
         var textWidthOffset = 14;
         var midWidth = $Basics.toFloat(w) - sideMargin - textWidthOffset;
         var leftDistance = _U.eq(totalEvents,
         0) ? sideMargin / 2 + textWidthOffset / 2 : scrubPosition / totalEvents * midWidth + sideMargin / 2 + textWidthOffset / 2;
         var xPos = $Graphics$Element.absolute($Basics.round(leftDistance));
         var textPosition = A2($Graphics$Element.middleAt,
         xPos,
         yPos);
         return A4($Graphics$Element.container,
         w,
         textHeight,
         textPosition,
         text$);
      }();
   });
   var buttonWidth = 40;
   var buttonHeight = 40;
   var view = F4(function (_v8,
   watches,
   permitHotswap,
   state) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return function () {
                 var showWatch = function (_v12) {
                    return function () {
                       switch (_v12.ctor)
                       {case "_Tuple2":
                          return A2($Graphics$Element.flow,
                            $Graphics$Element.down,
                            _L.fromArray([$Graphics$Element.width(_v8._0)($Text.leftAligned($Text.bold(watchStyle(_v12._0))))
                                         ,$Graphics$Element.width(_v8._0)($Text.leftAligned(codeStyle(_v12._1)))
                                         ,A2($Graphics$Element.spacer,
                                         1,
                                         12)]));}
                       _E.Case($moduleName,
                       "between lines 190 and 194");
                    }();
                 };
                 var watchView = A2($Graphics$Element.flow,
                 $Graphics$Element.right,
                 _L.fromArray([A2($Graphics$Element.spacer,
                              20,
                              1)
                              ,function () {
                                 switch (watches.ctor)
                                 {case "[]": return noWatches;}
                                 return $Graphics$Element.flow($Graphics$Element.down)(A2($List.map,
                                 showWatch,
                                 watches));
                              }()]));
                 var bar = A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([$Graphics$Element.opacity(0.3)($Graphics$Element.color(lightGrey)(A2($Graphics$Element.spacer,
                              _v8._0,
                              1)))
                              ,A2($Graphics$Element.spacer,
                              _v8._0,
                              12)]));
                 var fittedHotSwapButton = A3($Graphics$Element.container,
                 _v8._0 - 2 * buttonWidth - sideMargin,
                 buttonHeight,
                 $Graphics$Element.middle)(hotswapButton(permitHotswap));
                 var buttons = A2($Graphics$Element.flow,
                 $Graphics$Element.right,
                 _L.fromArray([restartButton
                              ,fittedHotSwapButton
                              ,state.paused ? playButton : pauseButton]));
                 var buttonContainer = A4($Graphics$Element.container,
                 _v8._0,
                 buttonHeight,
                 $Graphics$Element.midTop,
                 buttons);
                 var buttonSliderSpaceHeight = 10;
                 var topSpacerHeight = 15;
                 var midWidth = _v8._0 - sideMargin;
                 var centeredSliderContainer = A3($Graphics$Element.container,
                 _v8._0,
                 24 + textHeight,
                 $Graphics$Element.midTop)(A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([A2(scrubSlider,
                              {ctor: "_Tuple2"
                              ,_0: midWidth
                              ,_1: _v8._1},
                              state)
                              ,A2(sliderMinMaxText,
                              midWidth,
                              state)])));
                 var slider = A3($Graphics$Element.container,
                 _v8._0,
                 24 + 2 * textHeight,
                 $Graphics$Element.midTop)(A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([A2(sliderEventText,
                              _v8._0,
                              state)
                              ,centeredSliderContainer])));
                 var controls = A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([A2($Graphics$Element.spacer,
                              midWidth,
                              topSpacerHeight)
                              ,buttonContainer
                              ,A2($Graphics$Element.spacer,
                              midWidth,
                              buttonSliderSpaceHeight)
                              ,slider
                              ,A2($Graphics$Element.spacer,
                              midWidth,
                              10)]));
                 return A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([controls
                              ,bar
                              ,watchView]));
              }();}
         _E.Case($moduleName,
         "between lines 157 and 205");
      }();
   });
   var State = F3(function (a,
   b,
   c) {
      return {_: {}
             ,paused: a
             ,scrubPosition: c
             ,totalEvents: b};
   });
   var ScrubPosition = function (a) {
      return {ctor: "ScrubPosition"
             ,_0: a};
   };
   var TotalEvents = function (a) {
      return {ctor: "TotalEvents"
             ,_0: a};
   };
   var Pause = function (a) {
      return {ctor: "Pause",_0: a};
   };
   var Restart = {ctor: "Restart"};
   var aggregateUpdates = $Signal.merges(_L.fromArray([A2($Signal._op["<~"],
                                                      $Basics.always(Restart),
                                                      restartInput.signal)
                                                      ,A2($Signal._op["<~"],
                                                      Pause,
                                                      pausedInput.signal)
                                                      ,A2($Signal._op["<~"],
                                                      TotalEvents,
                                                      eventCounter)
                                                      ,A2($Signal._op["<~"],
                                                      ScrubPosition,
                                                      scrubInput.signal)]));
   var scene = A3($Signal.foldp,
   step,
   startState,
   aggregateUpdates);
   var main = A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["<~"],
   view,
   A2($Signal._op["<~"],
   function (_v17) {
      return function () {
         switch (_v17.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: panelWidth
                   ,_1: _v17._1};}
         _E.Case($moduleName,
         "on line 213, column 30 to 43");
      }();
   },
   $Window.dimensions)),
   watches),
   permitHotswapInput.signal),
   scene);
   var scrubTo = $Native$Ports.portOut("scrubTo",
   $Native$Ports.outgoingSignal(function (v) {
      return v;
   }),
   A2($Signal._op["<~"],
   function (_) {
      return _.scrubPosition;
   },
   scene));
   var pause = $Native$Ports.portOut("pause",
   $Native$Ports.outgoingSignal(function (v) {
      return v;
   }),
   A2($Signal._op["<~"],
   function (_) {
      return _.paused;
   },
   scene));
   _elm.DebuggerInterface.values = {_op: _op
                                   ,Restart: Restart
                                   ,Pause: Pause
                                   ,TotalEvents: TotalEvents
                                   ,ScrubPosition: ScrubPosition
                                   ,buttonHeight: buttonHeight
                                   ,buttonWidth: buttonWidth
                                   ,sideMargin: sideMargin
                                   ,textHeight: textHeight
                                   ,panelWidth: panelWidth
                                   ,blue: blue
                                   ,lightGrey: lightGrey
                                   ,darkGrey: darkGrey
                                   ,dataStyle: dataStyle
                                   ,textStyle: textStyle
                                   ,watchStyle: watchStyle
                                   ,codeStyle: codeStyle
                                   ,playButton: playButton
                                   ,pauseButton: pauseButton
                                   ,restartButton: restartButton
                                   ,hotswapButton: hotswapButton
                                   ,scrubSlider: scrubSlider
                                   ,sliderEventText: sliderEventText
                                   ,sliderMinMaxText: sliderMinMaxText
                                   ,view: view
                                   ,main: main
                                   ,pausedInput: pausedInput
                                   ,permitHotswapInput: permitHotswapInput
                                   ,restartInput: restartInput
                                   ,scrubInput: scrubInput
                                   ,scene: scene
                                   ,startState: startState
                                   ,step: step
                                   ,aggregateUpdates: aggregateUpdates
                                   ,roundedSquare: roundedSquare
                                   ,noWatches: noWatches};
   return _elm.DebuggerInterface.values;
};Elm.Slider = Elm.Slider || {};
Elm.Slider.make = function (_elm) {
   "use strict";
   _elm.Slider = _elm.Slider || {};
   if (_elm.Slider.values)
   return _elm.Slider.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Slider",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Native$Slider = Elm.Native.Slider.make(_elm);
   var slider = $Native$Slider.slider;
   var defaultSlider = {_: {}
                       ,disabled: false
                       ,horizontal: true
                       ,length: 100
                       ,max: 100
                       ,min: 0
                       ,step: 1
                       ,value: 0};
   var SliderStyle = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,disabled: b
             ,horizontal: a
             ,length: c
             ,max: e
             ,min: d
             ,step: f
             ,value: g};
   });
   _elm.Slider.values = {_op: _op
                        ,defaultSlider: defaultSlider
                        ,slider: slider};
   return _elm.Slider.values;
};