hdcharts
===

Haskell-driven Charts

<html><head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"><meta charset="utf-8"><link href="http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css" rel="stylesheet"><script src="http://d3js.org/d3.v3.min.js"></script><script src="http://code.jquery.com/jquery-1.6.3.min.js"></script>
    <style>
    .play
    {
      font-size     : 10px;
      font-family   : "Arial","Helvetica", sans-serif;
      margin-left   : 30px;
      margin-top    : 12px;
      margin-bottom : 12px;
    }    
    .play #spinner
    {
      margin-left : 6px;
    }    
    .play .btn
    {
      margin-left : 6px;
    }
    .play #buttons
    {
      margin-top    : 2px;
      margin-bottom : 6px;
    }
    .play #btnGo.on
    {
      color : rgb(0,128,0);
    }
    .play #btnStop.on
    {
      color : rgb(255,0,0);
    }
    .play .slider
    {
      float : left;
      width : 100px;
    }
    .play .box
    {
      width      : 40px;
      text-align : end;
    }
    .play .label
    {
      position     : relative;
      float        : left;
      margin-right : 20px;
      width        : 100px;
    }
    .play .ctrl
    {
      margin-top    : 8px;
      margin-bottom : 8px;
    }
    .play #paramSpeed
    {
      width  : 60px;
      height : 6px;
    }
    .play #textSpeed
    {
      width : 30px;
    }
    .play #textFrame
    {
      width        : 30px;
      margin-left  : 6px;
      margin-right : 6px;
    }
    .play #textTotalFrame
    {
      width       : 30px;
      margin-left : 6px;
    }
    .play input
    {
      background-color : rgba(0,0,0,0.0000);
      border           : solid 0px rgb(245,222,179);
    }
    .play #sliders, #framecount
    {
      float : left;
    }
    #charttext
    {
      clear : left;
    }
    .line
    {
      fill         : none;
      stroke       : rgb(70,130,180);
      stroke-width : 2px;
    }
    .line
    {
      fill         : none;
      stroke       : rgb(70,130,180);
      stroke-width : 2px;
    }
    .heatmap rect
    {
      shape-rendering : crispEdges;
    }
    path.domain
    {
      opacity : 0.5;
    }
    g.tick line
    {
      stroke : rgb(0,0,0);
    }
    g.tick text
    {
      font-size : 10px;
    }
    .barw rect
    {
      shape-rendering : crispEdges;
      fill            : rgb(70,130,180);
    }
    path.domain
    {
      opacity : 0.5;
    }
    g.tick line
    {
      stroke : rgb(0,0,0);
    }
    g.tick text
    {
      font-size : 10px;
    }
    .barw rect
    {
      shape-rendering : crispEdges;
      fill            : rgb(70,130,180);
    }
    path.domain
    {
      opacity : 0.5;
    }
    g.tick line
    {
      stroke : rgb(0,0,0);
    }
    g.tick text
    {
      font-size : 10px;
    }
    body .chart
    {
      float  : left;
      border : solid 1px rgb(128,128,128);
      margin : 10px 10px 10px 10px;
    }
    body .chart .axis path, .axis line
    {
      fill            : none;
      stroke          : rgb(0,0,0);
      shape-rendering : crispEdges;
    }
    body .chart .x.axis path
    {
      display : none;
    }
    body .chart .tick text
    {
      font-size : 8px;
    }
    /* Generated with Clay, http://fvisser.nl/clay */</style>
    </head>
    <body><div class="play"><div id="buttons"><button id="btnQuit" type="button"><i class="icon-eject"></i></button><button id="btnReset" type="button"><i class="icon-fast-backward"></i></button><button class="on" id="btnStop" type="button"><i class="icon-stop"></i></button><button class="" id="btnGo" type="button"><i class="icon-play"></i></button><button id="btnStepForward" type="button"><i class="icon-step-forward"></i></button><button id="btnForward" type="button"><i class="icon-forward"></i></button><button id="btnFForward" type="button"><i class="icon-fast-forward"></i></button></div><div id="sliders"><i class="icon-fighter-jet icon-large"></i><input max="5.0" value="1" name="Speed" step="0.1" min="0.0" id="paramSpeed" type="range"><input value="1" id="textSpeed" type="text"></div><div id="framecount"><i id="spinner" class="icon-spinner icon-large"></i><input value="11" id="textFrame" type="text">of<input value="?" id="textTotalFrame" type="text"></div></div><svg height="150" width="150" id="charttext" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"></g><g class="y axis"></g><g><text y="30" x="30" id="textid0" class="text">next: 0.5812918576753627</text></g></g></svg><svg height="150" width="150" id="chartline" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"><g transform="translate(0,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">3</text></g><g transform="translate(11,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">4</text></g><g transform="translate(22,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">5</text></g><g transform="translate(33,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">6</text></g><g transform="translate(44,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">7</text></g><g transform="translate(55,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">8</text></g><g transform="translate(66,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">9</text></g><g transform="translate(77,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">10</text></g><g transform="translate(88,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">11</text></g><g transform="translate(99,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">12</text></g><g transform="translate(110,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">13</text></g><path d="M0,6V0H110V6" class="domain"></path></g><g class="y axis"><g transform="translate(0,116.89166840084448)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.5</text></g><g transform="translate(0,99.10785421787041)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.0</text></g><g transform="translate(0,81.32404003489637)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.5</text></g><g transform="translate(0,63.54022585192232)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.0</text></g><g transform="translate(0,45.75641166894826)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.5</text></g><g transform="translate(0,27.972597485974212)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">1.0</text></g><g transform="translate(0,10.188783303000166)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">1.5</text></g><path d="M-6,0H0V120H-6" class="domain"></path></g><g><path d="M0,120L11,1.5741892687541492L22,35.98588632220555L33,78.81526517136453L44,0L55,88.51447008875371L66,79.55037252616732L77,109.71550140288092L88,30.912335479336875L99,79.6084911671595L110,42.86505308597342" id="line0" class="line"></path></g></g></svg><svg height="150" width="150" id="chartscroll" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"><g transform="translate(0,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">7.0</text></g><g transform="translate(11,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">7.5</text></g><g transform="translate(22,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">8.0</text></g><g transform="translate(33,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">8.5</text></g><g transform="translate(44,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">9.0</text></g><g transform="translate(55,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">9.5</text></g><g transform="translate(66,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">10.0</text></g><g transform="translate(77,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">10.5</text></g><g transform="translate(88,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">11.0</text></g><g transform="translate(99,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">11.5</text></g><g transform="translate(110,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">12.0</text></g><path d="M0,6V0H110V6" class="domain"></path></g><g class="y axis"><g transform="translate(0,114.67921654536939)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.2</text></g><g transform="translate(0,103.84687154020867)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.0</text></g><g transform="translate(0,93.01452653504793)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.8</text></g><g transform="translate(0,82.1821815298872)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.6</text></g><g transform="translate(0,71.34983652472647)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.4</text></g><g transform="translate(0,60.517491519565745)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.2</text></g><g transform="translate(0,49.68514651440502)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.0</text></g><g transform="translate(0,38.852801509244294)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.2</text></g><g transform="translate(0,28.020456504083565)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.4</text></g><g transform="translate(0,17.188111498922826)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.6</text></g><g transform="translate(0,6.355766493762087)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.8</text></g><path d="M-6,0H0V120H-6" class="domain"></path></g><g><path transform="" d="M0,87.71546259748486L22,74.065101030108L44,120L66,0L88,74.15360301904877L110,18.20137675925342" id="line0" class="line"></path></g></g></svg><svg height="150" width="150" id="chartheatmap" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"><g transform="translate(0,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-3.0</text></g><g transform="translate(9.166666666666666,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-2.5</text></g><g transform="translate(18.333333333333332,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-2.0</text></g><g transform="translate(27.5,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-1.5</text></g><g transform="translate(36.666666666666664,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-1.0</text></g><g transform="translate(45.833333333333336,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-0.5</text></g><g transform="translate(55,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">0.0</text></g><g transform="translate(64.16666666666667,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">0.5</text></g><g transform="translate(73.33333333333333,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">1.0</text></g><g transform="translate(82.5,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">1.5</text></g><g transform="translate(91.66666666666667,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">2.0</text></g><g transform="translate(100.83333333333333,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">2.5</text></g><g transform="translate(110,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">3.0</text></g><path d="M0,6V0H110V6" class="domain"></path></g><g class="y axis"><g transform="translate(0,120)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-3.0</text></g><g transform="translate(0,110)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-2.5</text></g><g transform="translate(0,100)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-2.0</text></g><g transform="translate(0,90)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.5</text></g><g transform="translate(0,80.00000000000001)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-1.0</text></g><g transform="translate(0,69.99999999999999)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">-0.5</text></g><g transform="translate(0,60)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.0</text></g><g transform="translate(0,49.99999999999999)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.5</text></g><g transform="translate(0,40.00000000000001)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">1.0</text></g><g transform="translate(0,30)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">1.5</text></g><g transform="translate(0,19.999999999999996)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">2.0</text></g><g transform="translate(0,10.000000000000004)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">2.5</text></g><g transform="translate(0,0)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">3.0</text></g><path d="M-6,0H0V120H-6" class="domain"></path></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.333333333333332" y="0" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(191, 212, 229);" height="19.999999999999986" width="18.333333333333332" y="20" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.000000000000014" width="18.333333333333332" y="39.999999999999986" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999993" width="18.333333333333332" y="60" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.00000000000001" width="18.333333333333332" y="80" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.333333333333332" y="100" x="0"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.333333333333332" y="0" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999986" width="18.333333333333332" y="20" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.000000000000014" width="18.333333333333332" y="39.999999999999986" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(148, 182, 211);" height="19.999999999999993" width="18.333333333333332" y="60" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.00000000000001" width="18.333333333333332" y="80" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.333333333333332" y="100" x="18.333333333333332"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.333333333333336" y="0" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(163, 193, 218);" height="19.999999999999986" width="18.333333333333336" y="20" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(93, 145, 189);" height="20.000000000000014" width="18.333333333333336" y="39.999999999999986" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(184, 207, 226);" height="19.999999999999993" width="18.333333333333336" y="60" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(176, 202, 223);" height="20.00000000000001" width="18.333333333333336" y="80" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.333333333333336" y="100" x="36.666666666666664"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.33333333333333" y="0" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999986" width="18.33333333333333" y="20" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(70, 130, 180);" height="20.000000000000014" width="18.33333333333333" y="39.999999999999986" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(158, 190, 216);" height="19.999999999999993" width="18.33333333333333" y="60" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.00000000000001" width="18.33333333333333" y="80" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.33333333333333" y="100" x="55"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.333333333333343" y="0" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999986" width="18.333333333333343" y="20" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.000000000000014" width="18.333333333333343" y="39.999999999999986" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999993" width="18.333333333333343" y="60" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(187, 209, 228);" height="20.00000000000001" width="18.333333333333343" y="80" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.333333333333343" y="100" x="73.33333333333333"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20" width="18.33333333333333" y="0" x="91.66666666666667"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999986" width="18.33333333333333" y="20" x="91.66666666666667"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.000000000000014" width="18.33333333333333" y="39.999999999999986" x="91.66666666666667"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999993" width="18.33333333333333" y="60" x="91.66666666666667"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="20.00000000000001" width="18.33333333333333" y="80" x="91.66666666666667"></rect></g><g class="heatmap"><rect style="fill: rgb(255, 255, 255);" height="19.999999999999996" width="18.33333333333333" y="100" x="91.66666666666667"></rect></g></g></svg><svg height="150" width="150" id="charthist" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"><g transform="translate(9.166666984558105,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-2.5</text></g><g transform="translate(27.5,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-1.5</text></g><g transform="translate(45.83333206176758,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-0.50</text></g><g transform="translate(64.16666412353516,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">0.50</text></g><g transform="translate(82.5,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">1.5</text></g><g transform="translate(100.83333587646484,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">2.5</text></g><path d="M0,6V0H110V6" class="domain"></path></g><g class="y axis"><g transform="translate(0,120)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.00</text></g><g transform="translate(0,104.01297735059723)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.05</text></g><g transform="translate(0,88.02595470119446)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.10</text></g><g transform="translate(0,72.03893205179172)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.15</text></g><g transform="translate(0,56.051909402388944)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.20</text></g><g transform="translate(0,40.06488675298618)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.25</text></g><g transform="translate(0,24.077864103583426)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.30</text></g><g transform="translate(0,8.09084145418067)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.35</text></g><path d="M-6,0H0V120H-6" class="domain"></path></g><g class="barw"><rect height="0" width="18.333333333333332" y="120" x="0"></rect></g><g class="barw"><rect height="53.986316174248344" width="18.333333333333332" y="66.01368382575166" x="18.333333333333332"></rect></g><g class="barw"><rect height="120" width="18.333333333333336" y="0" x="36.666666666666664"></rect></g><g class="barw"><rect height="95.13320229624" width="18.33333333333333" y="24.86679770376" x="55"></rect></g><g class="barw"><rect height="50.62093451756692" width="18.333333333333343" y="69.37906548243308" x="73.33333333333333"></rect></g><g class="barw"><rect height="0" width="18.33333333333333" y="120" x="91.66666666666667"></rect></g></g></svg><svg height="150" width="150" id="charthistadapt" class="chart"><g transform="translate(30,10)"><g transform="translate(0,120)" class="x axis"><g transform="translate(4.583333492279053,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-1.5</text></g><g transform="translate(19.86111068725586,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-1.0</text></g><g transform="translate(35.13888931274414,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">-0.5</text></g><g transform="translate(50.41666793823242,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">0.0</text></g><g transform="translate(65.69444274902344,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">0.5</text></g><g transform="translate(80.97222137451172,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">1.0</text></g><g transform="translate(96.25,0)" style="opacity: 1;" class="tick"><line x2="0" y2="6"></line><text x="0" y="9" style="text-anchor: middle;" dy=".71em">1.5</text></g><path d="M0,6V0H110V6" class="domain"></path></g><g class="y axis"><g transform="translate(0,120)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.00</text></g><g transform="translate(0,108.2192153930664)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.05</text></g><g transform="translate(0,96.43843078613281)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.10</text></g><g transform="translate(0,84.65764617919922)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.15</text></g><g transform="translate(0,72.87686157226562)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.20</text></g><g transform="translate(0,61.09607696533203)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.25</text></g><g transform="translate(0,49.3152961730957)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.30</text></g><g transform="translate(0,37.53451156616211)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.35</text></g><g transform="translate(0,25.753725051879883)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.40</text></g><g transform="translate(0,13.972941398620605)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.45</text></g><g transform="translate(0,2.192157506942749)" style="opacity: 1;" class="tick"><line y2="0" x2="-6"></line><text y="0" x="-9" style="text-anchor: end;" dy=".32em">0.50</text></g><path d="M-6,0H0V120H-6" class="domain"></path></g><g class="barw"><rect height="39.862236440396686" width="18.333333333333332" y="80.13776355960331" x="0"></rect></g><g class="barw"><rect height="47.00836799055216" width="9.166666666666664" y="72.99163200944784" x="18.333333333333332"></rect></g><g class="barw"><rect height="120" width="9.166666666666668" y="0" x="27.499999999999996"></rect></g><g class="barw"><rect height="19.709472926187715" width="18.333333333333336" y="100.29052707381229" x="36.666666666666664"></rect></g><g class="barw"><rect height="37.92342897610038" width="18.33333333333333" y="82.07657102389962" x="55"></rect></g><g class="barw"><rect height="27.308181351063894" width="36.66666666666667" y="92.6918186489361" x="73.33333333333333"></rect></g></g></svg></body>
    <p style="clear:left"></p>
</html>

<br>


What is hdcharts?
===

This is a project written in haskell for creating dynamic, updateable charts.

Stack
---

Chart Rendering - browser-based using the [d3js](http://d3js.org/) library
Page rendering - [jmacro](https://hackage.haskell.org/package/jmacro), [lucid](https://hackage.haskell.org/package/lucid) and [clay](https://hackage.haskell.org/package/clay)
Process - [websockets](https://hackage.haskell.org/package/websockets) and [mvc](https://hackage.haskell.org/package/mvc)
Serving - [happstack](https://hackage.haskell.org/package/happstack-server)


How to run hdcharts
===

test/example.hs contains the working version of each current chart type.  Just:

1. `cabal build`
2. run `dist/build/example/example` (sandboxed of course!).
3. point browser to http://localhost:8001/
4. pick a chart and click Play button

Each chart has a testPlay function (that tests if it plays ok) which can provide a quick check and a starting point for developing your own.

Why does it exist?
===

The project is mostly a hobby-horse of mine.  I'm at the visual end of the analytical spectrum and static charts lead to dulled and static opinion.  Dynamic systems (which is pretty much everything) is best understood with the use of dynamic tools.


Why isn't the project using ghcjs, charts, diagrams, snap, gtk, tcp, ggplot ...
===

It might in the future.  The current stack is just where it's meandered to after a series of (potentially unfortunate) design decisions:

- any rendering outside of a browser is a little risky right now in terms of picking winners.  Even my nanna is watching tv inside a browser these days. And once you choose to target the browser, you choose to use javascript.  And within this context, d3js is the mature front-runner for browser charting excellence.

- Since I've never coded in javascript, I looked at ghcjs and retreated in fear.  It seemed a bit too monolithic to attempt something where lots of learning was needed.  And I couldn't see how I was going to hook up with the d3js library. Meanwhile, with jmacro I could start with d3 example snippets lying around, put quasi-quotes around them, and they would kind of work.

- chart looks fine but I couldn't work the dynamic angle out.  Maybe if I knew more about diagrams ...








