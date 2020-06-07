var callback;
var isDrawing = false;
var xy = new SVG.PointArray();


function getCanvasCoord(svg,e) {
    var x = e.clientX;
    var y = e.clientY;
    var ctm = svg.screenCTM();
    var point = (new SVG.Point(x,y)).transform(ctm.inverse());
    return { x: point.x, y: point.y };
}

function shoutPointerType(e) {
    switch(e.pointerType) {
    case "mouse":
        console.log("it's a mouse event!");
        break;
    case "pen":
        console.log("it's a pen event!");
        break;
    case "touch":
        console.log("it's a touch event!");
        break;
    }
}

function shoutPointerCoord(e) {
    var x = e.clientX;
    var y = e.clientY;
    console.log("(x,y) = (" + x + " , " + y + ")");
}

function startLineBit(svg,e) {
    var p = getCanvasCoord(svg,e);
    xy = new SVG.PointArray([ p.x, p.y ]);
}

function drawLineBit(svg,e) {
    var p = getCanvasCoord(svg,e);
    xy.push([ p.x, p.y ]);
}

function endLineBit(svg,e) {
    var p = getCanvasCoord(svg,e);
    xy.push([ p.x, p.y ]);
    var path = svg.polyline(xy).fill("none").stroke({width:0.2, color:'#f06'});
}


function onPointerDown(e) {
    isDrawing = true;
    // console.log("on pointerdown");
    //shoutPointerType(e);
    //shoutPointerCoord(e);
    startLineBit(svg,e);
}

function onPointerUp(e) {
    callback();
    console.log("callback onPointerUp");
    console.log(callback);
    isDrawing = false;
    //console.log("on pointerup");
    //shoutPointerType(e);
    //shoutPointerCoord(e);
    endLineBit(svg,e);
}


function onPointerMove(e) {
    if (isDrawing) {
        drawLineBit(svg,e);
    }
}

var svg = SVG("#box");

document.body.addEventListener("touchmove", function(e){e.preventDefault()}, { passive: false, useCapture: false });

svg.on("pointerdown", onPointerDown);
svg.on("pointerup"  , onPointerUp);
svg.on("pointermove", onPointerMove);

var n = 0;
var start = null;
function step(timestamp) {
    if(!start) start = timestamp;
    var progress = timestamp - start;
    window.requestAnimationFrame(step);
}

window.requestAnimationFrame(step);

console.log ("pen.js is loaded");

// GHCJS start
h$main(h$mainZCMainzimain);
