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

function toSVGPointArray(svg,xys) {
    var ctm = svg.screenCTM();
    var xys_canvas = xys.map( function(xy) {
        var x = xy[0];
        var y = xy[1];
        var pt = (new SVG.Point(x,y)).transform(ctm.inverse());
        return [pt.x, pt.y];
    });
    var arr = new SVG.PointArray(xys_canvas);
    return arr;
    // console.log(arr);
    //console.log(xys_canvas);
    //return xys_canvas;
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

function drawPath(svg,xys) {
    var path = svg.polyline(xys).fill("none").stroke({width:0.2, color:'#f06'});
}


function preventDefaultTouchMove() {
  document.body.addEventListener("touchmove", function(e){e.preventDefault()}, { passive: false, useCapture: false });
}

// GHCJS start
h$main(h$mainZCMainzimain);
