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
}

function drawPath(svg,xys) {
    var path = svg.polyline(xys).fill("none").stroke({width:0.2, color:'#f06'});
}


function preventDefaultTouchMove() {
  document.body.addEventListener("touchmove", function(e){e.preventDefault()}, { passive: false, useCapture: false });
}


var canvas = document.getElementById("overlay");
var context = canvas.getContext("2d");


var offcanvas = document.createElement("canvas");
offcanvas.width = 1280;
offcanvas.height = 1024;
var offcontext = offcanvas.getContext("2d");

function refresh() {
    context.clearRect(0,0,offcanvas.width,offcanvas.height);
    context.drawImage(offcanvas,0,0);
}

function overlay_point(x,y) {
   var rect = canvas.getBoundingClientRect();
   var scaleX = canvas.width / rect.width;
   var scaleY = canvas.height / rect.height;

    var cx = (x - rect.left)*scaleX;
    var cy = (y - rect.top)*scaleY;
    console.log(x + "," + y);
    var radius = 1*scaleX;
    offcontext.beginPath();
    offcontext.arc(cx, cy, radius, 0, 2 * Math.PI, false);
    offcontext.fillStyle = 'green';
    offcontext.fill();
    offcontext.lineWidth = 1;
    offcontext.strokeStyle = '#003300';
    offcontext.stroke();
}

function clear_overlay() {
    offcontext.clearRect(0,0,offcanvas.width,offcanvas.height);
}

// GHCJS start
h$main(h$mainZCMainzimain);
