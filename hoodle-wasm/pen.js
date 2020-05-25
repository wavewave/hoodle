var callback;
var isDrawing = false;

function getCanvasCoord(cvs,e) {
    var rect = cvs.getBoundingClientRect();
    var scaleX = canvas.width / rect.width;
    var scaleY = canvas.height / rect.height;
    return { x: (e.clientX - rect.left)*scaleX, y: (e.clientY - rect.top)*scaleY };
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

function drawRectangle(cvs, ctxt, e) {
    var p = getCanvasCoord(cvs,e);
    ctxt.fillStyle = "#ff0000";
    ctxt.fillRect(p.x,p.y,1,1);
}

function onPointerDown(e) {
    isDrawing = true;
    console.log("on pointerdown");
    //callback();
    shoutPointerType(e);
    shoutPointerCoord(e);
}

function onPointerUp(e) {
    isDrawing = false;
    console.log("on pointerup");
    shoutPointerType(e);
    shoutPointerCoord(e);
}

function onPointerMove(e) {
    if (isDrawing) {
        drawRectangle(canvas,context,e);
    }
}

var canvas = document.getElementById("box");
var context = canvas.getContext("2d");

canvas.addEventListener("pointerdown", onPointerDown);
canvas.addEventListener("pointerup"  , onPointerUp);
canvas.addEventListener("pointermove", onPointerMove);

var background = new Image();
background.src = "img_640x480.jpg";
background.onload = function() {
    context.drawImage(background,0,0);
};

// asterius callback
setInterval(function () { callback(); },1000);

console.log ("pen.js is loaded");
