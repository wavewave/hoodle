
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
    //var x = e.clientX;
    //var y = e.clientY;
    var rect = cvs.getBoundingClientRect();
    var scaleX = canvas.width / rect.width;
    var scaleY = canvas.height / rect.height;
    var x = (e.clientX - rect.left)*scaleX;
    var y = (e.clientY - rect.top)*scaleY;

    ctxt.fillStyle = "#ff0000";
    ctxt.fillRect(x,y,5,5);
}

function onPointerDown(e) {
    console.log("on pointerdown");
    shoutPointerType(e);
    shoutPointerCoord(e);
    drawRectangle(canvas,context,e);
}

function onPointerUp(e) {
    console.log("on pointerup");
    shoutPointerType(e);
    shoutPointerCoord(e);
}

function onPointerMove(e) {
    //console.log("on pointermove");
    //shoutPointerType(e);
    shoutPointerCoord(e);
}

var canvas = document.getElementById("box");
var context = canvas.getContext("2d");

canvas.addEventListener("pointerdown", onPointerDown);
canvas.addEventListener("pointerup"  , onPointerUp);
canvas.addEventListener("pointermove", onPointerMove);

console.log ("pen.js is loaded");
