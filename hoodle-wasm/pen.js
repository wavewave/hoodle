var target = document.getElementById("box");

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

function onPointerDown(e) {
    console.log("on pointerdown");
    shoutPointerType(e);
}

function onPointerUp(e) {
    console.log("on pointerup");
    shoutPointerType(e);
}

function onPointerMove(e) {
    //console.log("on pointermove");
    //shoutPointerType(e);
}

target.addEventListener("pointerdown", onPointerDown);
target.addEventListener("pointerup"  , onPointerUp);
target.addEventListener("pointermove", onPointerMove);

console.log ("pen.js is loaded");
