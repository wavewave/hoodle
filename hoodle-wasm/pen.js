var target = document.getElementById("box");

function onPointerDown(e) {
    console.log("on pointerdown");
}

function onPointerUp(e) {
    console.log("on pointerup");
}

function onPointerMove(e) {
    console.log("on pointermove");
}

target.addEventListener("pointerdown", onPointerDown);
target.addEventListener("pointerup"  , onPointerUp);
target.addEventListener("pointermove", onPointerMove);

console.log ("pen.js is loaded");
