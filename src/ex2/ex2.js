/* Establish a connection to the server */
var ws = new WebSocket("ws://localhost:8080/");

/* Table to hold the rendered data */
var table  = document.getElementById('table');

/* The first row is the table heading */
var headerFlag = true;

/* Add new data to the table */
function log(data) {
    var row = document.createElement("tr");
    for (var i = 0; i < data.length; i++) {
        var column = document.createElement(headerFlag ? "th" : "td");
        column.innerHTML = data[i];
        row.appendChild(column);
    }
    table.appendChild(row);
}

/* Send request for more data if bottom of page has been reached */
function load(e) {
    var pageTopOffset = document.documentElement.scrollTop;
    var windowHeight  = window.innerHeight
    var pageHeight    = document.documentElement.scrollHeight;

    if (pageTopOffset + windowHeight >= pageHeight) {
        var json = JSON.stringify({ "_msg": "more" });
        ws.send(json);
    }
}

/* Action to perform when the connection is first opened */
ws.onopen = function(e) {

    /* Send request for initial data */
    ws.send(JSON.stringify({ "_msg": "init" }));

    /* Load the rest lazily */
    window.addEventListener('scroll', load, false);
};

/* Action to perform when data is received from the server */
ws.onmessage = function (m) {
    log(JSON.parse(m.data)._row);
    if (headerFlag) {
        headerFlag = false;
    }
}

