
function msg (from, to, text, typ, ev1, tt) {
    return new Diagram.Signal(from, typ | (Diagram.ARROWTYPE.FILLED << 2), to, text, ev1, tt)
}

function click_cb (sig, msg, moredata) {
    var last = sig.data[sig.data.length - 1]
    _.each (moredata, function (datum) {
        var dat = datum[0] != "" ? datum[0] + ": " : "" ;
        var txt = dat + datum[1];
        if (txt != last)
            sig.data.push (txt);
        last = txt
    })
    return function () {
        $("#details").empty().append(
          _.map (sig.data, function (d) {
            return $("<li><pre>").find("pre").append(d).end()
          })
        )
    }
}

function raw_click_cb (sig, moredata) {
    return function () {
        $("#details").empty().append(
          _.map (moredata, function (d) {
            return $("<li><pre>").find("pre").append(d).end()
          })
        )
    }
}

function process (me, you, in_record) {
    var txt = in_record.message;
    if (in_record.event == "message") {
        //msg
        var from = me ;
        var to = you ;
        if (in_record.direction == "in") {
            from = you ; to = me ;
        }
        return msg(from, to, txt, Diagram.LINETYPE.SOLID);
    } else if (in_record.event == "raw") {
        var from = me ;
        var to = you ;
        if (in_record.direction == "in") {
            from = you ; to = me ;
        }
        var txt = "" + in_record.count + " bytes " + txt + " " + in_record.layer;
        return msg(from, to, txt, Diagram.LINETYPE.DOTTED);
    } else { //if (in_record.event == "note") {
        //note
        if (in_record.data == "")
            return new Diagram.Note(me, Diagram.PLACEMENT.LEFTOF, txt)
        return new Diagram.Note(me, Diagram.PLACEMENT.LEFTOF, txt);
    }
}

function insertSignals (diagram, signals, data) {
    for (var i = 0 ; i < signals.length ; i++) {
        var now = signals[i];
        if (now.type == "Signal") {
            if (data[i].event == "message")
                now.ev1 = click_cb(now, now.message, data[i].data);
            else
                now.ev1 = raw_click_cb(now, data[i].data);
            diagram.addSignal(now);
        } else {
            diagram.addSignal(now);
        }
    }
}

var filter = [ "parse" , "kdf" ] ;

function switch_filter (filt) {
    if (filter.indexOf(filt) == -1)
        filter.push(filt)
    else
        filter = filter.filter(function (x) { return x != filt })
}

function filterdata (data) {
    if (data.event == "message")
        return (filter.indexOf("parse") != -1)
    if (data.event == "kdf")
        return (filter.indexOf("kdf") != -1)
    if (data.event == "raw")
        return (filter.indexOf(data.layer) != -1)
    return true
}

var data = [] ;

function processData (data1) {
    var diagram = new Diagram();
    var you = diagram.getActor("Me");
    var me = diagram.getActor("You");

    data = data1;

    var filtered = _.filter(data, function (x) { return filterdata(x) });
    var signals = _.map(filtered, function (x) { return process(me, you, x) });
    insertSignals(diagram, signals, filtered);

    var options = {
        theme: "simple" //hand
    };

    $("#diagram, #details").empty ();

    // Draw
    diagram.drawSVG($("#diagram")[0], options);
}

function initialise () {
    $(".chapter-NONE").show();

    var clicked = function (x) {
        return (function () {
            switch_filter(x);
            processData(data);
        })
    };

    $("#kdf_check").click(clicked("kdf"));
    $("#parse_check").click(clicked("parse"));
    $("#wire_check").click(clicked("wire"));
    $("#crypt_check").click(clicked("crypted"));
    $("#plain_check").click(clicked("plain"));

    $.ajax({ url: "/diagram.json" }).done( function (data) { processData(data) })
}
