var HIGHLIGHT = '#cc00cc';
var REGULAR = '#666666';

var cols = {
    'a':173,
    'b':325,
    'c':589,
    'd':797,
    'e':1005,
    'f':1195,
    'g':1402,
    'gg':1515,
    'h':1572,
    'i':1799,
    'j':1893,
    'k':1988,
    'l':2157,
    'll':2346,
    'm':2403,
    'mm':2535,
    'n':2554,
    'o':2649,
    'oo':2781,
    'ooo':2801,
    'p':2894,
    'q':3007
};

var rows = {
    '1':221,
    '2':298,
    '3':373,
    '4':448,
    '5':524,
    '6':599,
    '7':675,
    '8':751,
    '9':826,
    '10':902,
    '11':977,
    '12':1053,
    '13':1129,
    '14':1204,
    '15':1280,
    '16':1355,
    '17':1431,
    '18':1506,
    '19':1583,
    '20':1658,
    '21':1734,
    '22':1809,
    '23':1885,
    '24':1961,
    '25':2036,
    '26':2112
};

var edges = {
    'b14b13':['b14','b13'],

    'b13b12':['b13','b12'],
    'b13503':['b13','503'],

    'b12b11':['b12','b11'],
    'b12501':['b12','501'],

    'b11b10':['b11','b10'],
    'b11414':['b11','414'],

    'b10b9':['b10','b9'],
    'b10405':['b10','405'],

    'b9b8':['b9','b8'],
    'b9400':['b9','400'],

    'b8b7':['b8','b7'],
    'b8401':['b8','401'],

    'b7b6':['b7','b6'],
    'b7403':['b7','403'],

    'b6b5':['b6','b5'],
    'b6501':['b6','501a'],

    'b5b4':['b5','b4'],
    'b5415':['b5','415'],

    'b4b3':['b4','b3'],
    'b4413':['b4','b4'],

    'b3c3':['b3','c3'],
    'b3200':['b3','200'],

    'c3c4':['c3','c4'],
    'c3d4':['c3','d3','d4'],

    'c4d4':['c4','d4'],
    'c4406':['c4','406'],

    'd4d5':['d4','d5'],
    'd4e5':['d4','e4','e5'],

    'd5e5':['d5','e5'],
    'd5406':['d5','d7','406'],

    'e5e6':['e5','e6'],
    'e5f6':['e5','f5','f6'],

    'e6f6':['e6','f6'],
    'e6406':['e6','e7','406'],

    'f6f7':['f6','f7'],
    'f6g7':['f6','g6','g7'],

    'f7g7':['f7','g7'],
    'f7406':['f7','406'],

    'g7g8':['g7','g8'],
    'g7h7':['g7','h7'],

    'g8g9':['g8','g9'],
    'g8h10':['g8','h8','h10'],

    'g9g11':['g9','g11'],
    'g9h10':['g9','gg9','gg10','h10'],

    'g11h10':['g11','gg11','gg10','h10'],
    'g11412':['g11','g18','412a'],

    'h7i7':['h7','i7'],
    'h7412':['h7','412'],

    'h10h11':['h10','h11'],
    'h10i12':['h10','i10','i12'],

    'h11h12':['h11','h12'],
    'h11i12':['h11','i11','i12'],

    'h12i12':['h12','i12'],
    'h12412':['h12','412a'],

    'i4p3':['i4','i3','p3'],
    'i4301':['i4','301'],

    'i7i4':['i7','i4'],
    'i7k7':['i7','k7'],

    'i12l13':['i12','l12','l13'],
    'i12i13':['i12','i13'],

    'i13k13':['i13','k13'],
    'i13j18':['i13','i17','j17','j18'],

    'j18412':['j18','412a'],
    'j18304':['j18','304'],

    'k5l5':['k5','l5'],
    'k5301':['k5','301'],

    'k7k5':['k7','k5'],
    'k7l7':['k7','l7'],

    'k13j18':['k13','k17','j17','j18'],
    'k13l13':['k13','l13'],

    'l5m5':['l5','m5'],
    'l5307':['l5','307'],

    'l7m7':['l7','m7'],
    'l7404':['l7','l8','404'],

    'l13l14':['l13','l14'],
    'l13m16':['l13','m13','m16'],

    'l14l15':['l14','l15'],
    'l14m16':['l14','m14','m16'],

    'l15l17':['l15','l17'],
    'l15m16':['l15','ll15','ll16','m16'],

    'l17m16':['l17','ll17','ll16','m16'],
    'l17304':['l17','304'],

    'm5n5':['m5','n5'],
    'm5410':['m5','m4','410'],

    'm7n11':['m7','n7','n11'],
    'm7404':['m7','404'],

    'm16m20':['m16','m20'],
    'm16n16':['m16','n16'],

    'm20o20':['m20','o20'],
    'm20202':['m20','202'],

    'n5n11':['n5','n11'],
    'n5410':['n5','410'],

    'n11p11':['n11','p11'],
    'n11303':['n11','303'],

    'n16n11':['n16','n11'],
    'n16o16':['n16','o16'],

    'o14p11':['o14','o11','p11'],
    'o14409':['o14','409a'],

    'o16o14':['o16','o14'],
    'o16o18':['o16','o18'],

    'o18200':['o18','200a'],
    'o18300':['o18','oo18','300'],

    'o20o18':['o20','o18'],
    'o20204':['o20','204'],

    'p3p11':['p3','p11'],
    'p3409':['p3','409'],

    'p11o20':['p11','p20','o20'],
    'p11201':['p11','q11','201']
};

var ends = {
    '200': {col:'a', row:'3', width:190},
    '200a': {col:'mm', row:'18', width:116},
    '201': {col:'q', row:'12', width:154},
    '202': {col:'m', row:'21', width:116},
    '204': {col:'o', row:'21', width:152},

    '300': {col:'oo', row:'19', width:152},
    '301': {col:'k', row:'4', width:154},
    '303': {col:'m', row:'11', width:116},
    '304': {col:'l', row:'18', width:116},
    '307': {col:'l', row:'4', width:154},

    '400': {col:'a', row:'9', width:190},
    '401': {col:'a', row:'8', width:190},
    '403': {col:'a', row:'7', width:190},
    '404': {col:'m', row:'8', width:116},
    '405': {col:'a', row:'10', width:190},
    '406': {col:'c', row:'7', width:152},
    '409': {col:'p', row:'2', width:116},
    '409a': {col:'oo', row:'14', width:116},
    '410': {col:'n', row:'4', width:116},
    '412': {col:'h', row:'6', width:152},
    '412a': {col:'h', row:'18', width:152},
    '413': {col:'a', row:'4', width:190},
    '414': {col:'a', row:'11', width:190},
    '415': {col:'a', row:'5', width:190},

    '501a': {col:'a', row:'6', width:190},
    '501': {col:'a', row:'12', width:190},
    '503': {col:'a', row:'13', width:190}
};

var canvas;

function decorateTrace() {
    trace[0].x = cols[trace[0].d[0]];
    trace[0].y = rows[trace[0].d.slice(1)];
    trace[0].previewCalls = previewCalls(trace[0]);

    for (var i = 1; i < trace.length; i++) {
        trace[i].x = cols[trace[i].d[0]];
        trace[i].y = rows[trace[i].d.slice(1)];
        trace[i].previewCalls = previewCalls(trace[i]);
        
        var path = edges[trace[i-1].d+trace[i].d];
        if (path) {
            trace[i].path = [path.length-1];
            for (var p = 1; p < path.length; p++) {
                trace[i].path[p-1] = getSeg(path[p-1], path[p], p == path.length-1);
            }
        } else {
            trace[i].path = [];
        }
    }
    
    var path = edges[trace[i-1].d+response.code];
    if (path) {
        var end = ends[path[path.length-1]];
        response.x = cols[end.col];
        response.y = rows[end.row];
        response.width = end.width;
        response.type = 'normal';

        response.path = [path.length-1];
        for (var p = 1; p < path.length; p++) {
            response.path[p-1] = getSeg(path[p-1], path[p], p == path.length-1);
        }
    } else {
        var ld = trace[trace.length-1];
        response.x = ld.x+50;
        response.y = ld.y-50;
        response.width = 38;
        response.type = 'other';

        response.path = [
            {x1: ld.x+10, y1: ld.y-10,
             x2: ld.x+36, y2: ld.y-36}
        ];
    }
};

function previewCalls(dec) {
    var prev = '';
    for (var i = 0; i < dec.calls.length; i++) {
        if (dec.calls[i].output != "wmtrace_not_exported")
            prev += '<li>'+dec.calls[i].module+':'+dec.calls[i]['function']+'</li>';
    }
    return prev;
};

function drawTrace() {
    drawDecision(trace[0]);
    for (var i = 1; i < trace.length; i++) {
        drawPath(trace[i].path);
        drawDecision(trace[i]);
    }

    drawPath(response.path);
    drawResponse();
};

function drawResponse() {
    if (response.type == 'normal') {
        var context = canvas.getContext('2d');
        context.strokeStyle=HIGHLIGHT;
        context.lineWidth=4;

        context.beginPath();
        context.rect(response.x-(response.width/2),
                     response.y-19,
                     response.width,
                     38);
        context.stroke();
    } else {
        var context = canvas.getContext('2d');
        context.strokeStyle='#ff0000';
        context.lineWidth=4;

        context.beginPath();
        context.arc(response.x, response.y, 19,
                    0, 2*3.14159, false);
        context.stroke();

    }
};

function drawDecision(dec) {
    var context = canvas.getContext('2d');

    if (dec.previewCalls == '')
        context.strokeStyle=REGULAR;
    else
        context.strokeStyle=HIGHLIGHT;
    context.lineWidth=4;

    context.beginPath();
    context.moveTo(dec.x,    dec.y-19);
    context.lineTo(dec.x+19, dec.y);
    context.lineTo(dec.x,    dec.y+19);
    context.lineTo(dec.x-19, dec.y);
    context.closePath();
    context.stroke();
};

function drawPath(path) {
    var context = canvas.getContext('2d');
    context.strokeStyle=REGULAR;
    context.lineWidth=4;

    context.beginPath();
    context.moveTo(path[0].x1, path[0].y1);
    for (var p = 0; p < path.length; p++) {
        context.lineTo(path[p].x2, path[p].y2);
    }
    context.stroke();
};

function getSeg(p1, p2, last) {
    var seg = {
        x1:cols[p1[0]],
        y1:rows[p1.slice(1)]
    };
    if (ends[p2]) {
        seg.x2 = cols[ends[p2].col];
        seg.y2 = rows[ends[p2].row];
    } else {
        seg.x2 = cols[p2[0]];
        seg.y2 = rows[p2.slice(1)];
    }

    if (seg.x1 == seg.x2) {
        if (seg.y1 < seg.y2) {
            seg.y1 = seg.y1+19;
            if (last) seg.y2 = seg.y2-19;
        } else {
            seg.y1 = seg.y1-19;
            if (last) seg.y2 = seg.y2+19;
        }
    } else {
        //assume seg.y1 == seg.y2
        if (seg.x1 < seg.x2) {
            seg.x1 = seg.x1+19;
            if (last) seg.x2 = seg.x2-(ends[p2] ? (ends[p2].width/2) : 19);
        } else {
            seg.x1 = seg.x1-19;
            if (last) seg.x2 = seg.x2+(ends[p2] ? (ends[p2].width/2) : 19);
        }
    }
    return seg;
};

function traceDecision(name) {
    for (var i = trace.length-1; i >= 0; i--)
        if (trace[i].d == name) return trace[i];
};

var detailPanels = {};
function initDetailPanels() {
    var windowWidth = document.getElementById('sizetest').clientWidth;
    var infoPanel = document.getElementById('infopanel');
    var panelWidth = windowWidth-infoPanel.offsetLeft;

    var panels = {
        'request': document.getElementById('requestdetail'),
        'response': document.getElementById('responsedetail'),
        'decision': document.getElementById('decisiondetail')
    };

    var tabs = {
        'request': document.getElementById('requesttab'),
        'response': document.getElementById('responsetab'),
        'decision': document.getElementById('decisiontab')
    };

    var decisionId = document.getElementById('decisionid');
    var decisionCalls = document.getElementById('decisioncalls');
    var callInput = document.getElementById('callinput');
    var callOutput = document.getElementById('calloutput');

    var lastUsedPanelWidth = windowWidth-infoPanel.offsetLeft;

    var setPanelWidth = function(width) {
        infoPanel.style.left = (windowWidth-width)+'px';
        canvas.style.marginRight = (width+20)+'px';
        panelWidth = width;
    };
    setPanelWidth(panelWidth);

    var ensureVisible = function() {
        if (windowWidth-infoPanel.offsetLeft < 10)
            setPanelWidth(lastUsedPanelWidth);
    };

    var decChoices = '';
    for (var i = 0; i < trace.length; i++) {
        decChoices += '<option value="'+trace[i].d+'">'+trace[i].d+'</option>';
    }
    decisionId.innerHTML = decChoices;
    decisionId.selectedIndex = -1;

    decisionId.onchange = function() {
        detailPanels.setDecision(traceDecision(decisionId.value));
    }

    detailPanels.setDecision = function(dec) {
        decisionId.value = dec.d;

        var calls = [];
        for (var i = 0; i < dec.calls.length; i++) {
            calls.push('<option value="'+dec.d+'-'+i+'">');
            calls.push(dec.calls[i].module+':'+dec.calls[i]['function']);
            calls.push('</option>');
        }
        decisionCalls.innerHTML = calls.join('');
        decisionCalls.selectedIndex = 0;

        decisionCalls.onchange();
    };

    detailPanels.show = function(name) {
        for (p in panels) {
            if (p == name) {
                panels[p].style.display = 'block';
                tabs[p].className = 'selectedtab';
            }
            else {
                panels[p].style.display = 'none';
                tabs[p].className = '';
            }
        }
        ensureVisible();
    };

    detailPanels.hide = function() {
        setPanelWidth(0);
    }

    decisionCalls.onchange = function() {
        var val = decisionCalls.value;
        if (val) {
            var dec = traceDecision(val.substring(0, val.indexOf('-')));
            var call = dec.calls[parseInt(val.substring(val.indexOf('-')+1, val.length))];

            if (call.output != "wmtrace_not_exported") {
                callInput.style.color='#000000';
                callInput.innerHTML = call.input;
                if (call.output != null) {
                    callOutput.style.color = '#000000';
                    callOutput.innerHTML = call.output;
                } else {
                    callOutput.style.color = '#ff0000';
                    callOutput.textContent = 'Error: '+call.module+':'+call['function']+' never returned';
                }
            } else {
                callInput.style.color='#999999';
                callInput.textContent = call.module+':'+call['function']+' was not exported';
                callOutput.textContent = '';
            }
        } else {
            callInput.textContent = '';
            callOutput.textContent = '';
        }
    };

    var headersList = function(headers) {
        var h = '';
        for (n in headers) h += '<li>'+n+': '+headers[n];
        return h;
    };

    document.getElementById('requestmethod').innerHTML = request.method;
    document.getElementById('requestpath').innerHTML = request.path;
    document.getElementById('requestheaders').innerHTML = headersList(request.headers);
    document.getElementById('requestbody').innerHTML = request.body;

    document.getElementById('responsecode').innerHTML = response.code;
    document.getElementById('responseheaders').innerHTML = headersList(response.headers);
    document.getElementById('responsebody').innerHTML = response.body;


    var infoControls = document.getElementById('infocontrols');
    var md = false;
    var dragged = false;
    var msoff = 0;
    infoControls.onmousedown = function(ev) {
        md = true;
        dragged = false;
        msoff = ev.clientX-infoPanel.offsetLeft;
    };

    infoControls.onclick = function(ev) {
        if (dragged) {
            lastUsedPanelWidth = panelWidth;
        }
        else if (panelWidth < 10) {
            switch(ev.target.id) {
            case 'requesttab': detailPanels.show('request'); break;
            case 'responsetab': detailPanels.show('response'); break;
            case 'decisiontab': detailPanels.show('decision'); break;
            default: ensureVisible();
            }
        } else {
            var name = 'none';
            switch(ev.target.id) {
            case 'requesttab': name = 'request'; break;
            case 'responsetab': name = 'response'; break;
            case 'decisiontab': name = 'decision'; break;
            }

            if (panels[name] && panels[name].style.display != 'block')
                detailPanels.show(name);
            else
                detailPanels.hide();
        }

        return false;
    };

    document.onmousemove = function(ev) {
        if (md) {
            dragged = true;
            panelWidth = windowWidth-(ev.clientX-msoff);
            if (panelWidth < 0) {
                panelWidth = 0;
                infoPanel.style.left = windowWidth+"px";
            }
            else if (panelWidth > windowWidth-21) {
                panelWidth = windowWidth-21;
                infoPanel.style.left = '21px';
            }
            else
                infoPanel.style.left = (ev.clientX-msoff)+"px";

            canvas.style.marginRight = panelWidth+20+"px";
            return false;
        }
    };

    document.onmouseup = function() { md = false; };

    window.onresize = function() {
        windowWidth = document.getElementById('sizetest').clientWidth;
        infoPanel.style.left = windowWidth-panelWidth+'px';
    };
};

window.onload = function() {
    canvas = document.getElementById('v3map');

    initDetailPanels();

    var scale = 0.25;
    var coy = canvas.offsetTop;
    function findDecision(ev) {
        var x = (ev.clientX+window.pageXOffset)/scale;
        var y = (ev.clientY+window.pageYOffset-coy)/scale;

        for (var i = trace.length-1; i >= 0; i--) {
            if (x >= trace[i].x-19 && x <= trace[i].x+19 &&
                y >= trace[i].y-19 && y <= trace[i].y+19)
                return trace[i];
        }
    };

    var preview = document.getElementById('preview');
    var previewId = document.getElementById('previewid');
    var previewCalls = document.getElementById('previewcalls');
    function previewDecision(dec) {
        preview.style.left = (dec.x*scale)+'px';
        preview.style.top = (dec.y*scale+coy+15)+'px';
        preview.style.display = 'block';
        previewId.textContent = dec.d;

        previewCalls.innerHTML = dec.previewCalls;
    };

    function overResponse(ev) {
        var x = (ev.clientX+window.pageXOffset)/scale;
        var y = (ev.clientY+window.pageYOffset-coy)/scale;
        
        return (x >= response.x-(response.width/2)
                && x <= response.x+(response.width/2)
                && y >= response.y-19 && y <= response.y+19);
    };

    decorateTrace();

    var bg = new Image(3138, 2184);

    function drawMap() {
        var ctx = canvas.getContext("2d");

        ctx.save();
        ctx.scale(1/scale, 1/scale);
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(0, 0, 3138, 2184);
        ctx.restore();

        ctx.drawImage(bg, 0, 0);
        drawTrace();
    };

    bg.onload = function() {
        canvas.getContext("2d").scale(scale, scale);
        drawMap(scale);

        canvas.onmousemove = function(ev) {
            if (findDecision(ev)) {
                canvas.style.cursor = 'pointer';
                previewDecision(findDecision(ev));
            }
            else {
                preview.style.display = 'none';
                if (overResponse(ev))
                    canvas.style.cursor = 'pointer';
                else
                    canvas.style.cursor = 'default';
            }
        };

        canvas.onclick = function(ev) {
            var dec = findDecision(ev);
            if (dec) {
                detailPanels.setDecision(dec);
                detailPanels.show('decision');
            } else if (overResponse(ev)) {
                detailPanels.show('response');
            }
        };

        document.getElementById('zoomin').onclick = function() {
            scale = scale*2;
            canvas.getContext("2d").scale(2, 2);
            drawMap();
        };
 
        document.getElementById('zoomout').onclick = function() {
            scale = scale/2;
            canvas.getContext("2d").scale(0.5, 0.5);
            drawMap();
        };
    };

    bg.onerror = function() {
        alert('Failed to load background image.');
    };

    bg.src = 'static/map.png';
};
