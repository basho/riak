var zIndex = 0;
var gIndex = 0;
var colors = ["pink", "yellow", "green", "blue"];
var CurrentGroupId = '';

function renderNote(note) {
    var noteDisplay = NoteTemplate.clone();

    noteDisplay
        .data('note', note)
        .css('left', note.object.x)
        .css('top', note.object.y)
        .css('z-index', note.object.z)
        .addClass(note.object.color)
        .mousedown(zTop)
        .draggable({opacity: 0.75, stop:dragfun})
        .find('div.picker').click(pickColor).end()
        .find('textarea').val(note.object.text).focus(zTop).blur(upd).end();

    $("div#board").append(noteDisplay);
    $('textarea', noteDisplay).focus();
}

function renderNotes(list) {
    for (n in list) renderNote(list[n]);
}

function renderGroup(group) {
    var groupDisplay = GroupTemplate.clone();

    groupDisplay
        .data('group', group)
        .attr('id', 'group_'+group.key)
        .click(function() { loadGroup(group.key); })
        .find('input').val(group.object.name).blur(updGrp).end();

    if (CurrentGroupId == group.key)
        groupDisplay.addClass('active');

    $("div#groups div.new").before(groupDisplay);
}

function loadGroup(groupid) {
    CurrentGroupId = groupid;
    $("div#board").empty();
    Client.walk(['groups', groupid],
                [{bucket:'notes'}],
                function(res) {
                    renderNotes(res.results[0]);
                },
                true);
    $('div.group').removeClass('active').removeClass('active');
    $('div#group_'+groupid).addClass('active');
}

function topOfNoteDisplay(noteDisplay) {
    return noteDisplay.is('div.note') ? noteDisplay : noteDisplay.parents('div.note');
}

function dragfun(el, ui) {
    var noteDisplay = topOfNoteDisplay($(el.target));
    var note = noteDisplay.data('note');
    if (!note) return; //delete happened

    note.object.x = ui.position.left;
    note.object.y = ui.position.top;
    upd(el);
};

function zTop(el) {
    var noteDisplay = topOfNoteDisplay($(el.target));
    zIndex += 1; 
    noteDisplay.css('z-index', zIndex); 
    noteDisplay.data('note').z = zIndex;
}

function pickColor(el) {
    var noteDisplay = topOfNoteDisplay($(el.target));
    var note = noteDisplay.data('note');

    var color = note.object.color;
    var nextIndex = colors.indexOf(color) + 1;
    if (nextIndex >= colors.length) nextIndex = 0;
    var nextColor = colors[nextIndex];

    note.object.color = nextColor;
    noteDisplay.removeClass(color).addClass(nextColor);
    upd(el);
}

function upd(el) {
    var noteDisplay = topOfNoteDisplay($(el.target));
    var note = noteDisplay.data('note');
    note.object.text = $('textarea', noteDisplay).val();

    Client.store(note, function(updated) {
        noteDisplay.data('note', updated);
    });
}

function topOfGroupDisplay(groupDisplay) {
    return groupDisplay.is('div.group') ? groupDisplay : groupDisplay.parents('div.group');
}

function updGrp(el) {
    var groupDisplay = topOfGroupDisplay($(el.target));
    var group = groupDisplay.data('group');
    group.object.name = $('input', groupDisplay).val();
    
    Client.store(group, function(updated) {
        groupDisplay.data('group', updated);
    });
}

function rand(upper){
    return Math.floor(Math.random()*upper) + 50
}

function init(){
    NoteTemplate = $("div#templates div.note");
    GroupTemplate = $("div#templates div.group");
    Client = new JiakClient('/jiak/');

    $("div.trash").droppable({
 	accept: "div.note",
 	activeClass: 'trash-active',
 	hoverClass: "trash-hover",
	tolerance: "touch",
 	drop: function(e, ui) {
            var noteid = $(ui.draggable).data('note').key;
	    $(ui.draggable).draggable("destroy").fadeOut().remove();
	    Client.remove("notes", noteid, null);
	}	
    });
    
    Client.fetch("groups", "", function(bucket) {
        if (bucket.keys.length > 0) {
            for (var i in bucket.keys)
                Client.fetch("groups", bucket.keys[i], renderGroup);

            loadGroup(bucket.keys[i]);
        } else {
            var firstGroup = {
                "bucket":"groups",
                "object":{"name":"Group A"},
                "links":[]
            };
            Client.store(firstGroup, function(newgroup) {
                renderGroup(newgroup);
                loadGroup(newgroup.key)
            });
        }
    });
}

$(document).ready(function(){
    $('div.add').click(function(){
	zIndex += 1;
	var note = {
	    "bucket": "notes",
	    "object": {
		"text": "",
		"x": rand(50),
		"y": rand(50),
		"z": zIndex,
		"color": "yellow"
	    },
            "links":[["groups", CurrentGroupId, "mygroup"]]
	}
	Client.store(note, renderNote);
    });
    $('div.new').click(function() {
        gIndex += 1;
        var group = {
            "bucket": "groups",
            "object": {
                "name":"Group "+gIndex
            },
            "links":[]
        }
        Client.store(group, renderGroup);
    });
    init();
});
