window.ErlangRED = (function(){
    //
    // Used to highlight the links in markdown text - links that highlight
    // nodes in the flow. This is used for documentational purposes, see
    // https://discourse.nodered.org/t/highlighting-nodes-and-groups-from-the-info-text-box/84020
    // for details
    //
    function handleTextReferences() {
        var getDataIds = (ele) => {
            return ($(ele).data("ids") || $(ele).data("id") || "").split(",");
        };

        var setHrefClass = (ele) => {
            $(ele).attr('href', '#');
            $(ele).addClass('ahl');
        };

        var nodesInGrp = (grpId) => {
            var ndeIds = []
            let ndsInGrp = RED.nodes.group(grpId) || { nodes: [] }
            ndsInGrp.nodes.forEach( n => {
                if ( n.type == "group" ) {
                    ndeIds = ndeIds.concat( nodesInGrp(n.id) )
                } else {
                    ndeIds.push(n.id)
                }
            })
            return ndeIds
        };

        var highlightNodes = (ndeIds) => {
            // move the workspace to the first node of
            // the group but don't make the highlight blink
            RED.view.reveal(ndeIds[0], false)
            RED.view.redraw();

            RED.tray.hide();
            RED.view.selectNodes({
                selected: ndeIds,
                onselect: function(selection) { RED.tray.show(); },
                oncancel: function() { RED.tray.show(); }
            });
        };

        $('a.ahl-node-only').each(function (idx, ele) {
            setHrefClass(ele);
            $(ele).removeClass('ahl-node-only');
            $(ele).css('color', '#f4a0a0')

            var ndeIds = getDataIds(ele);

            $(ele).on('click', function (e) {
                if ( ndeIds.length == 1 ) {
                    RED.view.reveal(ndeIds[0], true)
                    RED.view.redraw();
                } else {
                    highlightNodes(ndeIds)
                }
            });
        });

        $('a.ahl-group-only').each(function (idx, ele) {
            setHrefClass(ele);
            $(ele).removeClass('ahl-group-only');
            $(ele).css('color', '#f4a0a0')

            // here the ids are group ides, need to find all nodes in those
            // groups and highlight them
            var grpIds = getDataIds(ele);
            var ndeIds = []
            grpIds.forEach( grpId => {
                ndeIds = ndeIds.concat( nodesInGrp( grpId ) )
            })

            $(ele).on('click', function (e) {
                if ( ndeIds.length == 1 ) {
                    RED.view.reveal(ndeIds[0], true)
                    RED.view.redraw();
                } else {
                    highlightNodes(ndeIds)
                }
            });
        });

        $('a.ahl-link-node').each(function (idx, ele) {
            setHrefClass(ele);
            $(ele).removeClass('ahl-link-node');
            $(ele).css('color', '#f4a0a0')

            var ndeIds = getDataIds(ele);

            $(ele).on('click', function (e) {
                if ( ndeIds.length == 1 ) {
                    RED.view.reveal(ndeIds[0], true)
                    RED.view.redraw();
                } else {
                    highlightNodes(ndeIds)
                }
            });
        });
    }

    return {
        init: () => {
            RED.events.on( 'markdown:rendered', () => {
                setTimeout( () => {
                    handleTextReferences()
                },1000)
            })
        }
    };
})();


while ( typeof(RED) == undefined ) {
    sleep(500);
}
ErlangRED.init()
