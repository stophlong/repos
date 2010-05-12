let PLUGIN_INFO =
<VimperatorPlugin>
<name>{NAME}</name>
<description>org-protocol interface for vimperator</description>
<description lang="ja">org-protocolインタフェース</description>
<author>Ryo Takaishi</author>
<version>0.1</version>
<minVersion>2.3.1</minVersion>
<maxVersion>2.3.1</maxVersion>
<updateURL>http://github.com/takaishi/repos/raw/master/vimperator/org-vimperator/org-vimperator.js</updateURL>
<detail><![CDATA[

== COMMANDS ==

:orgprotocol arg:
    connect to emacsclient by sub-protocol(arg)
    
]]></detail>
<detail lang="ja"><![CDATA[
== COMMANDS ==

:orgprotocol arg:
    sub-protocol(arg)でemacsclientに接続します

]]></detail>
</VimperatorPlugin>;


(function() {

    var evalFunc = window.eval;
    try {
        var sandbox = new Components.utils.Sandbox(window);
        if(Components.utils.evalInSandbox("true", sandbox) == true) {
            evalFunc = function(text) {
                return Components.utils.evalInSandbox(text, sandbox);
            }
        }
    } catch(e) { liberator.log('warning: org-vimperator.js is working with unsafe sandbox.');}

    var isUseMacOSX = typeof liberator.globalVariables.org_vimperator_mac_workaround == 'undefined' ?
        true : evalFunc(liberator.globalVariables.org_vimperator_mac_workaround);

    
    function orgProtocolSendURL(arg) {
        if (arg == "store-link") {
            var url = "store-link://" + encodeURIComponent( window.content.document.URL) 
                + "/" + encodeURIComponent( window.content.document.title);
        } else if (arg == "remember") {
            rememberTemplate = "";
            var url = "remember://" + rememberTemplate 
                + encodeURIComponent(window.content.document.URL) 
                + "/" + encodeURIComponent(window.content.document.title) 
                + "/" + encodeURIComponent(window.getSelection());
        } else {
            var url = ""
        }

        pref = Components.classes["@mozilla.org/preferences-service;1"]
            .getService(Components.interfaces.nsIPrefBranch);

        if( isUseMacOSX ) { // Workaround

            var tmpFileName = "~/.org-vimperator-mac.tmp"
            
            var file = Components.classes["@mozilla.org/file/local;1"]
            .createInstance(Components.interfaces.nsILocalFile);
            file.initWithPath( tmpFileName );
            if(file.exists() == false) {
                file.create( Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 420);
            }

            var stream = Components.classes["@mozilla.org/network/file-output-stream;1"]
            .createInstance(Components.interfaces.nsIFileOutputStream);

            stream.init(file, 0x02 | 0x08 | 0x10, 0666, 0);
            var finalString = "org-protocol://" + url + "\n";
            stream.write( finalString, finalString.length);
            stream.close();

        } else {
            var req = new XMLHttpRequest();
            try {
                req.open('POST', "org-protocol://" + url,true);
                req.send(null);
            } catch (ex) { }
        }
    }

    commands.addUserCommand(
        ["orgprotocol"],
        "org-protocol",
        function(arg){
            orgProtocolSendURL(arg);
        },
        {
            completer : function (context, args) {
                context.ancor = false;
                context.ignoreCase = true;
                context.title = ["url", "title"];
                context.completions = [
                    ["store-link", "store-link"],
                    ["remember", "remember"],
                ];
            },
        },
        true      
    );
    
})();
