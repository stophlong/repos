// PLUGIN_INFO {{ =========================================================== //

var PLUGIN_INFO =
    <KeySnailPlugin>
    <name>yet-another-direct-bookmark</name>
    <description>Direct Post to Social Bookmarks by KeySnail</description>
    <description lang="ja">KeySnailからソーシャルブックーマークに直接ポスト</description>
    <version>0.1</version>
    <updateURL>http://hogehoge.com</updateURL>
    <iconURL>http://hogehoge.com</iconURL>
    <author mail="r_takaishi@eiliant.com" homepage="http://d.hatena.ne.jp/r_takaishi/">r_takaishi</author>
    <license document="http://www.opensource.org/licenses/mit-license.php">The MIT License</license>
    <license lang="ja">MIT ライセンス</license>
    <minVersion>1.0.5</minVersion>
    <include>main</include>
    <provides>
    <ext>yadb-post</ext>
    </provides>
    <detail><![CDATA[
=== 使い方 ===
==== 設定 ====
    ]]></detail>
    </KeySnailPlugin>
    

function yadbPost (aEvent) {
    var statusbar = document.getElementById('statusbar-display');
    var input = aEvent.originalTarget;
    var url = content.document.location.href;
    var yadbDeliciousUsername;
    var yadbDeliciousPassword;
    var passwordManager = Cc['@mozilla.org/login-manager;1'].getService(Ci.nsILoginManager);
    var logins = passwordManager.findLogins({},
    					    "https://secure.del.icio.us",
    					    "https://secure.del.icio.us",
    					    null);
    if (logins.length > 0) {
	[yadbDeliiciousUsername, yadbDeliciousPassword] = [logins[0].username, logins[0].password];
    } else {
        var promptUser = { value : this.loginPrompt.user }, promptPass = { value : this.loginPrompt.password };
        var promptSvc = Cc["@mozilla.org/embedcomp/prompt-service;1"]
            .getService(Ci.nsIPromptService);
	
        var nsLoginInfo = new Components.Constructor("@mozilla.org/login-manager/loginInfo;1",
						     Ci.nsILoginInfo,
						     "init");
	
        var ret = promptSvc.promptUsernameAndPassword(
            window, form, this.loginPrompt.description,
            promptUser, promptPass, null, {}
        );
        if(ret){
            [yadbDeliciousUsername, yadbDeliciousPassword] = [promptUser.value, promptPass.value];
            var formLoginInfo = new nsLoginInfo(form,
						post, null,
						user, password, '', '');
            passwordManager.addLogin(formLoginInfo);
        } else {
            
        }
    }


    //statusbar.label = "hoge";
    statusbar.label = yadbDeliciousUsername + " | " + yadbDeliciousPassword;
    // prompt.read("Bookmark: ", function(aStr) {

    // 	var comment = "";
    // 	var tags= [];

    // 	if(aStr.length > 0) comment = aStr;

    // 	var re = /\[([^\]]+)\]([^\[].*)?/g;

    // 	if(/^\[[^\]]+\]/.test(comment)) {
    // 	    var tag, text;
    // 	    while((tag = re.exec(comment))) {
    // 	 	[, tag, text] = tag;
    // 	 	tags.push(tag);
    // 	    }
    // 	    comment = text || '';
    // 	}

    // 	var title = document.title;

    // 	var request_url = 'https://api.del.icio.us/v1/posts/add?' + [
    // 	    ['url', url], ['description', title], ['extended', comment], ['tags', tags.join(' ')]
    // 	].map(function(p) p[0] + '=' + encodeURIComponent(p[1])).join('&');
	
    // 	var xhr = new XMLHttpRequest();
    // 	xhr.open('GET', request_url, false, yadbDeliciousUsername, yadbDeliciousPassword);
    // 	xhr.send(null);
    // 	statusbar.label = xhr.statusText;
    // });
}

ext.add("yadb-post", yadbPost,
	M({ja: 'ソーシャルブックマークにポスト',
	   en: "Post to Social Bookmark"}));
