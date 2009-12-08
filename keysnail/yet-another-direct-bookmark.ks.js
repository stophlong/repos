// PLUGIN_INFO {{ =========================================================== //

var PLUGIN_INFO =
    <KeySnailPlugin>
    <name>yet-another-direct-bookmark</name>
    <description>Direct Post to Social Bookmarks by KeySnail</description>
    <description lang="ja">KeySnailからオンラインブックーマークに直接ポスト</description>
    <version>0.1</version>
    <updateURL>http://hogehoge.com</updateURL>
    <iconURL>http://hogehoge.com</iconURL>
    <author mail="stillpedant@gmail.com" homepage="http://d.hatena.ne.jp/mooz/">mooz</author>
    <license document="http://www.opensource.org/licenses/mit-license.php">The MIT License</license>
    <license lang="ja">MIT ライセンス</license>
    <minVersion>1.0.5</minVersion>
    <include>main</include>
    <provides>
    <ext>bookmark</ext>
    </provides>
    <detail>
    </detail>
    </KeySnailPlugin>
    

function tweet (aEvent) {
    var statusbar = document.getElementById('statusbar-display');
    var input = aEvent.originalTarget;
    var url = content.document.location.href;
    prompt.read("Bookmark: ", function(aStr) {

	var username = "r_takaishi";
	var password = "1qazxsw2";
	var comment = "";
	var tags= [];

	if(aStr.length > 0) comment = aStr;

	var re = /\[([^\]]+)\]([^\[].*)?/g;

	if(/^\[[^\]]+\]/.test(comment)) {
	    var tag, text;
	    while((tag = re.exec(comment))) {
	 	[, tag, text] = tag;
	 	tags.push(tag);
	    }
	    comment = text || '';
	}

	var title = document.title;

	var request_url = 'https://api.del.icio.us/v1/posts/add?' + [
	    ['url', url], ['description', title], ['extended', comment], ['tags', tags.join(' ')]
	].map(function(p) p[0] + '=' + encodeURIComponent(p[1])).join('&');
	
	var xhr = new XMLHttpRequest();
	xhr.open('GET', request_url, false, username, password);
	xhr.send(null);
	statusbar.label = xhr.statusText;;
    });
}

ext.add("bookmark-post", tweet,
	M({ja: 'post',
	   en: "Post"}));
