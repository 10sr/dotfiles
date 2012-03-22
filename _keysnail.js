// ========================== KeySnail Init File =========================== //

// この領域は, GUI により設定ファイルを生成した際にも引き継がれます
// 特殊キー, キーバインド定義, フック, ブラックリスト以外のコードは, この中に書くようにして下さい
// ========================================================================= //
//{{%PRESERVE%
// prompt.rows                = 12;
// prompt.useMigemo           = false;
// prompt.migemoMinWordLength = 2;
// prompt.displayDelayTime    = 300;
// command.kill.killRingMax   = 15;
// command.kill.textLengthMax = 8192;

//////////////////////////////////////
//// sitelocalkeymap
var local = {};
plugins.options["site_local_keymap.local_keymap"] = local;
function fake(k, i) function () { key.feed(k, i); };
function pass(k, i) [k, fake(k, i)];
function ignore(k, i) [k, null];

// ext.add("ext-name", function () {}, "ext description");
// style.register("");
// local["^http://"] = [['a', function(ev, arg){}],];

///////////////////////////////////////////
//// firefox
// style.register("#bookmarksPanel > hbox,#history-panel > hbox {display: none !important;} //#urlbar-container{max-width: 500px !important;}");

///////////////////////////////////
//検索エンジン
plugins.options["search-url-list"] = [
    ["bing","http://bing.com/search?q=%q"],
    ["yatwitter search","http://yats-data.com/yats/search?query=%q"],
    ["twitter search","http://search.twitter.com/search?q=%q&lang=all"],
    ["tospy", "http://topsy.com/s?allow_lang=ja&q=%q"],
    ["2ch","http://2ch-ranking.net/search.php?q=%q&imp=and&order=time"],
    ["I\'m feelig lucky!","http://www.google.co.jp/search?q=%q&btnI=kudos"],
    ["uncyclopedia","http://ja.uncyclopedia.info/wiki/%q"],
    ["wikipedia","http://ja.wikipedia.org/wiki/%q"],
    ["nicovideo.jp","http://www.nicovideo.jp/search/%q"],
    ["alc","http://eow.alc.co.jp/%q/UTF-8/"],
    ["google map","http://maps.google.co.jp/maps?hl=ja&q=%q&um=1&ie=UTF-8&sa=N&tab=wl"],
    ["weblio","http://www.weblio.jp/content_find?query=%q"],
    ["shoutcast","http://www.shoutcast.com/Internet-Radio/%q"],
    ["10sr.posterous.com","http://www.google.com/search?q=%q&ie=UTF-8&oe=UTF-8&hl=ja&domains=10sr.posterous.com&sitesearch=10sr.posterous.com"],
    ["delicious 10sr","http://delicious.com/10sr?addtag=%q&setcount=50&opennew=1"],
    ["open raw","%r"],
];

plugins.options["my-keysnail-bookmarks"] = [
    "twitter.com",
];

// sitelocal

//////////////////////////////////////////
// 2ch chaika
local["^http://127.0.0.1:8823/thread/"] = [
    ['k', function (ev, arg) {
        curl = window.content.location.href;
        kurl = curl.replace(/http:.*thread\/(.*\/).*/, "chaika://post/$1");
        window.content.location.href = kurl;
    }
    ],
];

local["^http://w2.p2.2ch.net/p2/read.php"] = [
    ['k', function (ev, arg) {
        var url = window.content.location.href;
        var pt = /host=(.*?)&bbs=(.*?)&key=(.*?)&ls=/ ;
        var result = url.match(pt);
        var k = format("chaika://post/http://%s/test/read.cgi/%s/%s/", result[1], result[2], result[3]);
        window.content.location.href = k;
    }
    ],
];

/////////////////////////////////////////
// feedly用マップ
local["^http://www.feedly.com/"] = [
    ['d', null],
    ['j', null],
    ['k', null],
    ['n', null],
    ['p', null],
    ['o', null],
    ['b', null],
    ['S', null],
    ['s', null],
    ['?', null],
    ['r', null],
    ['g', null],
    // ['x', function (ev, arg) {ev.target.dispatchEvent(key.stringToKeyEvent("g", true));}],
    ['l', function (ev, arg) {window.content.location.href = "http://www.feedly.com/home#latest";}],
    [['t', 'p'], function (ev, arg) {ev.target.dispatchEvent(key.stringToKeyEvent("t", true));}],
    [['t', 'w'], function (ev, arg) {ext.exec("twitter-client-tweet", arg, ev);}],
];

/////////////////////////////////////////
//nicovideo用
local["http://(www|tw|es|de|)\.nicovideo\.jp\/(watch|playlist)/*"] = [
    ["i", function (ev, arg) { ext.exec("nicoinfo", arg); }],
    ["p", function (ev, arg) { ext.exec("nicopause", arg); }],
    ["o", function (ev, arg) { ext.exec("nicommentvisible", arg); }],
    ["m", function (ev, arg) { ext.exec("nicomute", arg); }],
    [".", function (ev, arg) { ext.exec("nicovolumeIncrement", arg); }],
    [",", function (ev, arg) { ext.exec("nicovolumeDecrement", arg); }],
    ['f', function (ev, arg) {
        curl = window.content.location.href;
        kurl = curl.replace(/nicovideo.jp/, "nicovideofire.jp");
        window.content.location.href = kurl;
    }
    ],
];

/////////////////////////////////////////
// tumblr/dashboard
local["^http://www.tumblr.com/dashboard"] = [
    //        ["C-<left>", function (ev, arg) {gBrowser.mTabContainer.advanceSelectedTab(-1, true); }],
    //        ["C-<right>", function (ev, arg) {gBrowser.mTabContainer.advanceSelectedTab(1, true); }],
    ["<left>", function (ev, arg) { window.content.location.href = "http://www.tumblr.com/dashboard"; }],
    //        ["<right>", null],
    ["J", function (ev, arg) {
        if (window.loadURI) {
            loadURI("javascript:(function(){b=20;s=100;t=document.getElementById('next_page_link').href.split('/')[5];max=t.substr(0,t.length-5);min=max-s;i=Math.floor(Math.random()*(max-min)+min);u=(i<b)?'http://www.tumblr.com/dashboard':'http://www.tumblr.com/dashboard/2/'+i+'00000';window.content.location.href=u;}())");
        }
    }],
];

///////////////////////////////////////////
// plugin option

plugins.options["instapaper.close_after_post"] = true;
plugins.options["instapaper.initial_comment_function"] = function(){
    var now = new Date();
    return "[" + now.toString() + "]";
};

//////////////////////////////////////////
// yatc
style.register("#keysnail-twitter-client-container{ display:none !important; }");
plugins.options["twitter_client.popup_new_statuses"]           = false;
plugins.options["twitter_client.automatically_begin"]          = false;
plugins.options["twitter_client.automatically_begin_list"]     = false;
plugins.options["twitter_client.timeline_count_beginning"]     = 0;
plugins.options["twitter_client.timeline_count_every_updates"] = 0;

plugins.options["twitter_client.tweet_keymap"] = {
    "C-RET" : "prompt-decide",
    "RET"   : ""
};

plugins.options["twitter_client.jmp_id"] = "10sr";
plugins.options["twitter_client.jmp_key"] = "R_c51f889a77cb4b4e993ed868f65083f5";
plugins.options["twitter_client.use_jmp"] = true;

////////////////////////////////////////////
// エクステ

ext.add('my-setpref', function(){
    util.setPrefs(
        {
            "browser.bookmarks.max_backups":0,
            "browser.cache.memory.capacity":16384,
            "browser.download.manager.closeWhenDone":true,
            "browser.download.useDownloadDir":false,
            "browser.fullscreen.autohide":false,
            "browser.search.openintab":true,
            "browser.sessionhistory.max_total_viewers":8,
            "browser.sessionstore.restore_on_demand":true,
            "browser.tabs.closeWindowWithLastTab":false,
            "browser.tabs.loadDivertedInBackground": true,
            "browser.urlbar.autocomplete.enabled":false,
            "browser.urlbar.trimURLs":false,
            "dom.disable_window_open_feature.location": false,
            "dom.max_script_run_time": 30,
            "extensions.chaika.bbsmenu.open_new_tab":true,
            "extensions.chaika.bbsmenu.open_single_click":false,
            "extensions.chaika.board.open_new_tab":true,
            "extensions.chaika.board.open_single_click":false,
            "extensions.foxage2ch.openThreadInTab":true,
            "extensions.saveimageinfolder.general-duplicatefilenamevalue":1,
            "extensions.saveimageinfolder.general-fileprefixvalue":"%yyyy%%MM%%dd%-%hh%%mm%%ss%_",
            "extensions.saveimageinfolder.usecache":true,
            "extensions.tabutils.openTabNext":1,
            "extensions.tabutils.styles.current":"{\"bold\":true,\"italic\":false,\"underline\":true,\"strikethrough\":false,\"color\":true,\"colorCode\":\"#000000\",\"bgColor\":false,\"bgColorCode\":\"#000000\",\"outline\":false,\"outlineColorCode\":\"#000000\"}",
            "extensions.tabutils.styles.unread":"{\"bold\":false,\"italic\":false,\"underline\":false,\"strikethrough\":false,\"color\":true,\"colorCode\":\"#CC0000\",\"bgColor\":false,\"bgColorCode\":\"undefined\",\"outline\":false,\"outlineColorCode\":\"undefined\"}",
            "extensions.yass.edgetype":0,
            "extensions.yass.selectedpreset":"red",
            "font.default.x-western":"sans-serif",
            "general.warnOnAboutConfig":false,
            "keyword.URL":"http://www.bing.com/search?q=",
            "network.dns.disableIPv6":true,
            "refcontrol.actions":"@DEFAULT=@FORGE",
            "scrapbook.tabs.open":true
        } 
    );
    if(/^Linux/.test(navigator.platform)){
        util.setPrefs(
            {
                "browser.cache.disk.parent_directory":"/tmp",
                "browser.cache.disk.capacity":524288
            }
        );
    }
    display.showPopup("Keysnail", "My prefs done.");
}, 'my setpref');

ext.add('auto-install-plugins', function(ev, arg){
    var urls = [
        'https://raw.github.com/mooz/keysnail/master/plugins/yet-another-twitter-client-keysnail.ks.js',
        'https://raw.github.com/mooz/keysnail/master/plugins/site-local-keymap.ks.js',
        'https://raw.github.com/azu/KeySnail-Plugins/master/JSReference/js-referrence.ks.js',
        'https://raw.github.com/gongo/keysnail_plugin/master/linksnail.ks.js',
        'https://raw.github.com/tkosaka/keysnail-plugin/master/nicontroller.ks.js',
        'https://raw.github.com/10sr/keysnail-plugin/master/shiitake.ks.js',
        'https://raw.github.com/10sr/keysnail-plugin/master/dig-url.ks.js',
        'https://raw.github.com/10sr/keysnail-plugin/master/instapaper.ks.js',
        'https://raw.github.com/gist/1976942/firefox-addon-manager.ks.js',
        'https://raw.github.com/gist/1450594/mstranslator.ks.js'
    ];

    function inst(a){
        if(a.length == 0){
            display.showPopup("auto-install-plugins", "All installation finished.");
        }else{
            var url = a.shift();
            var path = userscript.pluginDir + userscript.directoryDelimiter + url.match(/[^/]+$/)[0];
            if(plugins.context[path] === undefined){
                userscript.installPluginFromURL(url, function(){inst(a);});
            }else{
                inst(a);
            }
        }
    }
    inst(urls);
}, 'Install plugins automatically if not installed yet.');

ext.add('put-aside-this-page', function (ev, arg) {
    var n = gBrowser.mCurrentTab._tPos;
    gBrowser.moveTabTo(gBrowser.mCurrentTab, 0);
    if (n != 0) {
        gBrowser.selectedTab = gBrowser.mTabs[n];
    }
}, 'put aside this page');

ext.add('send-escape', function (ev, arg) {
    ev.target.dispatchEvent(key.stringToKeyEvent("ESC", true));
}, 'escape');

ext.add("open-hatebu-comment", function (ev, arg) {
    if (window.loadURI) {
        loadURI("javascript:location.href='http://b.hatena.ne.jp/entry?mode=more&url='+escape(location.href);");
    }
}, 'hatebu');

ext.add("fullscreen-page",function (ev) {
    getBrowser().selectedTab = getBrowser().addTab("http://home.tiscali.nl/annejan/swf/timeline.swf");
    BrowserFullScreen();
}, "fullscreen page");

ext.add("focus-on-content", function(){
    let(elem = document.commandDispatcher.focusedElement) elem && elem.blur();
    gBrowser.focus();
    content.focus();
}, "forcus on content");

ext.add("hide-sidebar", function(){
    var sidebarBox = document.getElementById("sidebar-box");
    if (!sidebarBox.hidden) {
        toggleSidebar(sidebarBox.getAttribute("sidebarcommand"));
    }
}, "hide-sidebar");

ext.add("close-and-next-tab", function (ev, arg) {
    var n = gBrowser.mCurrentTab._tPos;
    BrowserCloseTabOrWindow();
    gBrowser.selectedTab = gBrowser.mTabs[n];
}, "close and focus to next tab");

/////////////////////////////////////
// google itranslate
// use mstranslator instead
(function(){
    let targetLang = "ja"; // target lang to translate into
    let alternativeLang = "en"; // if given word is in targetLang, use this instead as a target lang
    function translate(word, target, next) {
        next("", "", " getting...");
        const base = "https://www.googleapis.com/language/translate/v2?key=%s&q=%s&target=%s";
        const apikey = "AIzaSyBq48p8NhFgaJ1DfUJ5ltbwLxeXpjEL86A";
        let ep = util.format(base, apikey, encodeURIComponent(word), target);
        util.httpGet(ep, false, function (res) {
            if (res.status === 200) {
                let json = decodeJSON(res.responseText);
                let srclang = json.data.translations[0].detectedSourceLanguage;
                if (target == srclang) {
                    lookupword(word, alternativeLang);
                } else {
                    let result = json.data.translations[0].translatedText;
                    next(srclang, target, result);
                }
            } else {
                next("", "", "ERROR!");
            }
        });
    };
    function echo(srclang, from, tglang, to){
        display.echoStatusBar(srclang + " : " + from + " -> " + tglang + " : " + to);
    };
    function decodeJSON(json) {
        return util.safeEval("(" + json + ")");
    };
    function lookupword(word, target){
        translate(word, target, function (src, tg, translated) {
            echo(src, word, tg, translated);
        });
    };
    function read (aInitialInput) {
        let prevText = "";

        prompt.reader({
            message : "word or sentence to translate:",
            initialinput : aInitialInput,
            onChange: function (arg) {
                let word = arg.textbox.value;
                if (word !== prevText) {
                    prevText = word;
                    lookupword(word, targetLang);
                }
            },
            callback: function (s){},
        });
    };
    ext.add("google-itranslate",function(){read(content.document.getSelection() || "");},"google itranslate");
})();

//////////////////////////////////////
//
ext.add("restart-firefox-add-menu", function(){
    const XUL_NS = "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";

    var cmdelm = document.createElementNS(XUL_NS, "command");
    cmdelm.setAttribute("id", "my_cmd_restartFirefoxKs")
    cmdelm.setAttribute("oncommand", "ext.exec('restart-firefox');");
    var commandset = document.getElementById("mainCommandSet");
    // menu.insertBefore(elm, menu.getElementById("menu_FileQuitItem"));
    commandset.appendChild(cmdelm);

    var menuelm = document.createElementNS(XUL_NS, "menuitem");
    menuelm.setAttribute("label", "Restart Firefox");
    menuelm.setAttribute("id", "my_menu_restartFirefoxKs")
    menuelm.setAttribute("command", "my_cmd_restartFirefoxKs");
    var menu = document.getElementById("menu_FilePopup");
    // menu.insertBefore(elm, menu.getElementById("menu_FileQuitItem"));
    menu.appendChild(menuelm);
}, "add restart firefox menu");

//////////////////////////////////////
// restart firefox
// http://keysnail.g.hatena.ne.jp/Shinnya/20100723/1279878815
ext.add("restart-firefox",function (ev) {
    const nsIAppStartup = Components.interfaces.nsIAppStartup;
    // Notify all windows that an application quit has been requested.
    var os = Components.classes["@mozilla.org/observer-service;1"]
        .getService(Components.interfaces.nsIObserverService);
    var cancelQuit = Components.classes["@mozilla.org/supports-PRBool;1"]
        .createInstance(Components.interfaces.nsISupportsPRBool);
    os.notifyObservers(cancelQuit, "quit-application-requested", null);
    // Something aborted the quit process. 
    if (cancelQuit.data)
        return;
    // Notify all windows that an application quit has been granted.
    os.notifyObservers(null, "quit-application-granted", null);
    // Enumerate all windows and call shutdown handlers
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
        .getService(Components.interfaces.nsIWindowMediator);
    var windows = wm.getEnumerator(null);
    while (windows.hasMoreElements()) {
        var win = windows.getNext();
        if (("tryToClose" in win) && !win.tryToClose())
            return;
    }
    Components.classes["@mozilla.org/toolkit/app-startup;1"].getService(nsIAppStartup)
        .quit(nsIAppStartup.eRestart | nsIAppStartup.eAttemptQuit);
}, "restart firefox");

/////////////////////////////////////////
// copy feed url
ext.add("copy-url", function () {
    const doc = content.document;

    let feeds = [[e.getAttribute("title"), e.getAttribute("href")]
                 for ([, e] in Iterator(doc.querySelectorAll(['link[type="application/rss+xml"]',
                                                              'link[type="application/atom+xml"]'])))];
    var uh = window.content.location.href.replace(/(.*?\/\/[^/]*)(\/.*)?/,"$1");
    for (i = 0; i < feeds.length; i++)
        if ( feeds[i][1].substr(0,1) == "/" ) feeds[i][1] = uh + feeds[i][1];
    feeds.unshift([window.content.document.title,window.content.location.href]);
    prompt.selector(
        {
            message    : "Select Feed",
            collection : feeds,
            callback   : function (i) {
                if (i >= 0)
                    command.setClipboardText(feeds[i][1]);
            }
        }
    );
}, "Copy url or feed url of current page");

///////////////////////////////////////
// 評価しちゃうっぽい とりあえずこんな感じで
ext.add("keysnail-setting-menu",function(){
    var settingmenulist = [["keysnail setting dialogue", function(){return function(){KeySnail.openPreference();};}],
                           ["keysnail plugin manager", function(){return function(){userscript.openPluginManager();}}],
                           ["firefox addon manager", function(){return function(){BrowserOpenAddonsMgr();};}],
                           ["reload .keysnail.js", function(){return function() {userscript.reload();};}],
                           // ["check for plugins update", function(){return function(){ext.exec("check-for-plugins-update");};}],
                           ["restart firefox", function(){return function(){ext.exec("restart-firefox");};}],
                          ];
    prompt.selector(
        {
            message    : "open setting dialog",
            collection : settingmenulist,
            callback   : function (i) { settingmenulist[i][1]()(); },
        });
},"open keysnail setting menu");

////////////////////////
//マルチプルタブハンドラ
ext.add("multiple-tab-handler-close-selected-and-current-tabs", function () {
    BrowserCloseTabOrWindow();
    // if (MultipleTabService) {
    //     //BrowserCloseTabOrWindow();
    //     //MultipleTabService.setSelection(gBrowser.mCurrentTab, true);
    MultipleTabService.closeTabs(MultipleTabService.getSelectedTabs());
    // } else {
    //     BrowserCloseTabOrWindow();}
}, '選択タブと現在のタブを閉じる');

ext.add("if-mth-exist", function() {
    if (MultipleTabService === undefined) display.echoStatusBar("mth not exist.");
},'if mth exist');

// search web
ext.add("query-then-engine", function () {
    prompt.reader({message : "Search Word?:", 
                   callback : function (q) {
                       if (q) {
                           prompt.selector({ message : "search \"" + q + "\" with?",
                                             collection : plugins.options["search-url-list"],
                                             width : [20,80],
                                             callback : function (i) { getBrowser().selectedTab = getBrowser().addTab(plugins.options["search-url-list"][i][1].replace("%r",q).replace("%q",encodeURIComponent(q))); },
                                           });
                       };
                   },
                   initialInput : content.document.getSelection() || "",
                  });
}, "enter search word and then select engine");

/////////////////////////////////////
// 閉じたタブリスト
ext.add("list-closed-tabs", function () {
    const fav = "chrome://mozapps/skin/places/defaultFavicon.png";
    var ss   = Cc["@mozilla.org/browser/sessionstore;1"].getService(Ci.nsISessionStore);
    var json = Cc["@mozilla.org/dom/json;1"].createInstance(Ci.nsIJSON);
    var closedTabs = [[tab.image || fav, tab.title, tab.url] for each (tab in json.decode(ss.getClosedTabData(window)))];

    if (!closedTabs.length)
        return void display.echoStatusBar("最近閉じたタブが見つかりませんでした", 2000);

    prompt.selector(
        {
            message    : "select tab to undo:",
            collection : closedTabs,
            flags      : [ICON | IGNORE, 0, 0],
            callback   : function (i) { if (i >= 0) window.undoCloseTab(i); }
        });
}, "List closed tabs");

ext.add("echo-closed-tabs", function () {
    var ss   = Cc["@mozilla.org/browser/sessionstore;1"].getService(Ci.nsISessionStore);
    var json = Cc["@mozilla.org/dom/json;1"].createInstance(Ci.nsIJSON);
    // var closedTabs = [[tab.image || fav, tab.title, tab.url] for each (tab in json.decode(ss.getClosedTabData(window)))];
    var lasttab = json.decode(ss.getClosedTabData(window))[0];
    dump = ""
    for (var i in lasttab) { dump += lasttab[i] + "\n"; }
    confirm(dump);

}, "List closed tabs");

///////////////////////////////
// http://malblue.tumblr.com/post/349001250/tips-japanese-keysnail-github
ext.add("list-tab-history", function () {
    const fav = "chrome://mozapps/skin/places/defaultFavicon.png";
    var tabHistory = [];
    var sessionHistory = gBrowser.webNavigation.sessionHistory;
    if (sessionHistory.count < 1)
        return void display.echoStatusBar("Tab history not exist", 2000);
    var curIdx = sessionHistory.index;
    for (var i = 0; i < sessionHistory.count; i++) {
        var entry = sessionHistory.getEntryAtIndex(i, false);
        if (!entry)
            continue;
        try {
            var iconURL = Cc["@mozilla.org/browser/favicon-service;1"]
                .getService(Ci.nsIFaviconService)
                .getFaviconForPage(entry.URI).spec;
        } catch (ex) {}
        tabHistory.push([iconURL || fav, entry.title, entry.URI.spec, i]);
    }
    for (var thIdx = 0; thIdx < tabHistory.length; thIdx++) {
        if (tabHistory[thIdx][3] == curIdx) break;
    }

    prompt.selector(
        {
            message     : "select history in tab",
            collection  : tabHistory,
            flags       : [ICON | IGNORE, 0, 0, IGNORE | HIDDEN],
            header      : ["Title", "URL"],
            initialIndex : thIdx,
            callback    : function(i) { if (i >= 0) gBrowser.webNavigation.gotoIndex(tabHistory[i][3]); }
        }); 
},  'List tab history');

//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "<delete>";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "";
key.macroEndKey          = "";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "Not defined";

// ================================= Hooks ================================= //

hook.setHook('KeySnailInitialized', function () {
    ext.exec("shiitake-enable-style");
});

hook.setHook('KeyBoardQuit', function (aEvent) {
    ext.exec("hide-sidebar");
    let(elem = document.commandDispatcher.focusedElement) elem && elem.blur();
    gBrowser.focus();
    content.focus();
    command.closeFindBar();
    if (util.isCaretEnabled()) {
        command.resetMark(aEvent);
    } else {
        goDoCommand("cmd_selectNone");
    }
    key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
});

hook.setHook('Unload', function () {
    util.getBrowserWindows().some(function (win) {
        if (win === window) {
            return false;
        }
        const ks = win.KeySnail;
        share.pluginUpdater = ks.getPluginUpdater(share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});


// ============================= Key bindings ============================== //

key.setGlobalKey('C-<right>', function () {
    gBrowser.mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setGlobalKey('C-<left>', function () {
    gBrowser.mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setGlobalKey('C-<up>', function () {
    var browser = getBrowser();
    if (browser.mCurrentTab.previousSibling) {
        browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos - 1);
    } else {
        browser.moveTabTo(browser.mCurrentTab, browser.mTabContainer.childNodes.length - 1);
    }
}, '選択中のタブを右へ');

key.setGlobalKey('C-<down>', function () {
    var browser = getBrowser();
    if (browser.mCurrentTab.nextSibling) {
        browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos + 1);
    } else {
        browser.moveTabTo(browser.mCurrentTab, 0);
    }
}, '選択中のタブを左へ');

key.setGlobalKey('M-:', function (ev) {
    command.interpreter();
}, 'JavaScript のコードを評価');

key.setViewKey('D', function (ev, arg) {
    ext.exec("dig-url", arg, ev);
}, 'dig url with selector', true);

key.setViewKey('x', function (aEvent, aArg) {
    ext.select(aArg, aEvent);
}, 'エクステ一覧');

key.setViewKey(['t', 'm'], function (ev, arg) {
    if (window.loadURI) {
        loadURI("javascript:window.location='http://api.tweetmeme.com/visit?url='+window.location;");
    }
}, 'open with tweetmeme');

key.setViewKey(['t', 'w'], function (ev, arg) {
    ext.exec("twitter-client-tweet", arg, ev);
}, 'つぶやく', true);

key.setViewKey(['t', 'p'], function (ev, arg) {
    ext.exec("twitter-client-tweet-this-page", arg, ev);
}, 'このページのタイトルと URL を使ってつぶやく', true);

key.setViewKey('u', function () {
    undoCloseTab();
}, '閉じたタブを元に戻す');

key.setViewKey('g', function () {
    goDoCommand("cmd_scrollTop");
}, 'ページ先頭へ移動');

key.setViewKey('G', function () {
    goDoCommand("cmd_scrollBottom");
}, 'ページ末尾へ移動');

key.setViewKey('r', function (aEvent) {
    BrowserReload();
}, '再読み込み');

key.setViewKey('m', function (ev, arg) {
    _fi.toogle();
}, 'fetchimiをトグル');

key.setViewKey('d', function (ev, arg) {
    if (window.loadURI) {
        loadURI("javascript:(function(){f='http://www.delicious.com/save?url='+encodeURIComponent(window.location.href)+'&title='+encodeURIComponent(document.title)+'&notes='+encodeURIComponent(''+(window.getSelection?window.getSelection():document.getSelection?document.getSelection():document.selection.createRange().text))+'&v=6&';a=function(){if(!window.open(f+'noui=1&jump=doclose','deliciousuiv6','location=1,links=0,scrollbars=0,toolbar=0,width=550,height=585'))location.href=f+'jump=yes'};if(/Firefox/.test(navigator.userAgent)){setTimeout(a,0)}else{a()}})()");
    }
}, 'deliciousでブックマーク');

key.setViewKey('p', function (ev, arg) {
    if (window.loadURI) {
        loadURI("javascript:var%20b=document.body;var%20POSTEROUS___bookmarklet_domain='http://posterous.com';if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.type='text/javascript');void(z.src='http://posterous.com/javascripts/bookmarklet2.js');void(b.appendChild(z));}else{}");
    }
}, 'posterousに投稿');

key.setViewKey('SPC', function (ev, arg) {
    MultipleTabService.toggleSelection(gBrowser.mCurrentTab);
    gBrowser.mTabContainer.advanceSelectedTab(1, true);
}, 'タブの選択をトグルして次のタブ');

key.setViewKey('S-SPC', function (ev, arg) {
    MultipleTabService.toggleSelection(gBrowser.selectedTab);
    gBrowser.mTabContainer.advanceSelectedTab(-1, true);
}, 'タブの選択をトグルして前のタブ');

key.setViewKey('z', function (ev, arg) {
    ext.exec("keysnail-setting-menu", arg, ev);
}, 'open keysnail setting menu', true);

key.setViewKey('C-SPC', function (ev, arg) {
    MultipleTabService.toggleSelection(gBrowser.selectedTab);
}, 'タブの選択をトグル');

key.setViewKey('U', function (ev, arg) {
    ext.exec("list-closed-tabs", arg, ev);
}, 'List closed tabs', true);

key.setViewKey('e', function () {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, '最初のインプットエリアへフォーカス', true);

key.setViewKey('S', function (ev, arg) {
    if (window.loadURI) {
        loadURI("javascript:var%20b=document.body;var%20GR________bookmarklet_domain='https://www.google.com';if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.src='https://www.google.com/reader/ui/link-bookmarklet.js');void(b.appendChild(z));}else{}");
    }
}, 'google reader share');

key.setViewKey('!', function (ev, arg) {
    shell.input();
}, 'Command system');

key.setViewKey('b', function (ev, arg) {
    BarTap.putOnTap(gBrowser.mCurrentTab, gBrowser);
}, 'bartab put on tab');

key.setViewKey('R', function () {
    BrowserReloadSkipCache();
}, '更新(キャッシュを無視)');

key.setViewKey('<backspace>', function () {
    BrowserBack();
}, '戻る');

key.setViewKey('S-<backspace>', function () {
    BrowserForward();
}, '進む');

key.setViewKey('q', function (ev, arg) {
    ext.exec("query-then-engine", arg, ev);
}, 'enter search word and then select engine', true);

key.setViewKey('/', function () {
    command.iSearchForward();
}, 'インクリメンタル検索', true);

key.setViewKey('?', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs ライクなインクリメンタル検索', true);

key.setViewKey('a', function (ev, arg) {
    allTabs.open();
}, 'alltabs.open');

key.setViewKey('<left>', function (ev) {
    goDoCommand("cmd_scrollPageUp");
}, '一画面分スクロールアップ');

key.setViewKey('<right>', function (ev) {
    goDoCommand("cmd_scrollPageDown");
}, '一画面スクロールダウン');

key.setViewKey([['<prior>'], ['<next>']], function (ev, arg) {
    return;
}, 'ignore');

key.setViewKey(':', function (ev, arg) {
    return !document.getElementById("keysnail-prompt").hidden &&
        document.getElementById("keysnail-prompt-textbox").focus();
}, 'KeySnail のプロンプトへフォーカス', true);

key.setViewKey('H', function (ev, arg) {
    ext.exec("open-hatebu-comment", arg, ev);
}, 'hatebu', true);

key.setViewKey('l', function (ev) {
    command.focusToById("urlbar");
}, 'ロケーションバーへフォーカス', true);

key.setViewKey('0', function (ev) {
    BrowserCloseTabOrWindow();
}, 'タブ / ウィンドウを閉じる');

key.setViewKey('C', function (ev, arg) {
    ext.exec("linksnail", arg, ev);
}, 'LinkSnail', true);

key.setViewKey('C-<backspace>', function (ev, arg) {
    ext.exec("list-tab-history", arg, ev);
}, 'List tab history', true);

key.setViewKey('I', function (ev, arg) {
    ext.exec("instapaper-post-page-with-comment", arg, ev);
}, 'post page and comment', true);

key.setEditKey('C-<tab>', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, '次のテキストエリアへフォーカス');

key.setViewKey('T', function (ev, arg) {
    ext.exec('mstranslator-open-prompt', arg, ev);
}, 'MSTranslator - Open prompt', true);
