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

//userscript.addLoadPath(".");
//userscript.require("verticaltab.js");

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

util.setPrefs(
    {
        "browser.tabs.loadDivertedInBackground": true,
        "dom.disable_window_open_feature.location": false,
        "dom.max_script_run_time": 30,
        "browser.bookmarks.max_backups":0,
        "browser.urlbar.autocomplete.enabled":false,
        "browser.cache.memory.capacity":16384,
        "browser.sessionhistory.max_total_viewers":8,
        "browser.download.manager.closeWhenDone":true,
        "browser.download.useDownloadDir":false,
        "browser.tabs.closeWindowWithLastTab":false,
        "network.dns.disableIPv6":true,
        "browser.urlbar.trimURLs":false,
        "browser.fullscreen.autohide":false,
        "keyword.URL":"http://www.bing.com/search?q=",
    }
);

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



//sitelocal
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
    ['l', function (ev, arg) {window.content.location.href = "http://www.feedly.com/home#latest";}],
    ['r', null],
    ['x', function (ev, arg) {ev.target.dispatchEvent(key.stringToKeyEvent("g", true));}],
    [['t', 'p'], function (ev, arg) {ev.target.dispatchEvent(key.stringToKeyEvent("t", true));}],
    [['t', 'w'], function (ev, arg) {ext.exec("twitter-client-tweet", arg, ev);}],
];

/////////////////////////////////////////
//nicovideo用
local["http://(www|tw|es|de|)\.nicovideo\.jp\/watch/*"] = [
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
    ["<left>", function (ev, arg) {window.content.location.href = "http://www.tumblr.com/dashboard"; }],
    //        ["<right>", null],
    ["J", function (ev, arg) {
        if (window.loadURI) {
            loadURI("javascript:(function(){b=20;s=100;t=document.getElementById('next_page_link').href.split('/')[5];max=t.substr(0,t.length-5);min=max-s;i=Math.floor(Math.random()*(max-min)+min);u=(i<b)?'http://www.tumblr.com/dashboard':'http://www.tumblr.com/dashboard/2/'+i+'00000';window.content.location.href=u;}())");
        }
    }],
];

///////////////////////////////////
//tanythhing用
plugins.options["tanything_opt.keymap"] = {
    "\\"     : "prompt-cancel",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "o"     : "localOpen",
    ":"     : "localClose",
    "L"     : "localMovetoend"
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

/////////////////////////////////////
// google itranslate
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
//
ext.add("fullscreen-page",function (ev) {
    getBrowser().selectedTab = getBrowser().addTab("http://home.tiscali.nl/annejan/swf/timeline.swf");
    BrowserFullScreen();
}, "fullscreen page");

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

// //////////////////////////
// //プラグイン一括アップデート
// ext.add("check-for-plugins-update", function () {
//     [p for (p in plugins.context)].forEach(function (p) { try { userscript.updatePlugin(p); } catch(e) {} });
// }, "Check for all plugin's update");

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
    if (MultipleTabService != undefined) display.echoStatusBar("true");
},'if mth exist');

////////////////////////
// instapaper
ext.add("instapaper-add-this-page-and-close",function(){
    var url = window.content.location.href;
    var title = window.content.document.title;
    var tab = gBrowser.selectedTab;
    var username = "8slashes+instapaper@gmail.com";
    var password = "";
    var passwordManager = (Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager));
    var logins = passwordManager.findLogins({}, "http://www.instapaper.com", "", null);
    for (var i = 0; i < logins.length; i++) {
        if (logins[i].username == username) {
            password = logins[i].password;
            break;
        }
    }
    var comment = "";
    // prompt.read("Instapaper comment:",function(cm){
        // if(cm){comment = cm;}
    // });
    display.echoStatusBar("Instapaper: adding " + url + "...");
    util.httpPost("https://www.instapaper.com/api/add",
                  {"username" : encodeURIComponent(username),
                   "password" : password,
                   "url" : encodeURIComponent(url),
                   "title" : encodeURIComponent(title),},
                  function (xhr) {
                      display.echoStatusBar(xhr.status);
                      if (xhr.readyState == 4 && xhr.status == 201) {
                          // var title = decodeURIComponent(xhr.getResponseHeader("X-Instapaper-Title")); //超文字化けする
                          try {
                              Components.classes['@mozilla.org/alerts-service;1'].
                                  getService(Components.interfaces.nsIAlertsService).
                                  showAlertNotification(null, "Instapaper", "Page " + title + " added successfully", false, '', null);
                          } catch(e) {
                              // prevents runtime error on platforms that don't implement nsIAlertsService
                          }
                          display.echoStatusBar("Instapaper: adding " + url + "...done.");
                          gBrowser.removeTab(tab);
                      } else{
                          display.echoStatusBar("Instapaper: Something wrong has happended!");
                          if (window.loadURI) {
                              loadURI("javascript:function%20iprl5(){var%20d=document,z=d.createElement('scr'+'ipt'),b=d.body,l=d.location;try{if(!b)throw(0);d.title='(Saving...)%20'+d.title;z.setAttribute('src',l.protocol+'//www.instapaper.com/j/mt8YO6Cuosmf?u='+encodeURIComponent(l.href)+'&t='+(new%20Date().getTime()));b.appendChild(z);}catch(e){alert('Please%20wait%20until%20the%20page%20has%20loaded.');}}iprl5();void(0)");
                          }
                      }
                  });
},'instapaper add page and close tab when done without error.');

////////////////////////
//検索
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

///////////////////////
//diggler
ext.add("dig-url", function () {
    var url = window.content.location.href;
    var nsurl = [];
    var pname = "";
    var ssurl = [];
    var durl = [];
    nsurl = url.split("#");
    var pname = nsurl[1];
    ssurl = nsurl[0].split("/");
    durl[0] = ssurl[0] + "//" + ssurl[2];
    ssurl.splice(0,3);
    for (var i = 0; i < ssurl.length; i++){
        var durlsaved = durl[0];
        durl.unshift(durlsaved + "/" + ssurl[i]);
    };
    if (pname) {
        var durlfull = durl[0] + "#"+ pname;
        durl.unshift(durlfull);
    };
    prompt.selector({ message : "dig " + url,
                      collection : durl,
                      callback : function (i) { window.content.location.href = durl[i]; },
                    });
},"keysnail diggler ");

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

///////////////////////////////////////
//
ext.add("focus-on-content", function(){
    document.getElementById("searchbar").focus();
    document.commandDispatcher.advanceFocus();
    document.commandDispatcher.advanceFocus();
}, "forcus on content");

ext.add("_focus-on-content", function(){
    gBrowser.focus();
    _content.focus();
}, "focus on content");

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
}, "close and focus to next tab")
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "<delete>";
key.helpKey              = "C-h";
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
    command.closeFindBar();
    if (util.isCaretEnabled()) {
        command.resetMark(aEvent);
    } else {
        goDoCommand("cmd_selectNone");
    }
    key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
});
hook.addToHook('KeyBoardQuit', function (aEvent) {
    ext.exec("hide-sidebar");
    let(elem = document.commandDispatcher.focusedElement) elem && elem.blur();
    gBrowser.focus();
    content.focus();
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

key.setViewKey('', function (ev, arg) {
    var n = gBrowser.mCurrentTab._tPos;
    BrowserCloseTabOrWindow();
    gBrowser.selectedTab = gBrowser.mTabs[n];
}, '閉じて次のタブ');

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

key.setViewKey('T', function (ev, arg) {
    ext.exec("google-itranslate", arg, ev);
}, 'google itranslate', true);

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

key.setViewKey('D', function (ev, arg) {
    ext.exec("dig-url", arg, ev);
}, 'keysnail diggler ', true);

key.setViewKey('/', function () {
    command.iSearchForward();
}, 'インクリメンタル検索', true);

key.setViewKey('?', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs ライクなインクリメンタル検索', true);

key.setViewKey('a', function (ev, arg) {
    allTabs.open();
}, 'alltabs.open');

key.setViewKey('C', function (ev, arg) {
    ext.exec("copy-url", arg, ev);
}, '選択タブと現在のタブを閉じる', true);

key.setViewKey('I', function (ev, arg) {
    ext.exec("instapaper-add-this-page-and-close", arg, ev);
}, 'instapaper add page', true);

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

key.setEditKey('C-<tab>', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, '次のテキストエリアへフォーカス');

key.setViewKey('0', function (ev) {
    BrowserCloseTabOrWindow();
}, 'タブ / ウィンドウを閉じる');
