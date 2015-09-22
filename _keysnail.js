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

// Original is at https://gist.github.com/Griever/62715
ext.add("open-bookmarks-in-new-tab", function(){
  // this makes all clicks open in new tab.
  // return;
  // test if firefox 12
  if (12 > parseFloat(Cc['@mozilla.org/xre/app-info;1'].
                      getService(Ci.nsIXULAppInfo).version)) {
    return;
  }


  // I cannot fully understand what happen but keysnail emit error
  // *after* initialization of firefox finished successfully and I assured
  // that new tabs open as I expect when clicking bookmarks, for example,
  // when I try to tweet using the twitter keysnail plugin, with error
  // message saying "whereToOpenLink is undefined.".

  // if no modifier key pressed open in new tab.
  try {
    var whereToOpenLink_org = new String(window.whereToOpenLink.toString());
    window.whereToOpenLink = eval(
      "(" +
        whereToOpenLink_org.replace(
          '{',
          '{ if(!e || ' +
            '(!e.ctrlKey && !e.shiftKey && !e.metaKey && ' +
            '!(e.altKKey && !ignoreAlt))) ' +
            'return "tab";'
        ) +
        ")"
    );
  } catch (e if e instanceof TypeError) {
    return;
  }

  // This is useless because this makes reload open new tab.
  // window.whereToOpenLink = function(e, ib, ia){ return "tab"; };

}, "open in new tab");
// ext.exec("open-bookmarks-in-new-tab");

//////////////////////////////////////
//// sitelocalkeymap
var local = {};
plugins.options["site_local_keymap.local_keymap"] = local;
function fake(k, i) function(){ key.feed(k, i); };
function pass(k, i) [k, fake(k, i)];
function ignore(k, i) [k, null];

// ext.add("ext-name", function () {}, "ext description");
// style.register("");
// local["^http://"] = [['a', function(ev, arg){}],];

///////////////////////////////////////////
//// firefox
// style.register("#bookmarksPanel > hbox,#history-panel > hbox {" +
//                "display: none !important;}" +
//                " //#urlbar-container{max-width: 500px !important;}");
// style.register(                 //not work
//         <><![CDATA[
//             input,textarea {
//                 font-family: monospace !important;
//             }
//         ]]></>.toString()
// );
style.register(
  '@-moz-document ' +
    'url-prefix(\"http://www.tumblr.com/\"), ' +
    'url-prefix(\"https://www.tumblr.com/\") ' +
    '{ #pagination {' +
    'position: fixed;' +
    'bottom: 50px;' +
    'right: 100px;' +
    'padding-right: 0px !important' +
    '} }' , style.XHTML);

///////////////////////////////////
//search engine
plugins.options["search-url-list"] = [
  ["DuckDuckGo", "https://duckduckgo.com/?q=%q"],
  ["Bing", "http://bing.com/search?q=%q"],
  ["Google", "http://google.com/search?q=%q"],
  ["yatwitter search", "http://yats-data.com/yats/search?query=%q"],
  ["Yahoo Realtime",
   "http://realtime.search.yahoo.co.jp/search?rkf=1&ei=UTF-8&p=%q"],
  ["Twitter Search", "http://twitter.com/search?q=%q&lang=all"],
  ["Tospy", "http://topsy.com/s?allow_lang=ja&q=%q"],
  ["2ch syoboi.jp", "http://ff2ch.syoboi.jp/?q=%q"],
  ["2ch-ranking","http://2ch-ranking.net/search.php?q=%q&imp=and&order=time"],
  ["2ch ttsearch", "http://ttsearch.net/s2.cgi?k=%q&o=r"],
  ["I\'m feelig lucky!", "http://www.google.co.jp/search?q=%q&btnI=kudos"],
  ["Uncyclopedia", "http://ja.uncyclopedia.info/wiki/%q"],
  ["Wikipedia", "http://ja.wikipedia.org/wiki/%q"],
  ["nicovideo.jp", "http://www.nicovideo.jp/search/%q"],
  ["alc", "http://eow.alc.co.jp/%q/UTF-8/"],
  ["google map",
   "http://maps.google.co.jp/maps?hl=ja&q=%q&um=1&ie=UTF-8&sa=N&tab=wl"],
  ["Weblio", "http://www.weblio.jp/content_find?query=%q"],
  ["ShoutCast", "http://www.shoutcast.com/Internet-Radio/%q"],
  ["Delicious 10sr", "http://delicious.com/10sr/%q"],
  ["Open raw", "%r"]
];

plugins.options["my-keysnail-bookmarks"] = [
  "twitter.com"
];

// sitelocal keymap

//////////////////////////////////////////
// 2ch chaika

// change chaika port every time firefox starts
util.setIntPref("extensions.chaika.server_port.firefox",
                8800 + Math.floor(Math.random() * 30));

local["^http://127.0.0.1:88"] = [
  ['k', function(ev, arg){
    curl = window.content.location.href;
    kurl = curl.replace(/http:.*thread\/(.*\/).*/, "chaika://post/$1");
    window.content.location.href = kurl;
  }
  ]
];

local["^http://w2.p2.2ch.net/p2/read.php"] = [
  ['k', function(ev, arg){
    var url = window.content.location.href;
    var pt = /host=(.*?)&bbs=(.*?)&key=(.*?)&ls=/ ;
    var result = url.match(pt);
    var k = format("chaika://post/http://%s/test/read.cgi/%s/%s/",
                   result[1], result[2], result[3]);
    window.content.location.href = k;
  }
  ]
];

/////////////////////////////////////////
// feedly
local["^https?://(www\.|cloud\.|)feedly\.com/"] = [
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
  // ['x', function(ev, arg){
  //     ev.target.dispatchEvent(key.stringToKeyEvent("g", true));
  // }],
  // ['l', function(ev, arg){
  //     var host = window.content.location.host;
  //     if (host === "cloud.feedly.com" || host === "feedly.com") {
  //         window.content.location.href = "http://" + host + "/#latest";
  //     } else if (host === "www.feedly.com") {
  //         window.content.location.href = "http://" + host + "/home#latest";
  //     }
  // }],
  ['a', null],
  [['t', 'p'], function(ev, arg){
    ev.target.dispatchEvent(key.stringToKeyEvent("t", true));
  }],
  [['t', 'w'], function(ev, arg){
    ext.exec("twitter-client-tweet", arg, ev);
  }]
];

/////////////////////////////////////////
//nicovideo
// http://dic.nicovideo.jp/id/4858151
local["^http://(www|tw|es|de|)\.nicovideo\.jp\/(watch|playlist)"] = [
    // ["i", function(ev, arg){ ext.exec("nicoinfo", arg); }],
  ["SPC", function(ev, arg){ ext.exec("nicopause", arg); }],
  ["f", function(ev, arg){ ext.exec("nicosize", arg); }],
  ["F", function(ev, arg){ ext.exec("nicosize", arg); }],
    // ["o", function(ev, arg){ ext.exec("nicommentvisible", arg); }],
    // ["m", function(ev, arg){ ext.exec("nicomute", arg); }],
    // [".", function(ev, arg){ ext.exec("nicovolumeIncrement", arg); }],
    // [",", function(ev, arg){ ext.exec("nicovolumeDecrement", arg); }],
    // ['f', function(ev, arg){
    //     curl = window.content.location.href;
    //     kurl = curl.replace(/nicovideo.jp/, "nicovideofire.jp");
    //     window.content.location.href = kurl;
    // }]
];

/////////////////////////////////////////
// tumblr/dashboard
local["^http://www.tumblr.com/dashboard"] = [
  // ["C-<left>", function(ev, arg){
  //     gBrowser.mTabContainer.advanceSelectedTab(-1, true);
  // }],
  // ["C-<right>", function(ev, arg){
  //     gBrowser.mTabContainer.advanceSelectedTab(1, true);
  // }],
  ["<left>", function(ev, arg){
    window.content.location.href = "http://www.tumblr.com/dashboard";
  }],
  ["<right>", null],
  ["J", function(ev, arg){
    if (window.loadURI) {
      loadURI("javascript:(function(){b=20;s=100;t=document.getElementById('next_page_link').href.split('/')[5];max=t.substr(0,t.length-5);min=max-s;i=Math.floor(Math.random()*(max-min)+min);u=(i<b)?'http://www.tumblr.com/dashboard':'http://www.tumblr.com/dashboard/2/'+i+'00000';window.content.location.href=u;}())");
    }
  }]
];

///////////////////////////////////////////
// plugin option

plugins.options["builtin_commands_ext.ext_list"] = [
  "focus-to-prompt",
  "open-url-from-clipboard",
  "restart-firefox"
];

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
plugins.options["twitter_client.jmp_key"] =
  "R_c51f889a77cb4b4e993ed868f65083f5";
plugins.options["twitter_client.use_jmp"] = true;

////////////////////////////////////////////
// my exts and functions

var autoSaveTabList = (function(){
  const PREF_PREFIX = "extensions.keysnail.plugins.autosavetablist.";
  const PREF_DSTDIR = "dstdir";
  const PREF_ENABLED = "enabled";
  // use plugin option to set
  // in sec
  var default_timer_interval = 60 * 5;
  // "/" for unix system
  const DIR_DELIM = userscript.directoryDelimitter;

  var __timer = null;

  function selectDirectory(title){
    // open dialog and return nsILocalFile object
    // https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Interface/nsILocalFile
    // https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Interface/nsIFilePicker
    // this function blocks.
    var nsIFilePicker = Components.interfaces.nsIFilePicker;
    var fp = Components.classes["@mozilla.org/filepicker;1"].
        createInstance(nsIFilePicker);
    fp.init(window, title, nsIFilePicker.modeGetFolder);

    // block
    var res = fp.show();
    if (res !== nsIFilePicker.returnOK) {
      return null;
    }
    return fp.file;
  }

  function selectFile(title){
    // open dialog and return nsILocalFile object
    // https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Interface/nsILocalFile
    // https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Interface/nsIFilePicker
    // this function blocks.
    var nsIFilePicker = Components.interfaces.nsIFilePicker;
    var fp = Components.classes["@mozilla.org/filepicker;1"].
        createInstance(nsIFilePicker);
    fp.init(window, title, nsIFilePicker.modeOpen);

    // block
    var res = fp.show();
    if (res !== nsIFilePicker.returnOK) {
      return null;
    }
    return fp.file;
  }

  function setup(){
    // set destination directory
    var dstdir = selectDirectory("Select Directory to Save Tab List");
    if (! dstdir) { return; }
    if (! dstdir.isWritable()) {
      display.notify("Directory is not writable");
      return;
    }

    util.setUnicharPref(PREF_PREFIX + PREF_DSTDIR, dstdir.path);
  }

  function genFileName(){
    function formatCurrent(){
      var d = new Date();
      function pad(n){
        return n < 10 ? '0' + n.toString() : n.toString()
      }
      return [
        d.getFullYear().toString(),
        pad(d.getMonth() + 1),
        pad(d.getDate()),
        "-",
        pad(d.getHours()),
        pad(d.getMinutes()),
        pad(d.getSeconds())
      ].join("");
    }

    return "tablist." + formatCurrent() + ".lst";
  }

  function getTabList(){
    // returns list of urls of current tabs.
    return [(function(){
      var browser = tab.linkedBrowser;
      var win     = browser.contentWindow;
      // var title = tab.label;
      var url   = win.location.href;
      return url;
    })() for each (tab in Array.slice(gBrowser.mTabContainer.childNodes))];
    // Array.slice is required?
  }

  function saveCurrentList(){
    var dstdir = util.getUnicharPref(PREF_PREFIX + PREF_DSTDIR);
    if (! dstdir) {
      display.showPopup("AutoSaveTabList",
                        "Dest dir is not set yet. Run astl-setup first");
      return;
    }

    var filename = genFileName();

    util.writeTextFile(
      getTabList().join("\n") + "\n",
      dstdir + userscript.directoryDelimiter + filename
    );
    display.showPopup("AutoSaveTabList",
                      "Tab List saved: " + filename);
  }

  function openFromFile(){
    var file = selectFile();
    if (file) {
      openFromLFSplittedString(util.readTextFile(file.path));
    }
  }

  function openFromClipboard(){
    openFromLFSplittedString(command.getClipboardText());
  }

  function openFromLFSplittedString(str){
    var urls = str.split("\n");
    for (var i = 0; i < urls.length; i++) {
      if (urls[i].match(/^http/)) {
        window.openUILinkIn(urls[i], "tab");
      }
    }
  }

  function enableTimer(sec){
    sec = sec || default_timer_interval;
    // __timer = window.setInterval(saveCurrentList, sec * 1000);
    display.showPopup("AutoSaveTabList",
                      "Auto save enabled for every " + sec.toString() + " sec");
  }

  function disableTimer(){
    if (__timer) {
      window.clearInterval(__timer);
      __timer = null;
      display.showPopup("AutoSaveTabList",
                        "Auto save disabled");
    } else {
      display.showPopup("AutoSaveTabList",
                        "Auto save timer is not set yet !");
    }
  }

  return {
    selectDirectory: selectDirectory,
    setup: setup,
    getTabList: getTabList,
    saveCurrentList: saveCurrentList,
    openFromClipboard: openFromClipboard,
    openFromFile: openFromFile,
    enableTimer: enableTimer,
    disableTimer: disableTimer
  };
})();

ext.add("astl-setup", autoSaveTabList.setup, "Auto save tab list - Setup");
ext.add("astl-open-from-clipboard", autoSaveTabList.openFromClipboard, "Auto save tab list - Open tabs from clipboard");
ext.add("astl-open-from-file", autoSaveTabList.openFromFile, "Auto save tab list - Open tabs from local file");
ext.add("astl-save-current", autoSaveTabList.saveCurrentList,
        "Auto save tab list - Save current list");
ext.add("astl-enable-timer", autoSaveTabList.enableTimer, "Auto save tab list - Enable periodic timer");
ext.add("astl-disable-timer", autoSaveTabList.disableTimer, "Auto save tab list - disable periodic timer");


var echoTabInfo = (function(){
  var currenttab;
  function _display(msg){
    display.prettyPrint(msg, {
      timeout: 1500,
      style: {
        "font-size": "18px",
        "font-family": "monospace"
      }
    });
  }

  function __display(msg){
    display.echoStatusBar(msg);
  }

  function echo(){
    var newtab = getBrowser().mCurrentTab;
    if (currenttab === newtab) {
      return;
    }

    currenttab = newtab;
    var len = getBrowser().tabs.length;
    var idx = currenttab._tPos;
    var title = window.content.document.title;
    var url = window.content.location.href;

    var doc = content ? content.document : document;
    var dBody = doc.body;
    if (dBody && !util.isFrameSetWindow(content)) {
      // if page has been prepared
      _display(
        "[" +
          (idx + 1).toString() + "/" +
          len.toString() + "] " +
          title + " <" +
          decodeURIComponent(url) + ">"
      );
    }
  }

  return {
    echo: echo
  };
})();

ext.add("open-remote-init-file", function(ev, arg){
  const URL = "https://raw.github.com/10sr/dotfiles/master/_keysnail.js";
  window.openUILinkIn(URL, "tab");
}, "Open remote initialization file");

var updateInitFile = (function(){
  const URL = "https://raw.github.com/10sr/dotfiles/master/_keysnail.js";

  // content/modules/userscript.js
  // copy file from aFile
  function placeFile(aFile, force) {
    var dstdir = util.getUnicharPref("extensions.keysnail.userscript.location");
    if (dstdir === "") {
      throw util.getLocaleString("failedToInstallFile", [aFile.leafName]) + " :: " + x;
    }

    try
    {
      // calc dir from path
      let destinationDir  = util.openFile(dstdir);
      let destinationFile = util.openFile(dstdir);

      destinationFile.append(aFile.leafName);

      if (destinationFile.exists())
      {
        if (util.hashFile(aFile) === util.hashFile(destinationFile))
        {
          // no need to install this file
          return destinationFile;
        }

        let confirmed = force ||
            util.confirm(
              util.getLocaleString("overWriteConfirmationTitle"),
              util.getLocaleString("overWriteConfirmation",
                                   [destinationFile.path])
            );

        if (!confirmed) {
          throw util.getLocaleString("canceledByUser");
        }
      }

      aFile.moveTo(destinationDir, "");

      return destinationFile;
    }
    catch (x)
    {
      throw util.getLocaleString("failedToInstallFile",
                                 [aFile.leafName]) +
        " :: " + x;
    }
  }

  function updateFile() {
    util.httpGet(URL, false, function (req) {
      if (req.status !== 200) {
        util.message(req.responseText);
      }

      try {
        let name = util.getLeafNameFromURL(URL);
        let file = userscript.writeTextTmp(name, req.responseText);
        let installed = placeFile(file);
        util.message(installed.path + " installed");
        display.showPopup("update-init-file",
                          installed.path + " installed")
      } catch (x) {
        util.message(
          "An error occured while installing required scripts :: " +
            x.message
        );
        display.showPopup(
          "update-init-file",
          "An error occured while installing required scripts :: " +
            x.message
        );
      }
    });
  }

  return {
    updateFile: updateFile
  };

})();

ext.add("update-init-file", updateInitFile.updateFile, "update init file");

var importExportBookmarks = (function(){
  function getOrganizer(){
    // [How to call for Firefox bookmark dialog? - Stack Overflow]
    // (http://stackoverflow.com/questions/9158187/how-to-call-for-firefox-bookmark-dialog)
    Components.utils.import("resource://gre/modules/Services.jsm");

    var organizer = Services.wm.getMostRecentWindow("Places:Organizer");
    if (!organizer) {
      // No currently open places window,
      // so open one with the specified mode.
      openDialog("chrome://browser/content/places/places.xul",
                 "",
                 "chrome,toolbar=yes,dialog=no,resizable",
                 "AllBookmarks");
      return null;
    } else {
      return organizer;
    }
  }
  ext.add("export-bookmarks", function(ev, arg){
    var organizer = getOrganizer();
    if (organizer) {
      organizer.PlacesOrganizer.exportBookmarks();
    }
  }, "export bookmarks");
  ext.add("import-bookmarks", function(ev, arg){
    var organizer = getOrganizer();
    if (organizer) {
      organizer.PlacesOrganizer.importBookmarks();
    }
  }, "import bookmarks");

  return {
    getOrganizer: getOrganizer
  };
})();

ext.add("my-index-html", function(ev, arg){
  homepath = util.getEnv("HOME");
  file = ".index.html";
  if (homepath) {
    path = "file://" + homepath + "/" + file;
    window.openUILinkIn(path, "tab");
  }
}, "open my index.html");

ext.add("strong-fullscreen", function(){
  var elemids = [
    "navigator-toolbox"
  ];
  BrowserFullScreen();
  var isfullscreen = window.fullScreen;

  for(var i = 0; i < elemids.length; i++){
    var elem = document.getElementById(elemids[i]);
    if(elem){
      if(isfullscreen){
        elem.style.display = "none";
      }else{
        elem.style.display = null;
      }
    }
  }

  var tabs = document.getElementById("verticaltabs-box");
  if(tabs){
    var pref_key = "extensions.verticaltabs.width";
    var pref_key_bak = pref_key + "_bak";
    if(isfullscreen){
      var width_orig = util.getIntPref(pref_key);
      util.setIntPref(pref_key_bak, width_orig);
      util.setIntPref(pref_key, 0);
      tabs.setAttribute("width", "0");
    }else if(parseInt(tabs.getAttribute("width") || "") === 0){
      var width_bak = util.getIntPref(pref_key_bak);
      util.setIntPref(pref_key, width_bak);
      tabs.setAttribute("width", width_bak.toString());
    }
  }

}, "go fullscreen with hiding toolbar and tabbar");

ext.add("bookmark-delicious", function(){
  f= 'http://www.delicious.com/save?url=' +
    encodeURIComponent(window.content.location.href) +
    '&title=' + encodeURIComponent(document.title) +
    '&notes=' + encodeURIComponent(
      '' + (window.getSelection ?
            window.getSelection() : (
              document.getSelection ?
                document.getSelection() :
                document.selection.createRange().text))) + '&v=6&';
  a = function(){
    if(! window.open(
      f + 'noui=1&jump=doclose',
      'deliciousuiv6',
      'location=1,links=0,scrollbars=0,toolbar=0,width=710,height=660')){
      location.href = f + 'jump=yes';
    }
  };
  if(/Firefox/.test(navigator.userAgent)){
    setTimeout(a,0);
  }else{
    a();
  }
}, "bookmark delicious");

ext.add('view-page-source', function(){
  window.content.location.href = "view-source:" +
    window.content.location.href;
}, 'view page source');

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
      "browser.fixup.alternate.enabled": false,
      "dom.disable_window_open_feature.location": false,
      "dom.max_script_run_time": 30,
      "extensions.chaika.bbsmenu.open_new_tab":true,
      "extensions.chaika.bbsmenu.open_single_click":false,
      "extensions.chaika.board.open_new_tab":true,
      "extensions.chaika.board.open_single_click":false,
      "extensions.foxage2ch.openThreadInTab":true,
      "extensions.saveimageinfolder.general-duplicatefilenamevalue":1,
      "extensions.saveimageinfolder.general-fileprefixvalue":
      "%yyyy%%MM%%dd%-%hh%%mm%%ss%_",
      "extensions.saveimageinfolder.usecache":true,
      "extensions.tabutils.openTabNext":1,
      "extensions.tabutils.openUrlInTab":false,
      "extensions.tabutils.styles.current":
      "{\"bold\":true,\"italic\":false,\"underline\":true,\"strikethrough\":false,\"color\":true,\"colorCode\":\"#000000\",\"bgColor\":false,\"bgColorCode\":\"#000000\",\"outline\":false,\"outlineColorCode\":\"#000000\"}",
      "extensions.tabutils.styles.unread":
      "{\"bold\":false,\"italic\":false,\"underline\":false,\"strikethrough\":false,\"color\":true,\"colorCode\":\"#CC0000\",\"bgColor\":false,\"bgColorCode\":\"undefined\",\"outline\":false,\"outlineColorCode\":\"undefined\"}",
      "extensions.tabutils.TFS_Enable":false,
      // Always select right tab on close
      "extensions.tabutils.selectOnClose":10,
      "extensions.yass.edgetype":0,
      "extensions.yass.selectedpreset":"red",
      "font.default.x-western":"sans-serif",
      "gecko.handlerService.schemes.mailto.1.name":"Gmail",
      "general.warnOnAboutConfig":false,
      "keyword.URL":"http://www.bing.com/search?q=",
      "browser.search.defaultenginename":"Bing",
      "browser.search.defaulturl":"http://www.bing.com/search?q=",
      "network.dns.disableIPv6":true,
      "refcontrol.actions":
      "@DEFAULT=@FORGE www.heartrails.com=@NORMAL www.pixiv.net=@NORMAL",
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
    'https://raw.github.com/mooz/keysnail/master/plugins/hok.ks.js',
    'https://github.com/mooz/keysnail/raw/master/plugins/builtin-commands-ext.ks.js',
    'https://raw.github.com/azu/KeySnail-Plugins/master/JSReference/js-referrence.ks.js',
    'https://raw.github.com/gongo/keysnail_plugin/master/linksnail.ks.js',
    'https://raw.githubusercontent.com/azu/keysnail-plugin/master/nicontroller.ks.js',
    'https://raw.github.com/10sr/keysnail-plugin/master/shiitake.ks.js',
    'https://raw.github.com/10sr/keysnail-plugin/master/dig-url.ks.js',
    'https://raw.github.com/10sr/keysnail-plugin/master/instapaper.ks.js',
    'https://raw.github.com/10sr/keysnail-plugin/master/pixiv_autojump.ks.js',
    'https://raw.github.com/10sr/keysnail-plugin/master/list-current-urls.ks.js',
    'https://gist.githubusercontent.com/10sr/1976942/raw/firefox-addon-manager.ks.js',
    'https://gist.githubusercontent.com/958/1450594/raw/mstranslator.ks.js'
  ];

  function inst(a){
    if(a.length == 0){
      display.showPopup("auto-install-plugins",
                        "All installation finished.");
    }else{
      var url = a.shift();
      var path = userscript.pluginDir +
          userscript.directoryDelimiter + url.match(/[^/]+$/)[0];
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
  var n = getBrowser().mCurrentTab._tPos;
  getBrowser().moveTabTo(getBrowser().mCurrentTab, 0);
  if (n != 0) {
    getBrowser().selectedTab = getBrowser().mTabs[n];
  }
}, 'put aside this page');

ext.add('send-escape', function (ev, arg) {
  ev.target.dispatchEvent(key.stringToKeyEvent("ESC", true));
}, 'escape');

ext.add("open-hatebu-comment", function (ev, arg) {
  var url = window.content.location.href.replace(/^[^/]*\/\//, "");
        window.content.location.href = "http://b.hatena.ne.jp/entry/" + url;
       }, 'hatebu');

// ext.add("focus-on-content", function(){
//    let(elem = document.commandDispatcher.focusedElement) elem && elem.blur();
//     gBrowser.focus();
//     content.focus();
// }, "forcus on content");

ext.add("hide-sidebar", function(){
  var sidebarBox = document.getElementById("sidebar-box");
  if (!sidebarBox.hidden) {
    toggleSidebar(sidebarBox.getAttribute("sidebarcommand"));
  }
}, "hide-sidebar");

ext.add("close-and-next-tab", function (ev, arg) {
  var n = getBrowser().mCurrentTab._tPos;
  getBrowser().removeCurrentTab();
  getBrowser().selectedTab = getBrowser().mTabs[n];
}, "close and focus to next tab");

//////////////////////////////////////
//
ext.add("restart-firefox-add-menu", function(){
  const XUL_NS =
        "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";

  var cmdelm = document.createElementNS(XUL_NS, "command");
  cmdelm.setAttribute("id", "my_cmd_restartFirefoxKs");
  cmdelm.setAttribute("oncommand", "ext.exec('restart-firefox');");
  var commandset = document.getElementById("mainCommandSet");
  // menu.insertBefore(elm, menu.getElementById("menu_FileQuitItem"));
  commandset.appendChild(cmdelm);

  var menuelm = document.createElementNS(XUL_NS, "menuitem");
  menuelm.setAttribute("label", "Restart Firefox");
  menuelm.setAttribute("id", "my_menu_restartFirefoxKs");
  menuelm.setAttribute("command", "my_cmd_restartFirefoxKs");
  var menu = document.getElementById("menu_FilePopup");
  // menu.insertBefore(elm, menu.getElementById("menu_FileQuitItem"));
  menu.appendChild(menuelm);
}, "add restart firefox menu");

/////////////////////////////////////////
// feed url

ext.add("feed-add-to-feedly", function(){
  var url = window.content.location.href;
  window.content.location.href =
    "http://cloud.feedly.com/#subscription%2Ffeed%2F" +
    encodeURIComponent(url);
}, "Add current feed to feedly");

var feedUtils = (function(){
  var feedhandler = "http://cloud.feedly.com/#subscription%2Ffeed%2F%s";

  function getFeeds(){
    const doc = content.document;

    var feeds = [[e.getAttribute("title"), e.getAttribute("href")]
                 for ([, e] in Iterator(doc.querySelectorAll([
                   'link[type="application/rss+xml"]',
                   'link[type="application/atom+xml"]'
                 ])))];
    var uh = window.content.location.href.replace(/(.*?\/\/[^/]*)(\/.*)?/,
  "$1");
                 for (i = 0; i < feeds.length; i++)
                   if ( feeds[i][1].substr(0,1) == "/" ) feeds[i][1] = uh + feeds[i][1];
                 // feeds.unshift([window.content.document.title,
                 //                window.content.location.href]);

                 return feeds;
                };

ext.add("copy-feed-url", function () {
  var feeds = getFeeds();
  if (! feeds.length) {
    display.echoStatusBar("No feed found.");
    return;
  }
  prompt.selector({
    message    : "Select Feed",
    collection : feeds,
    callback   : function (i) {
      if (i >= 0) {
        command.setClipboardText(feeds[i][1]);
      }
    }
  });
}, "Copy url or feed url of current page");

ext.add("open-feed", function () {
  var feeds = getFeeds();
  if (! feeds.length) {
    display.echoStatusBar("No feed found.");
    return;
  }
  prompt.selector({
    message    : "Select Feed",
    collection : feeds,
    callback   : function (i) {
      if (i < 0) {
        return;
      }

      var feedurl = feeds[i][1];
      if (feedhandler) {
        window.openUILinkIn(
          feedhandler.replace("%s", feedurl),
          "tab"
        );
      } else {
        window.openUILinkIn(feedurl, "tab");
      }
    }
  });
}, "Copy url or feed url of current page");

return {
  getFeeds: getFeeds
};

})();

///////////////////////////////////////
// keysnail z menu

ext.add("keysnail-setting-dialog", function(){
  KeySnail.openPreference();
}, "keysnail setting dialog");

ext.add("keysnail-plugin-manager", function(){
  userscript.openPluginManager();
}, "keysnail plugin manager");

ext.add("firefox-open-addon-manager", function(){
  BrowserOpenAddonsMgr();
}, "firefox addon manager");

ext.add("keysnail-reload-init-file", function(){
  userscript.reload();
}, "keysnail reload init file");

ext.add("keysnail-z-menu",function(){
  var list = [["keysnail-setting-dialog"],
              ["keysnail-plugin-manager"],
              ["firefox-open-addon-manager"],
              ["keysnail-reload-init-file"],
              // ["check-for-plugins-update"],
              ["restart-firefox"]
             ];
  prompt.selector(
    {
      message    : "open setting dialog",
      collection : list,
      callback   : function (i) {
        ext.exec(list[i][0]);
      }
    });
},"open keysnail z menu");

///////////////////////////////////
// search web
ext.add("query-then-engine", function () {
  prompt.reader({
    message : "Search Word?:",
    group : "query_word",
    // completer : completer.matcher.header(share.friendsCache || []),
    initialInput : content.document.getSelection() || "",
    callback : function (q) {
      if (q) {
        prompt.selector({
          message : "search \"" + q + "\" with?",
          collection : plugins.options["search-url-list"],
          width : [20,80],
          callback : function (i) {
            getBrowser().selectedTab =
              getBrowser().addTab(
                plugins.options["search-url-list"][i][1].
                  replace("%r",q).replace(
                    "%q",encodeURIComponent(q)
                  )
              )
            ;
          }
        });
      };
    }
  });
}, "enter search word and then select engine");

/////////////////////////////////////
// closed tab list
ext.add("list-closed-tabs", function () {
  const fav = "chrome://mozapps/skin/places/defaultFavicon.png";
  var ss   = Cc["@mozilla.org/browser/sessionstore;1"].getService(
    Ci.nsISessionStore
  );
  var json = Cc["@mozilla.org/dom/json;1"].createInstance(Ci.nsIJSON);
  var closedTabs = [[tab.image || fav, tab.title, tab.url]
                    for each (tab in json.decode(ss.getClosedTabData(window)))
                    ];

  if (!closedTabs.length)
    return void display.echoStatusBar("No recently closed tab.", 2000);

  prompt.selector(
    {
      message    : "select tab to undo:",
      collection : closedTabs,
      flags      : [ICON | IGNORE, 0, 0],
      callback   : function (i) { if (i >= 0) window.undoCloseTab(i); }
    });
}, "List closed tabs");

///////////////////////////////
// http://malblue.tumblr.com/post/349001250/tips-japanese-keysnail-github
ext.add("list-tab-history", function () {
  const fav = "chrome://mozapps/skin/places/defaultFavicon.png";
  var tabHistory = [];
  var sessionHistory = getBrowser().webNavigation.sessionHistory;
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
      callback    : function(i) {
        if (i >= 0)
          getBrowser().webNavigation.gotoIndex(tabHistory[i][3]);
      }
    });
},  'List tab history');

//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "ESC";
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


hook.setHook('KeyBoardQuit', function (aEvent) {
    // ext.exec("hide-sidebar");
  let(elem = document.commandDispatcher.focusedElement) {elem && elem.blur()};
    getBrowser().focus();
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
        share.pluginUpdater = ks.getPluginUpdater(
        share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});

hook.setHook('LocationChange', function (aNsURI) {
    echoTabInfo.echo();
});

// ============================= Key bindings ============================== //

key.setGlobalKey('C-<up>', function () {
  var browser = getBrowser();
  if (browser.mCurrentTab.previousSibling) {
    browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos - 1);
  } else {
    browser.moveTabTo(browser.mCurrentTab,
                      browser.mTabContainer.childNodes.length - 1);
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

key.setGlobalKey('<delete>', function (ev, arg) {
  let (elem = document.commandDispatcher.focusedElement) {elem && elem.blur()};
  getBrowser().focus();
  content.focus();
}, 'コンテンツへフォーカス', true);

key.setGlobalKey('<f11>', function (ev, arg) {
  ext.exec("strong-fullscreen", arg, ev);
}, 'go fullscreen with hiding toolbar and tabbar', true);

key.setGlobalKey('<end>', function (ev) {
  getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setGlobalKey('<home>', function (ev) {
  getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setGlobalKey('<next>', function (ev) {
  let browser = getBrowser();
  if (browser.mCurrentTab.nextSibling) {
    browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos + 1);
  } else {
    browser.moveTabTo(browser.mCurrentTab, 0);
  }
}, '選択中のタブを右へ');

key.setGlobalKey('<prior>', function (ev) {
  let browser = getBrowser();
  if (browser.mCurrentTab.previousSibling) {
    browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos - 1);
  } else {
    browser.moveTabTo(browser.mCurrentTab, browser.mTabContainer.childNodes.length - 1);
  }
}, '選択中のタブを左へ');

key.setViewKey('0', function (ev) {
  BrowserCloseTabOrWindow();
}, 'タブ / ウィンドウを閉じる');

key.setViewKey('l', function (ev) {
  getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setViewKey('h', function (ev) {
  getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setViewKey('o', function (ev, arg) {
  ext.exec("hok-start-foreground-mode", arg, ev);
}, 'Start Hit a Hint foreground mode', true);

key.setViewKey('c', function (ev) {
  command.interpreter();
}, 'JavaScript のコードを評価');

key.setViewKey('D', function (ev, arg) {
  ext.exec("dig-url", arg, ev);
}, 'dig url with selector', true);

key.setViewKey('x', function (aEvent, aArg) {
  ext.select(aArg, aEvent);
}, 'エクステ一覧');

key.setViewKey(['t', 'w'], function (ev, arg) {
  ext.exec("twitter-client-tweet", arg, ev);
}, 'つぶやく', true);

key.setViewKey(['t', 'p'], function (ev, arg) {
  ext.exec("twitter-client-tweet-this-page", arg, ev);
}, 'このページのタイトルと URL を使ってつぶやく', true);

key.setViewKey([['u'], ['S-SPC']], function (ev) {
  goDoCommand("cmd_scrollPageUp");
}, '一画面分スクロールアップ');

key.setViewKey('g', function () {
  goDoCommand("cmd_scrollTop");
}, 'ページ先頭へ移動');

key.setViewKey('G', function () {
  goDoCommand("cmd_scrollBottom");
}, 'ページ末尾へ移動');

key.setViewKey('r', function (aEvent) {
  BrowserReload();
}, '再読み込み');

key.setViewKey('z', function (ev, arg) {
  ext.exec("keysnail-z-menu", arg, ev);
}, 'open keysnail setting menu', true);

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

key.setViewKey([['d'], ['SPC']], function (ev) {
  goDoCommand("cmd_scrollPageDown");
}, '一画面スクロールダウン');

key.setViewKey(':', function (ev, arg) {
  return !document.getElementById("keysnail-prompt").hidden &&
    document.getElementById("keysnail-prompt-textbox").focus();
}, 'KeySnail のプロンプトへフォーカス', true);

key.setViewKey('B', function (ev) {
  var browser = getBrowser();
  if (browser.mCurrentTab.previousSibling) {
    browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos - 1);
  } else {
    browser.moveTabTo(browser.mCurrentTab,
                      browser.mTabContainer.childNodes.length - 1);
  }
}, '選択中のタブを左へ');

key.setViewKey('C', function (ev, arg) {
  ext.exec("linksnail", arg, ev);
}, 'LinkSnail', true);

key.setViewKey('C-<backspace>', function (ev, arg) {
  ext.exec("list-tab-history", arg, ev);
}, 'List tab history', true);

key.setViewKey('I', function (ev, arg) {
  ext.exec("instapaper-post-page-with-comment", arg, ev);
}, 'post page and comment', true);

key.setViewKey('T', function (ev, arg) {
  ext.exec("mstranslator-open-prompt", arg, ev);
}, 'MSTranslator - Open prompt', true);

key.setViewKey('f', function (ev, arg) {
  ext.exec('strong-fullscreen', arg, ev);
}, 'go fullscreen with hiding toolbar and tabbar', true);

key.setViewKey('F', function (ev) {
  var browser = getBrowser();
  if (browser.mCurrentTab.nextSibling) {
    browser.moveTabTo(browser.mCurrentTab, browser.mCurrentTab._tPos + 1);
  } else {
    browser.moveTabTo(browser.mCurrentTab, 0);
  }
}, '選択中のタブを右へ');

key.setViewKey('U', function (ev, arg) {
  ext.exec("list-closed-tabs", arg, ev);
}, 'List closed tabs', true);

key.setViewKey('j', function (ev) {
  key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, '一行スクロールダウン');

key.setViewKey('k', function (ev) {
  key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, '一行スクロールアップ');

key.setViewKey('p', function (ev, arg) {
  ext.exec('pocket-add-current', arg, ev);
}, 'Add current page', true);

key.setViewKey('P', function (ev, arg) {
  ext.exec('pocket-open-latest', arg, ev);
}, 'Open last saved page', true);

key.setEditKey('C-<tab>', function (ev) {
  command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, '次のテキストエリアへフォーカス');

key.setEditKey('C-a', function (ev) {
  command.beginLine(ev);
}, '行頭へ移動');

key.setEditKey('C-e', function (ev) {
  command.endLine(ev);
}, '行末へ');

key.setEditKey('C-d', function (ev) {
  goDoCommand("cmd_deleteCharForward");
}, '次の一文字削除');

key.setEditKey('C-b', function (ev) {
  command.previousChar(ev);
}, '一文字左へ移動');

key.setEditKey('C-f', function (ev) {
  command.nextChar(ev);
}, '一文字右へ移動');

key.setEditKey('C-h', function (ev) {
  goDoCommand("cmd_deleteCharBackward");
}, '前の一文字を削除');

key.setEditKey('C-k', function (ev) {
  command.killLine(ev);
}, 'カーソルから先を一行カット (Kill line)');

key.setEditKey('C-l', function (ev) {
  command.recenter(ev);
}, 'カーソル位置が画面の中央へ来るようスクロール', true);

key.setEditKey('C-n', function (ev) {
  command.nextLine(ev);
}, '一行下へ');

key.setEditKey('C-p', function (ev) {
  command.previousLine(ev);
}, '一行上へ');

key.setEditKey('C-o', function (ev) {
  command.openLine(ev);
}, '行を開く (Open line)');

key.setGlobalKey('C-<right>', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setGlobalKey('C-<left>', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');
