var {Cc, Ci, Cu, Cr} = require("chrome");

Cu.import("resource://gre/modules/FileUtils.jsm");
var env = Cc["@mozilla.org/process/environment;1"]
            .getService(Ci.nsIEnvironment);
var shell = new FileUtils.File("/Users/pancia/Bookmarks/dist/build/bookmark-manager/bookmark-manager");

Cu.import("resource://gre/modules/XPCOMUtils.jsm", this);
var bookmarkService = Cc["@mozilla.org/browser/nav-bookmarks-service;1"]
                        .getService(Ci.nsINavBookmarksService);
var tagService = Cc["@mozilla.org/browser/tagging-service;1"]
                   .getService(Ci.nsITaggingService);
var IOService = Cc["@mozilla.org/network/io-service;1"]
                .getService(Ci.nsIIOService);

var bookmarkObserver = {
  onItemAdded: function(id, folder, i) {
    var url = bookmarkService.getBookmarkURI(id).spec;
    console.log("added ", url);
    var uri = IOService.newURI(url, null, null);
    console.log("with tags: ", tagService.getTagsForURI(uri, {}));
    var args = ["-s", "unix", "-f", "/Users/pancia/Bookmarks/bms.db"];
    var process = Cc["@mozilla.org/process/util;1"]
        .createInstance(Ci.nsIProcess);
    process.init(shell);
    process.runAsync(args, args.length);
  },
  QueryInterface: XPCOMUtils.generateQI([Ci.nsINavBookmarkObserver])
};

exports.main = function() {
  bookmarkService.addObserver(bookmarkObserver, false);
};

exports.onUnload = function() {
  bookmarkService.removeObserver(bookmarkObserver);
}
