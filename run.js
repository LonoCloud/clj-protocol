try {
    require("source-map-support").install();
} catch(err) {
}
require("./out/goog/bootstrap/nodejs.js");
require("./out/dhcp.js");
goog.require("dhcp.basic_server");
goog.require("cljs.nodejscli");
