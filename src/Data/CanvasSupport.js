"use strict";

exports.fromDataSource = function(src) {
    var im = new Image();
    im.src = src;
    return im;
}

exports.getBoundingClientRect_ = function(elm) {
    return elm.getBoundingClientRect();
}

exports.removaAllListeners = function(x) {
    return function() {
        document.body.outerHTML = document.body.outerHTML;
    }
}

exports.imageSmoothingEnabled = function(ctx) {
    return function(bool) {
        return function() {
            return ctx.imageSmoothingEnabled = bool
        }
    }
}