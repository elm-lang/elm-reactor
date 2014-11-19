
// A note to the reader:
// This file is concatenated with debuggerInterface.elm's compiled
// javascript and debug-core.js. This is done at build time in Setup.hs

var prettyPrint = function(){

    var independentRuntime = {};
    var List;
    var ElmArray;
    var Dict;

    var toString = function(v, separator) {
        var type = typeof v;
        if (type === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        } else if (type === "boolean") {
            return v ? "True" : "False";
        } else if (type === "number") {
            return v.toFixed(2).replace(/\.0+$/g, '');
        } else if ((v instanceof String) && v.isChar) {
            return "'" + addSlashes(v) + "'";
        } else if (type === "string") {
            return '"' + addSlashes(v) + '"';
        } else if (type === "object" && '_' in v && probablyPublic(v)) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i], separator));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k], separator));
            }
            if (output.length === 0) return "{}";
            var body = "\n" + output.join(",\n");
            return "{" + body.replace(/\n/g,"\n" + separator) + "\n}";
        } else if (type === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k], separator));
                }
                return "(" + output.join(", ") + ")";
            } else if (v.ctor === "_Array") {
                if (!ElmArray) {
                    ElmArray = Elm.Array.make(independentRuntime);
                }
                var list = ElmArray.toList(v);
                return "Array.fromList " + toString(list, separator);
            } else if (v.ctor === "::") {
                var output = '[\n' + toString(v._0, separator);
                v = v._1;
                while (v && v.ctor === "::") {
                    output += ",\n" + toString(v._0, separator);
                    v = v._1;
                }
                return output.replace(/\n/g,"\n" + separator) + "\n]";
            } else if (v.ctor === "[]") {
                return "[]";
            } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                if (!Dict || !List) {
                    Dict = Elm.Dict.make(independentRuntime);
                    List = Elm.List.make(independentRuntime);
                }
                var list = Dict.toList(v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list, separator);
            } else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i], separator);
                    var parenless = str[0] === '{' ||
                                    str[0] === '<' ||
                                    str[0] === "[" ||
                                    str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : "(" + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (type === 'object' && 'recv' in v) return '<signal>';
        return "<internal structure>";
    };

    function addSlashes(str) {
        return str.replace(/\\/g, '\\\\')
                  .replace(/\n/g, '\\n')
                  .replace(/\t/g, '\\t')
                  .replace(/\r/g, '\\r')
                  .replace(/\v/g, '\\v')
                  .replace(/\0/g, '\\0')
                  .replace(/\'/g, "\\'")
                  .replace(/\"/g, '\\"');
    }

    function probablyPublic(v) {
        var keys = Object.keys(v);
        var len = keys.length;
        if (len === 3
            && 'props' in v
            && 'element' in v) return false;
        if (len === 5
            && 'horizontal' in v
            && 'vertical' in v
            && 'x' in v
            && 'y' in v) return false;
        if (len === 7
            && 'theta' in v
            && 'scale' in v
            && 'x' in v
            && 'y' in v
            && 'alpha' in v
            && 'form' in v) return false;
        return true;
    }

    return toString;
}();
