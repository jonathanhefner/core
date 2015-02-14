Elm.Native.Generator = {};
Elm.Native.Generator.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Generator = localRuntime.Native.Generator || {};
    if (localRuntime.Native.Generator.values)
    {
        return localRuntime.Native.Generator.values;
    }
    if ('values' in Elm.Native.Generator)
    {
        return localRuntime.Native.Generator.values = Elm.Native.Generator.values;
    }

    var Utils = Elm.Native.Utils.make(localRuntime);

    var Nil = Utils.Nil;
    var Cons = Utils.Cons;

    function elide(gen) {
        return gen;
    }

    function toList(gen) {
        var next = gen.next;
        var peek = gen.peek;
        var st = gen.start;
        var until = gen.until;
        var limit = Math.max(gen.limit, 0);
        var x;
        var root = { _1: Nil };
        var curr = root;

        while (limit && st !== until) {
            x = peek(st);

            // if value exists, add to list (via mutation! muahaha!)
            if (x.ctor !== 'Nothing') {
                curr._1 = Cons(x._0, Nil);
                curr = curr._1;
                limit--;
            }

            st = next(st);
        }

        return root._1;
    }

    function foldl(f, b, gen) {
        var next = gen.next;
        var peek = gen.peek;
        var st = gen.start;
        var until = gen.until;
        var limit = Math.max(gen.limit, 0);
        var x;
        var acc = b;

        while (limit && st !== until) {
            x = peek(st);
            if (x.ctor !== 'Nothing') {
                acc = A2(f, x._0, acc);
                limit--;
            }
            st = next(st);
        }

        return acc;
    }

    function foldr(f, b, gen) {
        if (gen.limit <= 0 || gen.start === gen.until) { return b; }

        var next = gen.next;
        var peek = gen.peek;
        var st = gen.start;
        var until = gen.until;
        var limit = Math.max(gen.limit, 0);
        var x;

        // build array so we can iterate from the right
        var ary = [];
        while (limit && st !== until) {
            x = peek(st);
            if (x.ctor !== 'Nothing') {
                ary.push(x._0);
                limit--;
            }
            st = next(st);
        }

        // fold from the right
        var acc = b;
        for (var i = ary.length; i--; ) {
            acc = A2(f, ary[i], acc);
        }

        return acc;
    }

    Elm.Native.Generator.values = {
        elide: elide,
        toList: toList,
        foldl: F3(foldl),
        foldr: F3(foldr),
    };
    return localRuntime.Native.Generator.values = Elm.Native.Generator.values;
};
