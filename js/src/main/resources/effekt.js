//var effekt = require("effekt")

function Pure(a) {
  this.value = a
}
Pure.prototype.tag = 0

function Impure(c, k) {
  this.control = c
  this.k = k
}
Impure.prototype.tag = 1


function trampoline(res) {
    var r = res
    while (r.tag === 1) {
        r = r.control.apply(r.k)
    }
    return r.value;
}

var mc = {
    apply: function (a) {},
    append: function (k) {},
    map: function (f) { return this.flatMap(x => ControlJS.pure(f(x))) },
    flatMap: function (f) { return FrameCont(f, this); },
    splitAt: function (p) { throw "Prompt " + p + " not found" }
}


// Use linked list for now!
function Cons(h, t) {
    this.head = h
    this.tail = t
}

function FrameCont(f, tail) {
    return new FramesCont(new Cons(f), tail);
}

function FramesCont(frames, tail) {
    this.frames = frames;
    this.tail   = tail;
}
FramesCont.prototype = Object.create(mc);
FramesCont.prototype.map = function (f) {
    var fs = this.frames
    return new FramesCont(
        new Cons(x => fs.head(f(x)), fs.tail),
        this.tail)
}
FramesCont.prototype.flatMap = function (f) {
    var fs = this.frames
    return new FramesCont(new Cons(f, fs), this.tail);
}
FramesCont.prototype.append = function (k) {
    return new FramesCont(this.frames, this.tail.append(k));
}
FramesCont.prototype.splitAt = function (p) {
    var res = this.tail.splitAt(p);
    return new SplitResult(
        new FramesCont(this.frames, res.init),
        res.handler,
        res.tail);
}
FramesCont.prototype.apply = function (a) {
    var res = this.frames.head(a)

    if (!this.frames.tail) {
        return new Impure(res, this.tail)
    } else {
        return new Impure(res, new FramesCont(this.frames.tail, this.tail))
    }
}


//
//function FrameCont(f, tail) {
//    this.frame = f
//    this.tail = tail
//}
//FrameCont.prototype = Object.create(mc)
//FrameCont.prototype.append = function (k) {
//    return this.tail.append(k).flatMap(this.frame)
//}
//FrameCont.prototype.splitAt = function (p) {
//    var res = this.tail.splitAt(p);
//    return new SplitResult(
//        new FrameCont(this.frame, res.init),
//        res.handler,
//        res.tail);
//}
//FrameCont.prototype.apply = function (a) {
//    return new Impure(this.frame(a), this.tail)
//}

var CastCont = Object.create(mc)
CastCont.apply = function (a) { return new Pure(a) }
CastCont.append = function (k) { return k }
CastCont.map = function (g) { return new ReturnCont(g) }

function ReturnCont(f) {
    this.f = f
}
ReturnCont.prototype = Object.create(mc)
ReturnCont.prototype.append = function (k) {
    return k.map(this.apply);
}
ReturnCont.prototype.apply = function (x) {
    return new Pure(this.f(x));
}
ReturnCont.prototype.map = function (g) {
    var fun = this.f
    return new ReturnCont(x => fun(g(x)))
}

function SplitResult(init, handler, tail) {
    this.init = init
    this.handler = handler
    this.tail = tail
}

function HandlerCont(handler, tail) {
    this.prompt  = handler.prompt
    this.handler = handler
    this.tail    = tail
}
HandlerCont.prototype = Object.create(mc)
HandlerCont.prototype.splitAt = function (p) {
    if (this.prompt === p) {
        return new SplitResult(CastCont, this.handler, this.tail)
    } else {
        var res = this.tail.splitAt(p)
        return new SplitResult(
            new HandlerCont(this.handler, res.init),
            res.handler,
            res.tail)
    }
}
HandlerCont.prototype.apply = function (a) { return this.tail.apply(a) }
HandlerCont.prototype.append = function (k) {
    return new HandlerCont(this.handler, this.tail.append(k));
}

var idCont = new ReturnCont(x => x);

function Control(apply) {
    this.apply = apply
}
Control.prototype = {
    run: function () { return trampoline(this.apply(idCont)) },
    map: function (f) {
        var ap = this.apply;
        return new Control(k => ap(k.map(f)))
    },
    flatMap: function (f) {
        var ap = this.apply;
        return new Control(k => ap(k.flatMap(f)));
    }
}
function Trivial(a) {
    this.value = a
}
Trivial.prototype = {
    run: function () { return this.value },
    map: function (f) { return new Trivial(f(this.value)) },
    flatMap: function (f) { return f(this.value) },
    apply: function (k) { return k.apply(this.value) }
}

var id = 0

function Handler(p, s) {
    return { prompt: p, state: s }
}

function updateWith(h, s) {
    return Handler(h.prompt, s);
}

ControlJS = {

  pure: function(a) { return new Trivial(a) },

  use: function (c, f) {
    return new Control(k => {
        var parts = k.splitAt(c.prompt);
        var init = parts.init
        var handler = parts.handler
        var tail = parts.tail

        function localCont(a, updatedState) {
            return new Control(k => {
                var updatedHandler = updateWith(handler, updatedState);

                // here is the reason why handler is not
                // the last on the stack and a return
                // frame is inbetween! This is
                // caused by append on init (which is a
                // frame)
                // No! we have the same behavior in Scala.
                var repushedPrompt = init.append(
                    new HandlerCont(updatedHandler, k));
                return repushedPrompt.apply(a);
            });
        }

        var handled = f(handler.state, localCont);
        return new Impure(handled, tail);
    });
  },

  handle: function(e, init, f) {
    var prompt = id++;
    var capability = { prompt: prompt, effect: e };

    // not sure this has the right lazy/eager behavior
    var handler = Handler(prompt, init)
    var c = f(capability).flatMap(a =>
        // TODO use effekt.JSControl.unit
        new Control(k => k.tail.apply(e.unit(k.handler.state, a)))
    )
    return new Control(k => new Impure(c, new HandlerCont(handler, k)));
  }
}

// Handlers in JS, since we can't interop in the other way yet.

var state = {
    unit: (s, a) => a,
    get: _  => (s, k)  => k(s, s),
    put: s2 => (s1, k) => k(null, s2)
}

var amb = {
    unit: (s, a) => [a],
    flip: _  => (s, k) => k(true, s).flatMap(ts =>
        k(false, s).map(fs => ts.concat(fs) )
    )
}

function flipN(n, ambC) {
    if (n == 0) {
        return ControlJS.pure(false)
    } else {
        return ControlJS.use(ambC, amb.flip()).flatMap(x =>
            flipN(n - 1, ambC).map(y => x || y))
    }
}

function flipCounter(ambC, stateC) {
    return ControlJS.use(stateC, state.get()).flatMap(c => {
        if (c <= 0) {
            return ControlJS.use(ambC, amb.flip())
        } else {
            return ControlJS.use(stateC, state.put(c - 1)).flatMap(_ =>
                flipCounter(ambC, stateC).map(x => !x)
            )
        }
    })
}

function main() {
    var res = ControlJS.handle(amb, null, ambC =>
        ControlJS.handle(state, 1000, stateC => flipCounter(ambC, stateC)))

    var before = Date.now()
    res.run()
    var after = Date.now()
}

var before = Date.now();
for (var i = 1000; i > 0; i--) {
  main()
}
var after = Date.now();
console.log(after - before);
