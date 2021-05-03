package hamster.script;

import com.google.common.flogger.FluentLogger;
import hamster.util.IDPool;
import hamster.util.ObservableMap;
import hamster.util.ObservableMapListener;
import haven.Widget;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

public class Context {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private final ObservableMap<Long, Script> scripts = new ObservableMap<>(new HashMap<>());
    private final IDPool idpool = new IDPool(0, Integer.MAX_VALUE);
    private ScriptDescription lastScript = null;

    public Context() {
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    // Script management
    //////////////////////////////////////////////////////////////////////////////////////////////
    public void listenTo(final ObservableMapListener<Long, Script> listener) {
        scripts.addListener(listener);
    }

    public void stopListeningTo(final ObservableMapListener<Long, Script> listener) {
        scripts.removeListener(listener);
    }

    public synchronized void killLast() {
        if(scripts.size() > 0) {
            scripts.values()
                    .stream()
                    .reduce((x, y) -> x.time() < y.time() ? x : y)
                    .ifPresent(Script::interrupt);
        }
    }

    public synchronized void killAll() {
        scripts.values().forEach(Script::interrupt);
    }

    public Optional<ScriptDescription> lastScript() {
        return Optional.ofNullable(lastScript);
    }

    public synchronized void remove(final long sid) {
        scripts.remove(sid);
        idpool.release(sid);
    }

    public synchronized void launchJavaScript(final Class<? extends Script> scls, final SessionDetails session) {
        final Script thr;
        try {
            final Constructor<? extends Script> con = scls.getConstructor(Long.class, SessionDetails.class);
            thr = con.newInstance(idpool.next(), session);
        } catch (Exception e) {
            logger.atSevere().withCause(e).log("Failed to create script [%s]", scls);
            return;
        }
        scripts.put(thr.sid(), thr);
        thr.start();
    }


    public synchronized void launchLispScript(final String script, final SessionDetails session) {
        final Script thr = new LispScript(script, idpool.next(), session);
        lastScript = new ScriptDescription(script, ScriptDescription.Type.Lisp);
        scripts.put(thr.sid(), thr);
        thr.start();
    }

    public synchronized void launchLuaScript(final String script, final SessionDetails session) {
        final Script thr = new LuaScript(script, idpool.next(), session);
        lastScript = new ScriptDescription(script, ScriptDescription.Type.Lua);
        scripts.put(thr.sid(), thr);
        thr.start();
    }

    public synchronized void launch(final ScriptDescription desc, final SessionDetails session) {
        switch (desc.type) {
            case Lisp -> launchLispScript(desc.name, session);
            case Lua -> launchLuaScript(desc.name, session);
        }
    }

    public synchronized void dispatchmsg(final Widget wdg, final String msg, final Object... args) {
        for (final Script script : scripts.values()) {
            script.newmsg(wdg, msg, args);
        }
    }

    public synchronized void dispatchmsg(final boolean trusted, final Widget wdg, final String msg, final Object... args) {
        for (final Script script : scripts.values()) {
            if (script.allowExternal() || trusted) {
                script.newmsg(wdg, msg, args);
            }
        }
    }
}
