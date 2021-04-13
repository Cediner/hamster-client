package hamster.util;

import haven.Disposable;

import java.util.HashMap;
import java.util.Map;

public class TimedCache<T extends Disposable> {
    private final Map<T, Long> itmmap = new HashMap<>();
    private final long maxlife;

    public TimedCache(final long maxlife) {
        this.maxlife = maxlife;
    }

    public synchronized void access(final T itm) {
        itmmap.put(itm, itmmap.getOrDefault(itm, System.currentTimeMillis()));
    }

    public synchronized void tick() {
        final var now = System.currentTimeMillis();
        final var itr = itmmap.keySet().iterator();
        while(itr.hasNext()) {
            final var itm = itr.next();
            if((now - itmmap.get(itm)) >= maxlife) {
                itm.dispose();
                itr.remove();
            }
        }
    }
}
