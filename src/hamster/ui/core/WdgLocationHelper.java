package hamster.ui.core;

import haven.Coord;
import haven.Coord2d;
import haven.UI;
import haven.Widget;
import hamster.io.Storage;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class WdgLocationHelper {
    public interface HitCheck {
        boolean check(final Coord mc, final int button);
    }

    public interface RelPos {
        Coord2d get();
    }

    public interface ApplyPos {
        void set(final Coord2d pos, final boolean locked);
    }

    private static final Map<String, Coord2d> knownPositions = new HashMap<>();
    private static final Map<String, Boolean> knownLocks = new HashMap<>();

    static {
        //These settings are stored in dynamic.sqlite under `widget_position`
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS widget_position ( name TEXT PRIMARY KEY, x REAL, y REAL, locked BOOLEAN)");
                //For older client versions widget_position didn't have the `locked` column and it needs added in
                try (final ResultSet res = stmt.executeQuery("PRAGMA table_info(widget_position)")) {
                    boolean has3 = false;
                    while (res.next()) {
                        if (res.getInt(1) == 3) {
                            has3 = true;
                            break;
                        }
                    }

                    if (!has3) {
                        //Older client, make the column
                        stmt.executeUpdate("ALTER TABLE widget_position ADD COLUMN locked BOOLEAN");
                    }
                }
            }
        });
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name, x, y, locked FROM widget_position")) {
                    while (res.next()) {
                        final String name = res.getString(1);
                        final double x = res.getDouble(2);
                        final double y = res.getDouble(3);
                        final boolean locked = res.getBoolean(4);
                        knownPositions.put(name, new Coord2d(x, y));
                        knownLocks.put(name, locked);
                    }
                }
            }
        });
    }

    public static final double VISIBLE_PER = 0.8;

    private final Supplier<Widget> wdg;
    private final HitCheck moveHit;
    private final RelPos relpos;
    private final ApplyPos applypos;
    //Database key
    private final String key;
    //Whether we want to lock the current position or not
    private boolean lock;

    private UI.Grab dm = null;
    private Coord doff;

    public WdgLocationHelper(final String key, final Supplier<Widget> wdg,
			     final HitCheck moveHit, final RelPos relpos, final ApplyPos applypos) {
        this.key = key;
        this.wdg = wdg;
        this.moveHit = moveHit;
        this.relpos = relpos;
        this.applypos = applypos;
    }

    public void added() {
        lock = knownLocks.getOrDefault(key, false);
        loadPosition();
    }

    private void loadPosition() {
        if (key != null && knownPositions.containsKey(key)) {
            applypos.set(knownPositions.get(key), lock);
        }
    }

    public boolean moving() {
        return dm != null;
    }

    public void savePosition() {
        if (key != null) {
            final Coord2d rel = relpos.get();
            knownPositions.put(key, rel);
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO widget_position VALUES (?, ?, ?, ?)");
                stmt.setString(1, key);
                stmt.setDouble(2, rel.x);
                stmt.setDouble(3, rel.y);
                stmt.setBoolean(4, lock);
                stmt.executeUpdate();
            });
        }
    }

    public void toggleLock() {
        lock = !lock;
        savePosition();
    }

    public boolean locked() {
        return lock;
    }

    public boolean mousedown(final Coord mc, final int button) {
        if (moveHit.check(mc, button)) {
            if (!lock) {
                final Widget wdg = this.wdg.get();
                dm = wdg.ui.grabmouse(wdg);
                doff = mc;
                wdg.parent.setfocus(wdg);
                wdg.raise();
            }
            return true;
        } else {
            return false;
        }
    }

    public boolean mouseup(final Coord mc, final int button) {
        if (dm != null) {
            //Preference to this if we're in the middle of moving the widget
            dm.remove();
            dm = null;
            //Ensure user didn't throw the window right off the visible screen...
            final Widget wdg = this.wdg.get();
            if ((wdg.c.x + wdg.sz.x * VISIBLE_PER) > wdg.parent.sz.x) {
                wdg.c.x = wdg.parent.sz.x - wdg.sz.x;
            } else if ((wdg.c.x + (wdg.sz.x * VISIBLE_PER)) < 0) {
                wdg.c.x = 0;
            }
            if ((wdg.c.y + wdg.sz.y * VISIBLE_PER) > wdg.parent.sz.y) {
                wdg.c.y = wdg.parent.sz.y - wdg.sz.y;
            } else if ((wdg.c.y + (wdg.sz.y * VISIBLE_PER)) < 0) {
                wdg.c.y = 0;
            }
            savePosition();
            return true;
        } else {
            return false;
        }
    }

    public boolean mousemove(final Coord mc) {
        if (dm != null) {
            //Preference to this if we're in the middle of moving the widget
            final Widget wdg = this.wdg.get();
            wdg.c = wdg.c.add(mc.add(doff.inv()));
            return true;
        } else {
            return false;
        }
    }
}
