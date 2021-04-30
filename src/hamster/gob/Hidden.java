package hamster.gob;

import com.google.common.flogger.FluentLogger;
import hamster.GlobalSettings;
import hamster.io.Storage;
import hamster.script.pathfinding.Hitbox;
import hamster.util.ObservableCollection;
import hamster.util.ObservableListener;
import haven.FastMesh;
import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;
import haven.render.RenderTree;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;

/**
 * Hidden is a special GAttrib. If it exists only it will be rendered for this Gob if show hidden is on.
 * Otherwise nothing is rendered.
 */
public class Hidden extends GAttrib implements RenderTree.Node {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final ObservableCollection<String> hidden = new ObservableCollection<>(new HashSet<>());

    public static void init() {
        logger.atInfo().log("Loading hidden");
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_hidden ( name TEXT PRIMARY KEY )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name FROM gob_hidden")) {
                    while (res.next()) {
                        hidden.add(res.getString(1));
                    }
                }
            }
        });
    }

    public synchronized static void listen(final ObservableListener<String> listener) {
        hidden.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableListener<String> listener) {
        hidden.removeListener(listener);
    }

    public synchronized static void add(final String name) {
        hidden.add(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR IGNORE INTO gob_hidden VALUES (?)");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static void remove(final String name) {
        hidden.remove(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_hidden WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    private static class HiddenState implements Pipe.Op {
        public final BaseColor col;

        private HiddenState(final BaseColor col) {
            this.col = col;
        }

        public void apply(Pipe buf) {
            buf.prep(col);
        }

        public boolean equals(HiddenState that) {
            return that.col.equals(col);
        }

        public boolean equals(Object o) {
            return ((o instanceof HiddenState) && equals((HiddenState) o));
        }


    }

    public static boolean isHidden(final String name) {
        return hidden.contains(name);
    }

    private final FastMesh mesh;
    private HiddenState state;

    public Hidden(final Gob g) {
        super(g);
        mesh = Hitbox.hbfor(g).mesh();
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        final UI ui = gob.glob.ui.get();
        if(ui != null && ui.gui != null) {
            state = new HiddenState(GlobalSettings.GOBHIDDENCOL.get());
            slot.add(mesh);
            slot.ostate(state);
        }
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        if(ui != null && ui.gui != null && slots != null) {
            final boolean upd;
            upd = !state.col.equals(GlobalSettings.GOBHIDDENCOL.get());
            if(upd)
                state = new HiddenState(GlobalSettings.GOBHIDDENCOL.get());

            if(upd) {
                for (final RenderTree.Slot s : slots) {
                    s.ostate(state);
                }
            }
        }
    }

    @Override
    public String toString() {
        return "Hidden";
    }
}
