package hamster.gob.attrs.monitors;

import hamster.gob.Tag;
import hamster.io.Storage;
import haven.*;
import haven.res.gfx.fx.bprad.BPRad;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class RangeMonitor extends GAttrib {
    private static final Map<String, Integer> rangemap = new HashMap<>();
    public static void init(final Storage internal) {
	internal.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery(
			"SELECT object.name, object_range.range " +
				"FROM object JOIN object_range USING (object_id)")) {
		    while (res.next()) {
			final String name = res.getString(1);
			final int tiles = res.getInt(2);
			rangemap.put(name, tiles);
		    }
		}
	    }
	});
    }

    public static boolean hasRange(final String resname) {
	return rangemap.containsKey(resname);
    }

    public RangeMonitor(final Gob g) {
	super(g);
    }

    @Override
    public void ctick(double dt) {
	super.ctick(dt);
	final UI ui = gob.glob.ui.get();
	if(ui != null) {
	    final GameUI gui = ui.gui;
	    if(gui != null) {
	        final var bpol = gob.findol(BPRad.CUSTOM_BPRAD_ID);
	        if(bpol == null) {
		    if (gui.settings.SHOWANIMALRADIUS.get() && gob.hasTag(Tag.ANIMAL) && !gob.isDead()) {
		        gob.addol(new Gob.Overlay(gob, BPRad.CUSTOM_BPRAD_ID, new BPRad(gob, null, rangemap.get(gob.name()))));
		    }
		} else if(gob.isDead() || (!gui.settings.SHOWANIMALRADIUS.get() && gob.hasTag(Tag.ANIMAL))) {
		    ((BPRad) bpol.spr).rem();
		}
	    }
	}
    }
}
