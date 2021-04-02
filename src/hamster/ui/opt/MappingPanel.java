package hamster.ui.opt;

import haven.*;
import integrations.mapv4.MapConfig;
import integrations.mapv4.MappingClient;

import java.awt.event.KeyEvent;

public class MappingPanel extends Widget {
    public MappingPanel(final UI ui) {
	super(new Coord(UI.scale(500), UI.scale(395)));
	final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

	int y = 0, x = 0;
	y += add(new Label("Online Auto-Mapper Service:")).sz.y + spacer.y;

	x = add(new Label("Server URL:"), new Coord(0, y)).sz.x + spacer.x;

	y += add(
		new TextEntry(240, Utils.getpref("vendan-mapv4-endpoint", "")) {
		    public boolean keydown(KeyEvent ev) {
			if (!parent.visible)
			    return false;
			Utils.setpref("vendan-mapv4-endpoint", text);
			if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
			    MappingClient.getInstance(ui.sess.username).SetEndpoint(Utils.getpref("vendan-mapv4-endpoint", ""));
			}
			return buf.key(ev);
		    }
		}, new Coord(x, y)).sz.y;

	y += add(new CheckBox("Enable mapv4 mapper") {
	    public void set(boolean val) {
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    MapConfig.saveMapSetting(ui.sess.username, val, "mapper");
		    MappingClient.getInstance(ui.sess.username).EnableGridUploads(val);
		    a = val;
		}
	    }

	    public void tick(double dt) {
		super.tick(dt);
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    boolean b = MapConfig.loadMapSetting(ui.sess.username, "mapper");
		    if (a != b) {
			a = b;
			MappingClient.getInstance(ui.sess.username).EnableGridUploads(a);
		    }
		}
	    }
	}, new Coord(0, y)).sz.y;

	y += add(new CheckBox("Enable navigation tracking") {
	    public void set(boolean val) {
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    MapConfig.saveMapSetting(ui.sess.username, val, "track");
		    MappingClient.getInstance(ui.sess.username).EnableTracking(val);
		    a = val;
		}
	    }

	    public void tick(double dt) {
		super.tick(dt);
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    boolean b = MapConfig.loadMapSetting(ui.sess.username, "track");
		    if (a != b) {
			a = b;
			MappingClient.getInstance(ui.sess.username).EnableTracking(a);
		    }
		}
	    }
	}, new Coord(0, y)).sz.y;
	y += add(new CheckBox("Upload custom GREEN markers to map") {
	    public void set(boolean val) {
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    MapConfig.saveMapSetting(ui.sess.username, val, "green");
		    a = val;
		}
	    }

	    public void tick(double dt) {
		super.tick(dt);
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    boolean b = MapConfig.loadMapSetting(ui.sess.username, "green");
		    if (a != b) {
			a = b;
		    }
		}
	    }
	}, new Coord(0, y)).sz.y;

	pack();
    }
}
