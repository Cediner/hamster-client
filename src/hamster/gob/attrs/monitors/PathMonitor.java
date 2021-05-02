package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.gob.Tag;
import hamster.gob.sprites.GobPathSprite;
import haven.*;
import haven.render.BaseColor;

public class PathMonitor extends GAttrib {
    public PathMonitor(final Gob g) {
	super(g);
    }

    @Override
    public void ctick(double dt) {
	super.ctick(dt);
	final UI ui = gob.glob.ui.get();
	if(ui != null) {
	    final GameUI gui = ui.gui;
	    final Moving mv = gob.getattr(Moving.class);
	    if (gui != null && mv != null && gob.findol(GobPathSprite.id) == null) {
		final Coord2d dest = mv.getDest().orElse(null);
		if(dest != null) {
		    if (gob.hasTag(Tag.HUMAN) && !gob.isHeldBySomething() && GlobalSettings.SHOWGOBPATH.get()) {
		        final KinInfo ki = gob.getattr(KinInfo.class);
		        final BaseColor col = gui.map != null && gui.map.rlplgob == gob.id ? GlobalSettings.GOBPATHCOL.get()
				: ki != null ? new BaseColor(BuddyWnd.gc[ki.group]) : new BaseColor(BuddyWnd.gc[GlobalSettings.BADKIN.get()]);
			gob.addol(new Gob.Overlay(gob, GobPathSprite.id, new GobPathSprite(gob, dest, gob.rc, col)));
		    } else if (gob.hasTag(Tag.ANIMAL) && !gob.hasTag(Tag.TAMED_ANIMAL) && GlobalSettings.SHOWANIMALPATH.get()) {
			gob.addol(new Gob.Overlay(gob, GobPathSprite.id, new GobPathSprite(gob, dest, gob.rc, GlobalSettings.ANIMALPATHCOL.get())));
		    } else if(gob.hasTag(Tag.VEHICLE) && GlobalSettings.SHOWGOBPATH.get()) {
		        gob.addol(new Gob.Overlay(gob, GobPathSprite.id, new GobPathSprite(gob, dest, gob.rc, GlobalSettings.VEHPATHCOL.get())));
		    }
		}
	    }
	}
    }

    @Override
    public String toString() {
	return "PathMonitor";
    }
}
