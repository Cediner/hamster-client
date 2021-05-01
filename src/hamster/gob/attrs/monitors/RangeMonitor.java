package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.data.gob.ObjData;
import hamster.gob.Tag;
import haven.*;
import haven.res.gfx.fx.bprad.BPRad;

public class RangeMonitor extends GAttrib {
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
		    if (GlobalSettings.SHOWANIMALRADIUS.get() && gob.hasTag(Tag.ANIMAL) && !gob.isDead()) {
		        gob.addol(new Gob.Overlay(gob, BPRad.CUSTOM_BPRAD_ID,
				new BPRad(gob, null, ObjData.getRange(gob.name()))));
		    }
		} else if(gob.isDead() || (!GlobalSettings.SHOWANIMALRADIUS.get() && gob.hasTag(Tag.ANIMAL))) {
		    ((BPRad) bpol.spr).rem();
		}
	    }
	}
    }

    @Override
    public String toString() {
	return "RangeMonitor";
    }
}
