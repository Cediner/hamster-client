package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.data.gob.ObjData;
import hamster.gob.Tag;
import hamster.gob.attrs.draw2d.Speed;
import haven.*;
import haven.res.gfx.fx.bprad.BPRad;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SpeedMonitor extends GAttrib {
    public SpeedMonitor(final Gob g) {
	super(g);
    }

    @Override
    public void ctick(double dt) {
	super.ctick(dt);
	if(((GlobalSettings.SHOWGOBSPEED.get() && gob.hasTag(Tag.HUMAN)) ||
		(GlobalSettings.SHOWANIMALSPEED.get() && gob.hasTag(Tag.ANIMAL) && !gob.hasTag(Tag.TAMED_ANIMAL)))
		&& !gob.isDead()) {
	    final Speed spd = gob.getattr(Speed.class);
	    if(spd == null) {
		final List<OCache.Delta> deltas = new ArrayList<>();
		deltas.add((gob) -> gob.setattr(new Speed(gob)));
		gob.queueDeltas(deltas);
	    }
	}
    }

    @Override
    public String toString() {
	return "SpeedMonitor";
    }
}
