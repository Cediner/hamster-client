package hamster.ui.map;

import haven.*;
import haven.render.Pipe;

public class PreviewGob extends Gob {
    public PreviewGob(final Gob org) {
	super(org.glob, new Coord2d(0, 0));
	synchronized (this) {
	    for (GAttrib a : org.attr.values()) {
		if (a instanceof ResDrawable) {
		    setattr(((ResDrawable) a).cloneFor(this));
		} else if (a instanceof Composite) {
		    setattr(((Composite) a).cloneFor(this));
		}
	    }
	    for (Gob.Overlay ol : org.ols) {
	        if(ol.res != null)
	            addol(new Overlay(this, ol.id, ol.res, ol.sdt != null ? ol.sdt.clone() : new MessageBuf()));
	    }
	}
    }

    @Override
    protected Pipe.Op getmapstate(Coord3f pc) {
	return null;
    }

    @Override
    public Coord3f getrc() {
	return new Coord3f((float)rc.x, (float)rc.y, 0f);
    }
}
