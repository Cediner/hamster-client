package hamster.ui;

import hamster.script.PointerData;
import hamster.script.pathfinding.Move;
import hamster.ui.core.Theme;
import haven.*;
import haven.render.BaseColor;
import haven.render.Model;

import java.awt.*;

import static haven.OCache.posres;
import static java.lang.Math.abs;

/**
 * Based off Loftar's Point for custom uses
 */
public class CustomPointer extends Widget {
    private static final Tex icon = Theme.tex("pointer");
    private static final BaseColor col = new BaseColor(new Color(241, 227, 157, 255));
    private final long gobid;
    private Coord2d tc;
    private Coord lc;
    private final Tex licon;
    private final PointerData data;

    //Tooltip
    private Text.Line tt = null;
    private int dist;

    public CustomPointer(final String name, final Tex icon, final long gobid, final Coord2d tc) {
	this.licon = icon;
	this.gobid = gobid;
	this.tc = tc;
	data = new PointerData(name, tc);
    }

    public CustomPointer(final String name, final Resource icon, final long gobid, final Coord2d tc) {
        this(name, icon.layer(Resource.imgc).tex(), gobid, tc);
    }

    // Non-gob based pointers
    public CustomPointer(final String name, final Resource icon, final Coord2d tc) {
	this(name, icon, -1, tc);
    }

    public CustomPointer(final String name, final Coord2d tc) {
	this(name, CustomPointer.icon, -1, tc);
    }

    public CustomPointer(final String name) {
	this(name, CustomPointer.icon, -1, null);
    }

    // Gob based pointers
    public CustomPointer(final String name, final Resource icon, final Gob g) {
	this(name, icon, g.id, g.rc);
    }

    public CustomPointer(final String name,  final Gob g) {
	this(name, CustomPointer.icon, g.id, g.rc);
    }

    public void updatetc(final Coord2d tc) {
        this.tc = tc;
    }

    public void presize() {
	resize(parent.sz);
    }

    protected void added() {
	presize();
	super.added();
	ui.sess.details.attachPointer(data);
    }

    @Override
    protected void removed() {
	super.removed();
	ui.sess.details.removePointer(data);
    }

    private int signum(int a) {
	return Integer.compare(a, 0);
    }

    private void drawarrow(GOut g, Coord tc) {
	Coord hsz = sz.div(2);
	tc = tc.sub(hsz);
	if(tc.equals(Coord.z))
	    tc = new Coord(1, 1);
	double d = Coord.z.dist(tc);
	Coord sc = tc.mul((d - 25.0) / d);
	float ak = ((float)hsz.y) / ((float)hsz.x);
	if((abs(sc.x) > hsz.x) || (abs(sc.y) > hsz.y)) {
	    if(abs(sc.x) * ak < abs(sc.y)) {
		sc = new Coord((sc.x * hsz.y) / sc.y, hsz.y).mul(signum(sc.y));
	    } else {
		sc = new Coord(hsz.x, (sc.y * hsz.x) / sc.x).mul(signum(sc.x));
	    }
	}
	Coord ad = sc.sub(tc).norm(UI.scale(30.0));
	sc = sc.add(hsz);

	g.usestate(col);
	g.drawp(Model.Mode.TRIANGLES, new float[] {
		sc.x, sc.y,
		sc.x + ad.x - (ad.y / 3), sc.y + ad.y + (ad.x / 3),
		sc.x + ad.x + (ad.y / 3), sc.y + ad.y - (ad.x / 3),
	});

	g.aimage(licon, sc.add(ad), 0.5, 0.5);
	this.lc = sc.add(ad);
    }

    public void draw(GOut g) {
	this.lc = null;
	if(tc == null)
	    return;
	Gob gob = (gobid < 0) ? null : ui.sess.glob.oc.getgob(gobid);
	Coord3f sl;
	if(gob != null) {
	    try {
		sl = getparent(GameUI.class).map.screenxf(gob.getc());
	    } catch(Loading l) {
		return;
	    }
	} else {
	    sl = getparent(GameUI.class).map.screenxf(tc);
	}
	if(sl != null)
	    drawarrow(g, new Coord(sl));
    }

    @Override
    public boolean mousedown(Coord c, int button) {
	if (this.lc != null && this.lc.dist(c) < 20.0) {
	    if (gobid > 0) {
		ui.gui.map.wdgmsg("click", rootpos().add(c), this.tc.floor(posres), button,
			ui.modflags(), 0, (int) gobid, this.tc.floor(posres), 0, -1);
	    } else {
		ui.gui.map.queuemove(new Move(this.tc));
	    }
	    return true;
	} else {
	    return super.mousedown(c, button);
	}
    }

    private void updtt() {
	final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
	if (me != null) {
	    final Coord2d ltc;
	    if(gobid != -1) {
		final Gob ot = ui.sess.glob.oc.getgob(gobid);
		if(ot != null) {
		    ltc = tc = ot.rc;
		} else {
		    ltc = tc;
		}
	    } else {
		ltc = tc;
	    }

	    if(ltc != null) {
		final int cdist = (int) (Math.ceil(me.rc.dist(ltc) / 11.0));
		if (cdist != dist) {
		    dist = cdist;
		    final String extra;
		    if (dist >= 1000) {
			extra = " - May be further than the client can see";
		    } else {
			extra = "";
		    }
		    if (tt != null && tt.tex() != null)
			tt.tex().dispose();
		    tt = Text.render(data.name() + " - Distance: " + dist + extra);
		}
	    }
	}
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	updtt();
    }

    public Object tooltip(Coord c, Widget wdg) {
	if ((this.lc != null) && (this.lc.dist(c) < 20.0D) && tc != null) {
	    return tt;
	} else {
	    return null;
	}
    }
}
