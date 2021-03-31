/* Preprocessed source code */
import hamster.script.PointerData;
import hamster.script.pathfinding.Move;
import haven.*;
import haven.render.*;
import java.awt.Color;

import static haven.OCache.posres;
import static java.lang.Math.*;

/* >wdg: Pointer */
@SuppressWarnings("unused") // Dynamically created by resource
public class Pointer extends Widget {
    public static final BaseColor col = new BaseColor(new Color(241, 227, 157, 255));
    public Indir<Resource> icon;
    public Coord2d tc;
    public Coord lc;
    public long gobid = -1;
    public boolean click;
    private Tex licon;
    private final PointerData data = new PointerData("Unknown", Coord2d.z);

    public Pointer(Indir<Resource> icon) {
	super(Coord.z);
	this.icon = icon;
    }

    public static Widget mkwidget(UI ui, Object... args) {
	int iconid = (Integer)args[0];
	Indir<Resource> icon = (iconid < 0) ? null : ui.sess.getres(iconid);
	return(new Pointer(icon));
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

	// gl.glEnable(GL2.GL_POLYGON_SMOOTH); XXXRENDER
	g.usestate(col);
	g.drawp(Model.Mode.TRIANGLES, new float[] {
		sc.x, sc.y,
		sc.x + ad.x - (ad.y / 3f), sc.y + ad.y + (ad.x / 3f),
		sc.x + ad.x + (ad.y / 3f), sc.y + ad.y - (ad.x / 3f),
	});

	if(icon != null) {
	    try {
		if(licon == null)
		    licon = icon.get().layer(Resource.imgc).tex();
		g.aimage(licon, sc.add(ad), 0.5, 0.5);
	    } catch(Loading ignored) {
	    }
	}
	this.lc = sc.add(ad);
    }

    private void drawarrow(GOut g, double a) {
	Coord hsz = sz.div(2);
	double ca = -Coord.z.angle(hsz);
	Coord ac;
	if ((a > ca) && (a < -ca)) {
	    ac = new Coord(sz.x, hsz.y - (int) (Math.tan(a) * hsz.x));
	} else if ((a > -ca) && (a < Math.PI + ca)) {
	    ac = new Coord(hsz.x - (int) (Math.tan(a - Math.PI / 2) * hsz.y), 0);
	} else if ((a > -Math.PI - ca) && (a < ca)) {
	    ac = new Coord(hsz.x + (int) (Math.tan(a + Math.PI / 2) * hsz.y), sz.y);
	} else {
	    ac = new Coord(0, hsz.y + (int) (Math.tan(a) * hsz.x));
	}
	Coord sc = ac.add(Coord.sc(a, 0));
	Coord sc2 = sc.add(Coord.sc(a + Math.PI / 12, -35));
	Coord sc3 = sc.add(Coord.sc(a - Math.PI / 12, -35));

	g.usestate(col);
	g.drawp(Model.Mode.TRIANGLES, new float[] {
		sc.x, sc.y, sc2.x, sc2.y, sc3.x, sc3.y
	});

	if(icon != null) {
	    try {
		if(licon == null)
		    licon = icon.get().layer(Resource.imgc).tex();
		g.aimage(licon, sc.add(Coord.sc(a, -30)), 0.5, 0.5);
	    } catch(Loading ignored) {
	    }
	}
	this.lc = sc.add(Coord.sc(a, -30));
    }

    public void draw(GOut g) {
	this.lc = null;
	if(tc == null)
	    return;
	Gob gob = (gobid < 0) ? null : ui.sess.glob.oc.getgob(gobid);
	Coord2d gobrc;
	Coord3f sl;
	if(gob != null) {
	    try {
	        gobrc = gob.rc;
		sl = getparent(GameUI.class).map.screenxf(gob.getc());
	    } catch(Loading l) {
		return;
	    }
	} else {
	    gobrc = tc;
	    sl = getparent(GameUI.class).map.screenxf(tc);
	}


	if(gobrc != null) {
	    final Double angle = ui.gui.map.screenangle(gobrc, true);
	    if (!angle.equals(Double.NaN)) {
		drawarrow(g, ui.gui.map.screenangle(gobrc, true));
	    } else if(sl != null) {
		drawarrow(g, new Coord(sl));
	    }
	}
    }

    public void update(Coord2d tc, long gobid) {
	this.tc = tc;
	this.gobid = gobid;
    }

    public void uimsg(String name, Object... args) {
	switch (name) {
	    case "upd" -> {
		if (args[0] == null)
		    tc = null;
		else {
		    tc = ((Coord) args[0]).mul(OCache.posres);
		    data.updatec(this.tc);
		}
		if (args[1] == null)
		    gobid = -1;
		else
		    gobid = Utils.uint32((Integer) args[1]);
	    }
	    case "icon" -> {
		int iconid = (Integer) args[0];
		this.icon = (iconid < 0) ? null : ui.sess.getres(iconid);
		licon = null;
	    }
	    case "cl" -> click = ((Integer) args[0]) != 0;
	    default -> {
		super.uimsg(name, args);
		if(name.equals("tip") && args[0] instanceof String) {
		    data.updateName((String)args[0]);
		}
	    }
	}
    }

    @Override
    public boolean mousedown(Coord c, int button) {
	if (this.lc != null && this.lc.dist(c) < 20.0) {
	    if (gobid > 0) {
		ui.gui.map.wdgmsg("click", rootpos().add(c), this.tc.floor(posres), button, ui.modflags(), 0, (int) gobid, this.tc.floor(posres), 0, -1);
	    } else {
		ui.gui.map.queuemove(new Move(this.tc));
	    }
	    return true;
	} else {
	    return super.mousedown(c, button);
	}
    }

    private Text.Line tt = null;
    private int dist;

    public Object tooltip(Coord c, Widget wdg) {
	if ((this.lc != null) && (this.lc.dist(c) < 20.0D) && tc != null) {
	    if (tooltip instanceof Text.Line || tooltip instanceof KeyboundTip) {
		final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
		if (me != null) {
		    final int cdist = (int) (Math.ceil(me.rc.dist(tc) / 11.0));
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
			final String base = tooltip instanceof Text.Line ? ((Text.Line) tooltip).text : ((KeyboundTip)tooltip).base;
			return tt = Text.render(base + " - Distance: " + dist + extra);
		    } else {
			return tt;
		    }
		}
	    } else {
		return this.tooltip;
	    }
	}
	return null;
    }
}
