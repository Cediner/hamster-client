package hamster.ui.map;

import hamster.GlobalSettings;
import haven.*;
import haven.render.Location;
import haven.render.Pipe;
import haven.render.Projection;
import haven.render.RenderTree;

import java.awt.*;

public class Preview extends PView {
    public abstract class Camera implements Pipe.Op {
	protected haven.render.Camera view = new haven.render.Camera(Matrix4f.identity());
	protected Projection proj = new Projection(Matrix4f.identity());

	public Camera() {
	    resized();
	}

	public boolean click(Coord sc) {
	    return(false);
	}
	public void drag(Coord sc) {}
	public void release() {}
	public boolean wheel(Coord sc, int amount) {
	    return(false);
	}

	public void resized() {
	    float field = 0.5f;
	    float aspect = ((float)sz.y) / ((float)sz.x);
	    proj = new Projection(Projection.makefrustum(new Matrix4f(), -field, field,
		    -aspect * field, aspect * field,
		    1, ui != null ? GlobalSettings.CAMERAPROJFAR.get() : 10000));
	}

	public void apply(Pipe p) {
	    proj.apply(p);
	    view.apply(p);
	}

	public abstract float angle();
	public abstract void tick(double dt);

	public String stats() {return("N/A");}
    }
    public class FreeCam extends Camera {
	private float dist = 50.0f, tdist = dist;
	private float elev = (float)Math.PI / 4.0f, telev = elev;
	private float angl = 0.0f, tangl = angl;
	private Coord dragorig = null;
	private float elevorig, anglorig;
	private Coord3f cc = null;

	private long lastwh = 0;
	private float whz;

	public void tick(double dt) {
	    float cf = (1f - (float)Math.pow(500, -dt * 3));
	    angl = angl + ((tangl - angl) * cf);
	    float pi2 = (float) (Math.PI * 2);
	    while(angl > pi2) {angl -= pi2; tangl -= pi2; anglorig -= pi2;}
	    while(angl < 0)   {angl += pi2; tangl += pi2; anglorig += pi2;}
	    if(Math.abs(tangl - angl) < 0.0001) angl = tangl;

	    elev = elev + ((telev - elev) * cf);
	    if(Math.abs(telev - elev) < 0.0001) elev = telev;

	    dist = dist + ((tdist - dist) * cf);
	    if(Math.abs(tdist - dist) < 0.0001) dist = tdist;

	    Coord3f mc = getcc();
	    mc.y = -mc.y;
	    if((cc == null) || (Math.hypot(mc.x - cc.x, mc.y - cc.y) > 250))
		cc = mc;
	    else
		cc = cc.add(mc.sub(cc).mul(cf));
	    view = new haven.render.Camera(PointedCam.compute(cc.add(0.0f, 0.0f, 15f), dist, elev, angl));
	}

	public float angle() {
	    return(angl);
	}

	public boolean click(Coord c) {
	    elevorig = elev;
	    anglorig = angl;
	    dragorig = c;
	    return(true);
	}

	public void drag(Coord c) {
	    if (GlobalSettings.FREECAMREXAXIS.get())
		c = new Coord(c.x + (dragorig.x - c.x) * 2, c.y);
	    if (GlobalSettings.FREECAMREYAXIS.get())
		c = new Coord(c.x, c.y + (dragorig.y - c.y) * 2);
	    if (ui.modshift || !GlobalSettings.FREECAMLOCKELAV.get()) {
		telev = elevorig - ((float) (c.y - dragorig.y) / 100.0f);
		if (telev < 0.0f) telev = 0.0f;
		if (telev > (Math.PI / 2.0)) telev = (float) Math.PI / 2.0f;
	    }
	    tangl = anglorig + ((float) (c.x - dragorig.x) / 100.0f);
	}

	public boolean wheel(Coord c, int amount) {
	    if (whz < 0 && amount > 0)
		whz = 0;
	    else if (whz > 0 && amount < 0)
		whz = 0;
	    else if ((System.currentTimeMillis() - lastwh) < 1000)
		whz += amount * 5;
	    else
		whz = amount * 5;
	    lastwh = System.currentTimeMillis();

	    float d = tdist + whz;
	    if (d < 20)
		d = 20;
	    tdist = d;
	    return(true);
	}
    }
    public class OrthoCam extends Camera {
	public boolean exact;
	protected float dist = 500.0f;
	protected float elev = (float)Math.PI / 6.0f;
	protected float angl = -(float)Math.PI / 4.0f;
	protected float field = (float)(20 * Math.sqrt(2));
	private Coord dragorig = null;
	private float anglorig;
	protected Coord3f cc, jc;

	public OrthoCam(boolean exact) {
	    this.exact = exact;
	}

	public void tick2(double dt) {
	    Coord3f cc = getcc();
	    cc.y = -cc.y;
	    this.cc = cc;
	}

	public void tick(double dt) {
	    tick2(dt);
	    float aspect = ((float)sz.y) / ((float)sz.x);
	    Matrix4f vm = PointedCam.compute(cc.add(0.0f, 0.0f, 15f), dist, elev, angl);
	    if(exact) {
		if(jc == null)
		    jc = cc;
		float pfac = rsz().x / (field * 2);
		Coord3f vjc = vm.mul4(jc).mul(pfac);
		Coord3f corr = new Coord3f(Math.round(vjc.x) - vjc.x, Math.round(vjc.y) - vjc.y, 0).div(pfac);
		if((Math.abs(vjc.x) > 500) || (Math.abs(vjc.y) > 500))
		    jc = null;
		vm = Location.makexlate(new Matrix4f(), corr).mul1(vm);
	    }
	    view = new haven.render.Camera(vm);
	    proj = new Projection(Projection.makeortho(new Matrix4f(), -field, field, -field * aspect, field * aspect, 1, 5000));
	}

	public float angle() {
	    return(angl);
	}

	public boolean click(Coord c) {
	    anglorig = angl;
	    dragorig = c;
	    return(true);
	}

	public void drag(Coord c) {
	    angl = anglorig + ((float)(c.x - dragorig.x) / 100.0f);
	    angl = angl % ((float)Math.PI * 2.0f);
	}

	public String stats() {
	    return(String.format("%.1f %.2f %.2f %.1f", dist, elev / Math.PI, angl / Math.PI, field));
	}
    }
    public class SOrthoCam extends OrthoCam {
	private Coord dragorig = null;
	private float anglorig;
	private float tangl = angl;
	private float tfield = field;
	private final boolean isometric;

	private long lastwh = 0;
	private float whz;

	public SOrthoCam(boolean exact, boolean lock) {
	    super(exact);
	    this.isometric = lock;
	}

	public void tick2(double dt) {
	    double tf = 1.0;
	    dt *= tf;
	    float cf = 1f - (float)Math.pow(500, -dt);
	    Coord3f mc = getcc();
	    mc.y = -mc.y;
	    if((cc == null) || (Math.hypot(mc.x - cc.x, mc.y - cc.y) > 250))
		cc = mc;
	    else if(!exact || (mc.dist(cc) > 2))
		cc = cc.add(mc.sub(cc).mul(cf));

	    angl = angl + ((tangl - angl) * cf);
	    float pi2 = (float) (Math.PI * 2);
	    while(angl > pi2) {angl -= pi2; tangl -= pi2; anglorig -= pi2;}
	    while(angl < 0)   {angl += pi2; tangl += pi2; anglorig += pi2;}
	    if(Math.abs(tangl - angl) < 0.001)
		angl = tangl;
	    else
		jc = cc;

	    field = field + ((tfield - field) * cf);
	    if(Math.abs(tfield - field) < 0.1)
		field = tfield;
	    else
		jc = cc;
	}

	public boolean click(Coord c) {
	    anglorig = angl;
	    dragorig = c;
	    return(true);
	}

	public void drag(Coord c) {
	    tangl = anglorig + ((float)(c.x - dragorig.x) / 100.0f);
	}

	public void release() {
	    if(isometric && (tfield > 100))
		tangl = (float)(Math.PI * 0.5 * (Math.floor(tangl / (Math.PI * 0.5)) + 0.5));
	}

	private void chfield(float nf) {
	    tfield = nf;
	    tfield = Math.max(tfield, 0);
	    if(tfield > 100)
		release();
	}

	public boolean wheel(Coord c, int amount) {
	    if (whz < 0 && amount > 0)
		whz = 0;
	    else if (whz > 0 && amount < 0)
		whz = 0;
	    else if ((System.currentTimeMillis() - lastwh) < 1000)
		whz += amount * 5;
	    else
		whz = amount * 5;
	    lastwh = System.currentTimeMillis();

	    chfield(tfield + whz);
	    return (true);
	}
    }

    private final Gob gob;
    private final Camera cam;
    private UI.Grab camdrag = null;

    public final Outlines outlines = new Outlines(GlobalSettings.SYMMETRICOUTLINES);
    public Preview(final Gob g) {
	super(UI.scale(200, 200));
	gob = g;
	cam = new SOrthoCam(true, true);
	basic.add(outlines);
	basic.add(g);
    }

    public DirLight amblight = null;
    private RenderTree.Slot s_amblight = null;
    private void amblight() {
	if(amblight == null) {
	    final Color lamb = Color.WHITE;
	    final Color ldif = Color.WHITE;
	    final Color lspc = Color.WHITE;

	    amblight = new DirLight(lamb, ldif, lspc,
		    Coord3f.o.sadd((float) Math.PI / 2, (float) Math.PI / 2, 1f));
	    amblight.prio(100);
	}

	if(s_amblight != null) {
	    s_amblight.remove();
	    s_amblight = null;
	}
	if(amblight != null)
	    s_amblight = basic.add(amblight);
    }

    public Coord3f getcc() {
	return gob.getc().sub(0, 0, 10);
    }

    @Override
    public void draw(GOut g) {
	gob.gtick(g.out);
	super.draw(g);
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	cam.tick(dt);
	amblight();
	gob.ctick(dt);
	basic(Camera.class, cam);
    }

    @Override
    public void dispose() {
	gob.dispose();
    }

    public boolean mousedown(Coord c, int button) {
	parent.setfocus(this);
	if (button == 3) {
	    if (cam.click(c)) {
		camdrag = ui.grabmouse(this);
	    }
	    return true;
	} else {
	    return super.mousedown(c, button);
	}
    }

    public void mousemove(Coord c) {
	if (camdrag != null) {
	    cam.drag(c);
	} else {
	    super.mousemove(c);
	}
    }

    public boolean mouseup(Coord c, int button) {
	if (button == 3) {
	    if (camdrag != null) {
		cam.release();
		camdrag.remove();
		camdrag = null;
	    }
	    return true;
	} else {
	    return super.mouseup(c, button);
	}
    }

    public boolean mousewheel(Coord c, int amount) {
	return cam.wheel(c, amount);
    }
}
