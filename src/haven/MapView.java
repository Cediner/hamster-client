/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import static hamster.KeyBind.*;
import static hamster.MouseBind.*;
import static haven.MCache.cmaps;
import static haven.MCache.tilesz;
import static haven.OCache.posres;
import java.awt.Color;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.*;
import java.lang.ref.*;
import java.lang.reflect.*;

import hamster.GlobalSettings;
import hamster.KeyBind;
import hamster.MouseBind;
import hamster.gob.Hidden;
import hamster.script.pathfinding.Move;
import hamster.script.pathfinding.NBAPathfinder;
import haven.render.*;
import haven.MCache.OverlayInfo;
import haven.render.sl.Uniform;
import haven.render.sl.Type;

public class MapView extends PView implements DTarget, Console.Directory {
    public static boolean clickdb = false;
    public long plgob = -1;
    public long rlplgob = -1;
    public Coord2d cc;
    private final Glob glob;
    private int view = 2;
    private Collection<Delayed> delayed = new LinkedList<Delayed>();
    private Collection<Delayed> delayed2 = new LinkedList<Delayed>();
    public Camera camera;
    private Loader.Future<Plob> placing = null;
    private Grabber grab;
    private Selector selection;
    private Coord3f camoff = new Coord3f(Coord3f.o);
    public double shake = 0.0;
    public static int plobgran = Utils.getprefi("plobgran", 8);
    private static final Map<String, Class<? extends Camera>> camtypes = new HashMap<String, Class<? extends Camera>>();

    //Queued Movement
    private Move movingto;
    private Coord2d lastrc;
    private double mspeed, totaldist = 0, mspeedavg, totaldt = 0;
    private long lastMove = System.currentTimeMillis();
    public final Queue<Move> movequeue = new ArrayDeque<>();
    //Tooltip info
    private String lasttt = "";
    private Object tt;

    
    public interface Delayed {
	public void run(GOut g);
    }

    public interface Grabber {
	boolean mmousedown(Coord mc, int button);
	boolean mmouseup(Coord mc, int button);
	boolean mmousewheel(Coord mc, int amount);
	void mmousemove(Coord mc);
    }

    public abstract class Camera implements Pipe.Op {
	protected haven.render.Camera view = new haven.render.Camera(Matrix4f.identity());
	protected Projection proj = new Projection(Matrix4f.identity());
	private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
	
	public Camera() {
	    resized();

	    binds.put(KeyBind.KB_CAM_IN, this::zoomin);
	    binds.put(KeyBind.KB_CAM_OUT, this::zoomout);
	    binds.put(KeyBind.KB_CAM_LEFT, this::turnleft);
	    binds.put(KeyBind.KB_CAM_RIGHT, this::turnright);
	    binds.put(KeyBind.KB_CAM_RESET, this::reset);
	    binds.put(KeyBind.KB_RECENTER_CAMERA, this::center);
	}

	protected boolean zoomin() { return false; }
	protected boolean zoomout() { return false; }
	protected boolean turnleft() { return false; }
	protected boolean turnright() { return false; }
	protected boolean reset() { return false; }
	protected boolean center() { return false; }

	public boolean keydown(KeyEvent ev) {
	    final String bind = KeyBind.generateSequence(ev, ui);
	    for(final var kb : binds.keySet()) {
		if(kb.check(bind, binds.get(kb)))
		    return true;
	    }
	    return(false);
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
	    proj = new Projection(Projection.makefrustum(new Matrix4f(), -field, field, -aspect * field, aspect * field, 1, 5000));
	}

	public void apply(Pipe p) {
	    proj.apply(p);
	    view.apply(p);
	}
	
	public abstract float angle();
	public abstract void tick(double dt);

	public String stats() {return("N/A");}
    }
    
    public class FollowCam extends Camera {
	private final float fr = 0.0f, h = 10.0f;
	private float ca, cd;
	private Coord3f curc = null;
	private float elev, telev;
	private float angl, tangl;
	private Coord dragorig = null;
	private float anglorig;
	
	public FollowCam() {
	    elev = telev = (float)Math.PI / 6.0f;
	    angl = tangl = 0.0f;
	}
	
	public void resized() {
	    ca = (float)sz.y / (float)sz.x;
	    cd = 400.0f * ca;
	}
	
	public boolean click(Coord c) {
	    anglorig = tangl;
	    dragorig = c;
	    return(true);
	}
	
	public void drag(Coord c) {
	    tangl = anglorig + ((float)(c.x - dragorig.x) / 100.0f);
	    tangl = tangl % ((float)Math.PI * 2.0f);
	}

	private double f0 = 0.2, f1 = 0.5, f2 = 0.9;
	private double fl = Math.sqrt(2);
	private double fa = ((fl * (f1 - f0)) - (f2 - f0)) / (fl - 2);
	private double fb = ((f2 - f0) - (2 * (f1 - f0))) / (fl - 2);
	private float field(float elev) {
	    double a = elev / (Math.PI / 4);
	    return((float)(f0 + (fa * a) + (fb * Math.sqrt(a))));
	}

	private float dist(float elev) {
	    float da = (float)Math.atan(ca * field(elev));
	    return((float)(((cd - (h / Math.tan(elev))) * Math.sin(elev - da) / Math.sin(da)) - (h / Math.sin(elev))));
	}

	public void tick(double dt) {
	    elev += (telev - elev) * (float)(1.0 - Math.pow(500, -dt));
	    if(Math.abs(telev - elev) < 0.0001)
		elev = telev;
	    
	    float dangl = tangl - angl;
	    while(dangl >  Math.PI) dangl -= (float)(2 * Math.PI);
	    while(dangl < -Math.PI) dangl += (float)(2 * Math.PI);
	    angl += dangl * (float)(1.0 - Math.pow(500, -dt));
	    if(Math.abs(tangl - angl) < 0.0001)
		angl = tangl;
	    
	    Coord3f cc = getcc();
	    cc.y = -cc.y;
	    if(curc == null)
		curc = cc;
	    float dx = cc.x - curc.x, dy = cc.y - curc.y;
	    float dist = (float)Math.sqrt((dx * dx) + (dy * dy));
	    if(dist > 250) {
		curc = cc;
	    } else if(dist > fr) {
		Coord3f oc = curc;
		float pd = (float)Math.cos(elev) * dist(elev);
		Coord3f cambase = new Coord3f(curc.x + ((float)Math.cos(tangl) * pd), curc.y + ((float)Math.sin(tangl) * pd), 0.0f);
		float a = cc.xyangle(curc);
		float nx = cc.x + ((float)Math.cos(a) * fr), ny = cc.y + ((float)Math.sin(a) * fr);
		Coord3f tgtc = new Coord3f(nx, ny, cc.z);
		curc = curc.add(tgtc.sub(curc).mul((float)(1.0 - Math.pow(500, -dt))));
		if(curc.dist(tgtc) < 0.01)
		    curc = tgtc;
		tangl = curc.xyangle(cambase);
	    }
	    
	    float field = field(elev);
	    view = new haven.render.Camera(PointedCam.compute(curc.add(camoff).add(0.0f, 0.0f, h), dist(elev), elev, angl));
	    proj = new Projection(Projection.makefrustum(new Matrix4f(), -field, field, -ca * field, ca * field, 1, 5000));
	}

	public float angle() {
	    return(angl);
	}
	
	private static final float maxang = (float)(Math.PI / 2 - 0.1);
	private static final float mindist = 50.0f;
	public boolean wheel(Coord c, int amount) {
	    float fe = telev;
	    telev += amount * telev * 0.02f;
	    if(telev > maxang)
		telev = maxang;
	    if(dist(telev) < mindist)
		telev = fe;
	    return(true);
	}

	public String stats() {
	    return(String.format("%f %f %f", elev, dist(elev), field(elev)));
	}
    }
    static {camtypes.put("follow", FollowCam.class);}

    public class SimpleCam extends Camera {
	private float dist = 50.0f;
	private float elev = (float)Math.PI / 4.0f;
	private float angl = 0.0f;
	private Coord dragorig = null;
	private float elevorig, anglorig;

	public void tick(double dt) {
	    Coord3f cc = getcenter();
	    cc.y = -cc.y;
	    view = new haven.render.Camera(PointedCam.compute(cc.add(camoff).add(0.0f, 0.0f, 15f), dist, elev, angl));
	}

	public Coord3f getcenter() {
	    return getcc();
	}
	
	public float angle() {
	    return(angl);
	}

	public void setDist(final float d) {
	    this.dist = d;
	}
	
	public boolean click(Coord c) {
	    elevorig = elev;
	    anglorig = angl;
	    dragorig = c;
	    return(true);
	}
	
	public void drag(Coord c) {
	    elev = elevorig - ((float)(c.y - dragorig.y) / 100.0f);
	    if(elev < 0.0f) elev = 0.0f;
	    if(elev > (Math.PI / 2.0)) elev = (float)Math.PI / 2.0f;
	    angl = anglorig + ((float)(c.x - dragorig.x) / 100.0f);
	    angl = angl % ((float)Math.PI * 2.0f);
	}

	public boolean wheel(Coord c, int amount) {
	    float d = dist + (amount * 25);
	    if(d < 5)
		d = 5;
	    dist = d;
	    return(true);
	}
    }
    static {camtypes.put("worse", SimpleCam.class);}

    public class FreeCam extends Camera {
	private float dist = 50.0f, tdist = dist;
	private float elev = (float)Math.PI / 4.0f, telev = elev;
	private float angl = 0.0f, tangl = angl;
	private Coord dragorig = null;
	private float elevorig, anglorig;
	private final float pi2 = (float)(Math.PI * 2);
	private Coord3f cc = null;

	public void tick(double dt) {
	    float cf = (1f - (float)Math.pow(500, -dt * 3));
	    angl = angl + ((tangl - angl) * cf);
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
	    telev = elevorig - ((float)(c.y - dragorig.y) / 100.0f);
	    if(telev < 0.0f) telev = 0.0f;
	    if(telev > (Math.PI / 2.0)) telev = (float)Math.PI / 2.0f;
	    tangl = anglorig + ((float)(c.x - dragorig.x) / 100.0f);
	}

	public boolean wheel(Coord c, int amount) {
	    float d = tdist + (amount * 25);
	    if(d < 5)
		d = 5;
	    tdist = d;
	    return(true);
	}
    }
    static {camtypes.put("bad", FreeCam.class);}

    public class Fixator extends SimpleCam {
	private Coord3f offset = new Coord3f(0, 0, 0);
	private Coord doff;

	@Override
	public Coord3f getcenter() {
	    return getcc().add(offset);
	}

	public boolean reset() {
	    offset = new Coord3f(0, 0, 0);
	    return true;
	}

	@Override
	public boolean click(Coord c) {
	    doff = c;
	    return super.click(c);
	}

	public void drag(final Coord c) {
	    if (ui.modctrl) {
		offset = offset.add(new Coord3f(c.add(doff.inv())).rotate(-angle() + (float) (Math.PI / 2)));
		doff = c;
	    } else {
		super.drag(c);
	    }
	}
    }
    static {
	camtypes.put("fixator", Fixator.class);
    }

    public class FreeStyle extends SimpleCam {
	private Coord3f plcc = null;
	private Coord3f focus = null;
	private Coord doff;

	public FreeStyle() {
	    setDist(250f);
	}

	@Override
	public void tick(double dt) {
	    super.tick(dt);
	    final Coord3f nplcc = getcc();
	    if (Math.abs(nplcc.dist(plcc)) > (30 * 11)) {
		reset();
	    }
	    plcc = nplcc;
	}

	@Override
	public Coord3f getcenter() {
	    if (focus == null) {
		focus = plcc = getcc();
	    }
	    return new Coord3f(focus);
	}

	public boolean reset() {
	    focus = getcc();
	    return true;
	}

	@Override
	public boolean click(Coord c) {
	    doff = c;
	    return super.click(c);
	}

	public void drag(final Coord c) {
	    if (ui.modflags() == 0) {
		focus = focus.add(new Coord3f(c.add(doff.inv())).rotate(-angle() + (float) (Math.PI / 2)));
		doff = c;
	    } else {
		super.drag(c);
	    }
	}
    }
    static {
	camtypes.put("freestyle", FreeStyle.class);
    }

    public class TopDownCam extends Camera {
	private Coord3f cc;
	private final float dist = 500.0f;
	private final float elev = (float) Math.toRadians(90);
	protected float field = (float) (100 * Math.sqrt(2));
	private float tfield = field;
	private Coord dragorig = null;
	private float angl = 0.0f;
	private float tangl = angl;
	private float anglorig;

	private long lastwh = 0;
	private float whz;

	public TopDownCam() {
	}

	public void tick2(double dt) {
	    Coord3f cc = getcc();
	    cc.y = -cc.y;
	    this.cc = cc;
	}

	public void tick(double dt) {
	    tick2(dt);
	    float aspect = ((float) sz.y) / ((float) sz.x);

	    //Smooth transition for angle
	    angl = angl + ((tangl - angl) * (1f - (float) Math.pow(500, -dt)));
	    float pi2 = (float) (Math.PI * 2);
	    while (angl > pi2) {
		angl -= pi2;
		tangl -= pi2;
		anglorig -= pi2;
	    }
	    while (angl < 0) {
		angl += pi2;
		tangl += pi2;
		anglorig += pi2;
	    }
	    if (Math.abs(tangl - angl) < 0.001)
		angl = tangl;

	    //Smooth transition for zoom in/out
	    field = field + ((tfield - field) * (1f - (float) Math.pow(500, -dt)));
	    if (Math.abs(tfield - field) < 0.1)
		field = tfield;

	    view = new haven.render.Camera(PointedCam.compute(cc.add(camoff).add(0.0f, 0.0f, 15f), dist, elev, angl));
	    proj = new Projection(Projection.makeortho(new Matrix4f(), -field, field, -field * aspect, field * aspect, 1, 5000));
	}

	public float angle() {
	    return (angl);
	}

	public boolean click(Coord c) {
	    anglorig = angl;
	    dragorig = c;
	    return (true);
	}

	public void drag(Coord c) {
	    tangl = anglorig + ((float) (c.x - dragorig.x) / 100.0f);
	}

	public void release() {
	    tangl = (float) (Math.floor((tangl + Math.PI / 4) / (Math.PI / 2)) * Math.PI / 2);
	}

	private void chfield(float nf) {
	    tfield = nf;
	    tfield = Math.max(tfield, 50);
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

	public String toString() {
	    return (String.format("%f", dist));
	}
    }

    static {
	camtypes.put("topdown", TopDownCam.class);
    }
    
    public class OrthoCam extends Camera {
	public boolean exact = true;
	protected float dist = 500.0f;
	protected float elev = (float)Math.PI / 6.0f;
	protected float angl = -(float)Math.PI / 4.0f;
	protected float field = (float)(100 * Math.sqrt(2));
	private Coord dragorig = null;
	private float anglorig;
	protected Coord3f cc, jc;

	public OrthoCam(boolean exact) {
	    this.exact = exact;
	}

	public OrthoCam() {this(false);}

	public void tick2(double dt) {
	    Coord3f cc = getcc();
	    cc.y = -cc.y;
	    this.cc = cc;
	}

	public void tick(double dt) {
	    tick2(dt);
	    float aspect = ((float)sz.y) / ((float)sz.x);
	    Matrix4f vm = PointedCam.compute(cc.add(camoff).add(0.0f, 0.0f, 15f), dist, elev, angl);
	    if(exact) {
		if(jc == null)
		    jc = cc;
		float pfac = rsz.x / (field * 2);
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
	private boolean isometric = true;
	private final float pi2 = (float)(Math.PI * 2);
	private boolean lock = true;

	private long lastwh = 0;
	private float whz;

	public SOrthoCam(boolean exact, boolean lock) {
	    super(exact);
	    this.isometric = lock;
	}

	public SOrthoCam(String... args) {
	    PosixArgs opt = PosixArgs.getopt(args, "enif");
	    for(char c : opt.parsed()) {
		switch(c) {
		case 'e':
		    exact = true;
		    break;
		case 'n':
		    exact = false;
		    break;
		case 'i':
		    isometric = true;
		    break;
		case 'f':
		    isometric = false;
		    break;
		}
	    }
	}

	public void tick2(double dt) {
	    float cf = 1f - (float)Math.pow(500, -dt);
	    Coord3f mc = getcc();
	    mc.y = -mc.y;
	    if((cc == null) || (Math.hypot(mc.x - cc.x, mc.y - cc.y) > 250))
		cc = mc;
	    else if(!exact || (mc.dist(cc) > 2))
		cc = cc.add(mc.sub(cc).mul(cf));

	    angl = angl + ((tangl - angl) * cf);
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
	    tfield = Math.max(Math.min(tfield, sz.x * (float)Math.sqrt(2) / 8f), 50);
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

	@Override
	protected boolean zoomin() {
	    chfield(tfield - 50);
	    return(true);
	}

	@Override
	protected boolean zoomout() {
	    chfield(tfield + 50);
	    return(true);
	}

	@Override
	protected boolean turnleft() {
	    tangl = (float)(Math.PI * 0.5 * (Math.floor((tangl / (Math.PI * 0.5)) - 0.51) + 0.5));
	    return(true);
	}

	@Override
	protected boolean turnright() {
	    tangl = (float)(Math.PI * 0.5 * (Math.floor((tangl / (Math.PI * 0.5)) + 0.51) + 0.5));
	    return(true);
	}

	@Override
	protected boolean reset() {
	    tangl = angl + (float)Utils.cangle(-(float)Math.PI * 0.25f - angl);
	    chfield((float)(100 * Math.sqrt(2)));
	    return(true);
	}
    }
    static {camtypes.put("ortho", SOrthoCam.class);}

    @RName("mapview")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    Coord sz = UI.scale((Coord)args[0]);
	    Coord2d mc = ((Coord)args[1]).mul(posres);
	    long pgob = -1;
	    if(args.length > 2)
		pgob = Utils.uint32((Integer)args[2]);
	    return(new MapView(sz, ui.sess.glob, mc, pgob));
	}
    }
    
    public MapView(Coord sz, Glob glob, Coord2d cc, long plgob) {
	super(sz);
	this.glob = glob;
	this.cc = cc;
	this.plgob = this.rlplgob = plgob;
	this.camera = restorecam();
	basic.add(new Outlines(GlobalSettings.SYMMETRICOUTLINES));
	basic.add(this.gobs = new Gobs());
	basic.add(this.terrain = new Terrain());
	this.clickmap = new ClickMap();
	clmaptree.add(clickmap);
	setcanfocus(true);
	setupKeyBinds();
    }
    
    protected void envdispose() {
	if(smap != null) {
	    smap.dispose(); smap = null;
	    slist.dispose(); slist = null;
	}
	super.envdispose();
    }

    public void dispose() {
	gobs.slot.remove();
	clmaplist.dispose();
	clobjlist.dispose();
	super.dispose();
    }

    public boolean visol(String tag) {
	synchronized(oltags) {
	    return(oltags.containsKey(tag));
	}
    }

    public void enol(String tag) {
	synchronized(oltags) {
	    oltags.put(tag, oltags.getOrDefault(tag, 0) + 1);
	}
    }

    public void disol(String tag) {
	synchronized(oltags) {
	    Integer rc = oltags.get(tag);
	    if((rc != null) && (--rc > 0))
		oltags.put(tag, rc);
	    else
		oltags.remove(tag);;
	}
    }

    @Deprecated private String oltag(int id) {
	switch(id) {
	case 0: case 1:
	    return("cplot");
	case 2: case 3:
	    return("vlg");
	case 4: case 5:
	    return("realm");
	case 16:
	    return("cplot-s");
	case 17:
	    return("sel");
	}
	return("n/a");
    }
    @Deprecated public void enol(int id) {
	enol(oltag(id));
    }
    @Deprecated public void disol(int id) {
	disol(oltag(id));
    }

    private final Gobs gobs;
    private class Gobs implements RenderTree.Node, OCache.ChangeCallback {
	final OCache oc = glob.oc;
	final Map<Gob, Loader.Future<?>> adding = new HashMap<>();
	final Map<Gob, RenderTree.Slot> current = new HashMap<>();
	RenderTree.Slot slot;

	private void addgob(Gob ob) {
	    RenderTree.Slot slot = this.slot;
	    if(slot == null)
		return;
	    synchronized(ob) {
		synchronized(this) {
		    if(!adding.containsKey(ob))
			return;
		}
		RenderTree.Slot nslot;
		try {
		    nslot = slot.add(ob.placed);
		} catch(RenderTree.SlotRemoved e) {
		    /* Ignore here as there is a harmless remove-race
		     * on disposal. */
		    return;
		}
		synchronized(this) {
		    if(adding.remove(ob) != null)
			current.put(ob, nslot);
		    else
			nslot.remove();
		}
	    }
	}

	public void added(RenderTree.Slot slot) {
	    synchronized(this) {
		if(this.slot != null)
		    throw(new RuntimeException());
		this.slot = slot;
		synchronized(oc) {
		    for(Gob ob : oc)
			adding.put(ob, glob.loader.defer(() -> addgob(ob), null));
		    oc.callback(this);
		}
	    }
	}

	public void removed(RenderTree.Slot slot) {
	    synchronized(this) {
		if(this.slot != slot)
		    throw(new RuntimeException());
		this.slot = null;
		oc.uncallback(this);
		Collection<Loader.Future<?>> tasks = new ArrayList<>(adding.values());
		adding.clear();
		for(Loader.Future<?> task : tasks)
		    task.restart();
		current.clear();
	    }
	}

	public void added(Gob ob) {
	    synchronized(this) {
		if(current.containsKey(ob))
		    throw(new RuntimeException());
		adding.put(ob, glob.loader.defer(() -> addgob(ob), null));
	    }
	}

	public void removed(Gob ob) {
	    RenderTree.Slot slot;
	    synchronized(this) {
		slot = current.remove(ob);
		if(slot == null) {
		    Loader.Future<?> t = adding.remove(ob);
		    if(t != null)
			t.restart();
		}
	    }
	    if(slot != null) {
		try {
		    slot.remove();
		} catch(RenderTree.SlotRemoved e) {
		    /* Ignore here as there is a harmless remove-race
		     * on disposal. */
		}
	    }
	}
    }

    private class MapRaster extends RenderTree.Node.Track1 {
	final MCache map = glob.map;
	Area area;
	Loading lastload = new Loading("Initializing map...");

	abstract class Grid<T> extends RenderTree.Node.Track1 {
	    final Map<Coord, Pair<T, RenderTree.Slot>> cuts = new HashMap<>();
	    final boolean position;
	    Loading lastload = new Loading("Initializing map...");

	    Grid(boolean position) {
		this.position = position;
	    }

	    Grid() {this(true);}

	    abstract T getcut(Coord cc);
	    RenderTree.Node produce(T cut) {return((RenderTree.Node)cut);}

	    void tick() {
		if(slot == null)
		    return;
		Loading curload = null;
		for(Coord cc : area) {
		    try {
			T cut = getcut(cc);
			Pair<T, RenderTree.Slot> cur = cuts.get(cc);
			if((cur != null) && (cur.a != cut)) {
			    /* XXX: It is currently important that invalidated
			     * cuts are removed immediately (since they are
			     * disposed in MCache in thus not drawable
			     * anymore). This is not currently a problem, but
			     * conflicts with the below stated goal of
			     * asynchronizing mapraster ticking. */
			    cur.b.remove();
			    cuts.remove(cc);
			    cur = null;
			}
			if(cur == null) {
			    Coord2d pc = cc.mul(MCache.cutsz).mul(tilesz);
			    RenderTree.Node draw = produce(cut);
			    Pipe.Op cs = null;
			    if(position)
				cs = Location.xlate(new Coord3f((float)pc.x, -(float)pc.y, 0));
			    cuts.put(cc, new Pair<>(cut, slot.add(draw, cs)));
			}
		    } catch(Loading l) {
			curload = l;
		    }
		}
		this.lastload = curload;
		for(Iterator<Map.Entry<Coord, Pair<T, RenderTree.Slot>>> i = cuts.entrySet().iterator(); i.hasNext();) {
		    Map.Entry<Coord, Pair<T, RenderTree.Slot>> ent = i.next();
		    if(!area.contains(ent.getKey())) {
			ent.getValue().b.remove();
			i.remove();
		    }
		}
	    }

	    public void removed(RenderTree.Slot slot) {
		super.removed(slot);
		cuts.clear();
	    }
	}

	void tick() {
	    /* XXX: Should be taken out of the main rendering
	     * loop. Probably not a big deal, but still. */
	    try {
		Coord cc = new Coord2d(getcc()).floor(tilesz).div(MCache.cutsz);
		area = new Area(cc.sub(view, view), cc.add(view, view).add(1, 1));
		lastload = null;
	    } catch(Loading l) {
		lastload = l;
	    }
	}

	public Loading loading() {
	    if(this.lastload != null)
		return(this.lastload);
	    return(null);
	}
    }

    public final Terrain terrain;
    public class Terrain extends MapRaster {
	final Grid main = new Grid<MapMesh>() {
		MapMesh getcut(Coord cc) {
		    return(map.getcut(cc));
		}
	    };
	final Grid flavobjs = new Grid<RenderTree.Node>(false) {
		RenderTree.Node getcut(Coord cc) {
		    return(map.getfo(cc));
		}
	    };

	private Terrain() {
	}

	void tick() {
	    super.tick();
	    if(area != null) {
		main.tick();
		flavobjs.tick();
	    }
	}

	public void added(RenderTree.Slot slot) {
	    slot.add(main);
	    slot.add(flavobjs);
	    super.added(slot);
	}

	public Loading loading() {
	    Loading ret = super.loading();
	    if(ret != null)
		return(ret);
	    if((ret = main.lastload) != null)
		return(ret);
	    if((ret = flavobjs.lastload) != null)
		return(ret);
	    return(null);
	}
    }

    public class Overlay extends MapRaster {
	final OverlayInfo id;
	int rc = 0;
	boolean used;

	final Grid base = new Grid<RenderTree.Node>() {
		RenderTree.Node getcut(Coord cc) {
		    return(map.getolcut(id, cc));
		}
	    };
	final Grid outl = new Grid<RenderTree.Node>() {
		RenderTree.Node getcut(Coord cc) {
		    return(map.getololcut(id, cc));
		}
	    };

	private Overlay(OverlayInfo id) {
	    this.id = id;
	}

	void tick() {
	    super.tick();
	    if(area != null) {
		base.tick();
		outl.tick();
	    }
	}

	public void added(RenderTree.Slot slot) {
	    slot.add(base, id.mat());
	    Material omat = id.omat();
	    if(omat != null)
		slot.add(outl, omat);
	    super.added(slot);
	}

	public Loading loading() {
	    Loading ret = super.loading();
	    if(ret != null)
		return(ret);
	    if((ret = base.lastload) != null)
		return(ret);
	    return(null);
	}

	public void remove() {
	    slot.remove();
	}
    }

    private final Map<String, Integer> oltags = new HashMap<>();
    private final Map<OverlayInfo, Overlay> ols = new HashMap<>();
    {oltags.put("show", 1);}
    private void oltick() {
	try {
	    for(Overlay ol : ols.values())
		ol.used = false;
	    if(terrain.area != null) {
		for(OverlayInfo id : glob.map.getols(terrain.area.mul(MCache.cutsz))) {
		    boolean vis = false;
		    synchronized(oltags) {
			for(String tag : id.tags()) {
			    if(oltags.containsKey(tag)) {
				vis = true;
				break;
			    }
			}
		    }
		    if(vis) {
			Overlay ol = ols.get(id);
			if(ol == null) {
			    try {
				basic.add(ol = new Overlay(id));
				ols.put(id, ol);
			    } catch(Loading l) {
				continue;
			    }
			}
			ol.used = true;
		    }
		}
	    }
	    for(Iterator<Overlay> i = ols.values().iterator(); i.hasNext();) {
		Overlay ol = i.next();
		if(!ol.used) {
		    ol.remove();
		    i.remove();
		}
	    }
	} catch(Loading l) {
	}
	for(Overlay ol : ols.values())
	    ol.tick();
    }

    private static final Material gridmat = new Material(new BaseColor(255, 255, 255, 48), States.maskdepth, new MapMesh.OLOrder(null),
							 Location.xlate(new Coord3f(0, 0, 0.5f))   /* Apparently, there is no depth bias for lines. :P */
							 );
    private class GridLines extends MapRaster {
	final Grid grid = new Grid<RenderTree.Node>() {
		RenderTree.Node getcut(Coord cc) {
		    return(map.getcut(cc).grid());
		}
	    };

	private GridLines() {}

	void tick() {
	    super.tick();
	    if(area != null)
		grid.tick();
	}

	public void added(RenderTree.Slot slot) {
	    slot.ostate(gridmat);
	    slot.add(grid);
	    super.added(slot);
	}

	public void remove() {
	    slot.remove();
	}
    }

    GridLines gridlines = null;
    public void showgrid(boolean show) {
	if((gridlines == null) && show) {
	    basic.add(gridlines = new GridLines());
	} else if((gridlines != null) && !show) {
	    gridlines.remove();
	    gridlines = null;
	}
    }

    static class MapClick extends Clickable {
	final MapMesh cut;

	MapClick(MapMesh cut) {
	    this.cut = cut;
	}

	public String toString() {
	    return(String.format("#<mapclick %s>", cut));
	}
    }

    private final ClickMap clickmap;
    private class ClickMap extends MapRaster {
	final Grid grid = new Grid<MapMesh>() {
		MapMesh getcut(Coord cc) {
		    return(map.getcut(cc));
		}
		RenderTree.Node produce(MapMesh cut) {
		    return(new MapClick(cut).apply(cut.flat));
		}
	    };

	void tick() {
	    super.tick();
	    if(area != null) {
		grid.tick();
	    }
	}

	public void added(RenderTree.Slot slot) {
	    slot.add(grid);
	    super.added(slot);
	}

	public Loading loading() {
	    Loading ret = super.loading();
	    if(ret != null)
		return(ret);
	    if((ret = grid.lastload) != null)
		return(ret);
	    return(null);
	}
    }

    private final Material[] olmats;
    {
	olmats = new Material[32];
	olmats[0] = olmat(255, 0, 128, 32);
	olmats[1] = olmat(0, 0, 255, 32);
	olmats[2] = olmat(255, 0, 0, 32);
	olmats[3] = olmat(128, 0, 255, 32);
	olmats[4] = olmat(255, 255, 255, 32);
	olmats[5] = olmat(0, 255, 128, 32);
	olmats[6] = olmat(0, 0, 0, 64);
	olmats[16] = olmat(0, 255, 0, 32);
	olmats[17] = olmat(255, 255, 0, 32);
    }

    private Material olmat(int r, int g, int b, int a) {
	return(new Material(new BaseColor(r, g, b, a),
			    States.maskdepth));
    }

    public String camstats() {
	String cc;
	try {
	    Coord3f c = getcc();
	    cc = String.format("(%.1f %.1f %.1f)", c.x, c.y, c.z);
	} catch(Loading l) {
	    cc = "<nil>";
	}
	return(String.format("C: %s, Cam: %s", cc, camera.stats()));
    }

    public String stats() {
	String ret = String.format("Tree %s", tree.stats());
	if(back != null)
	    ret = String.format("%s, Inst %s, Draw %s", ret, instancer.stats(), back.stats());
	return(ret);
    }

    private Coord3f smapcc = null;
    private ShadowMap.ShadowList slist = null;
    private ShadowMap smap = null;
    private double lsmch = 0;
    public static final int[] shadowmap = {128, 256, 512, 1024, 2048, 4096, 8192, 16384};
    public static final int[] shadowsizemap = {100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000};
    public static final int[] shadowdepthmap = {100, 1000, 3000, 5000, 10000, 25000, 50000};
    public AtomicBoolean resetsmap = new AtomicBoolean(false);

    public void resetshadows() {
	resetsmap.set(true);
    }

    private void updsmap(DirLight light) {
	boolean usesdw = GlobalSettings.SHADOWS.get();
	if(usesdw) {
	    Coord3f dir, cc;
	    try {
		dir = new Coord3f(-light.dir[0], -light.dir[1], -light.dir[2]);
		cc = getcc();
	    } catch(Loading l) {
		return;
	    }


	    if (resetsmap.getAndSet(false)) {
		if (smap != null) {
		    instancer.remove(slist);
		    smap.dispose();
		    smap = null;
		    slist.dispose();
		    slist = null;
		    basic(ShadowMap.class, null);
		}
		smapcc = null;
	    }

	    if(smap == null) {
		if(instancer == null)
		    return;
		slist = new ShadowMap.ShadowList(instancer);
		smap = new ShadowMap(new Coord(shadowmap[GlobalSettings.SHADOWQUALITY.get()], shadowmap[GlobalSettings.SHADOWQUALITY.get()]),
			shadowsizemap[GlobalSettings.SHADOWSIZE.get()],
			shadowdepthmap[GlobalSettings.SHADOWDEPTH.get()], 1);
	    } else if(smap.lbuf.w != shadowmap[GlobalSettings.SHADOWQUALITY.get()]) {
		smap.dispose();
		smap = new ShadowMap(new Coord(shadowmap[GlobalSettings.SHADOWQUALITY.get()], shadowmap[GlobalSettings.SHADOWQUALITY.get()]),
			shadowsizemap[GlobalSettings.SHADOWSIZE.get()],
			shadowdepthmap[GlobalSettings.SHADOWDEPTH.get()], 1);
		smapcc = null;
		basic(ShadowMap.class, null);
	    }
	    smap = smap.light(light);
	    cc.y = -cc.y;
	    boolean ch = false;
	    double now = Utils.rtime();
	    if((smapcc == null) || (smapcc.dist(cc) > 50)) {
		smapcc = cc;
		ch = true;
	    } else {
		if(now - lsmch > 0.1)
		    ch = true;
	    }
	    if(ch || !smap.haspos()) {
		smap = smap.setpos(smapcc.add(dir.neg().mul(1000f)), dir);
		lsmch = now;
	    }
	    basic(ShadowMap.class, smap);
	} else {
	    if(smap != null) {
		instancer.remove(slist);
		smap.dispose(); smap = null;
		slist.dispose(); slist = null;
		basic(ShadowMap.class, null);
	    }
	    smapcc = null;
	}
    }

    private void drawsmap(Render out) {
	if(smap != null)
	    smap.update(out, slist);
    }

    public DirLight amblight = null;
    private RenderTree.Slot s_amblight = null;
    private void amblight() {
	synchronized(glob) {
	    if(glob.lightamb != null) {
		amblight = new DirLight(glob.lightamb, glob.lightdif, glob.lightspc, Coord3f.o.sadd((float)glob.lightelev, (float)glob.lightang, 1f));
		amblight.prio(100);
	    } else {
		amblight = null;
	    }
	}
	if(s_amblight != null) {
	    s_amblight.remove();
	    s_amblight = null;
	}
	if(amblight != null)
	    s_amblight = basic.add(amblight);
    }

    public static final Uniform amblight_idx = new Uniform(Type.INT, p -> {
	    DirLight light = ((MapView)((WidgetContext)p.get(RenderContext.slot)).widget()).amblight;
	    Light.LightList lights = p.get(Light.lights);
	    int idx = -1;
	    if(light != null)
		idx = lights.index(light);
	    return(idx);
	}, RenderContext.slot, Light.lights);

    public static final Uniform maploc = new Uniform(Type.VEC3, p -> {
	    Coord3f orig = Homo3D.locxf(p).mul4(Coord3f.o);
	    try {
		orig.z = p.get(RenderContext.slot).context(Glob.class).map.getcz(orig.x, -orig.y);
	    } catch(Loading l) {
		/* XXX: WaterTile's obfog effect is the only thing
		 * that uses maploc, in order to get the precise water
		 * surface level. Arguably, maploc should be
		 * eliminated entirely and the obfog should pass the
		 * water level in a uniform instead. However, this
		 * works better for now, because with such a mechanic,
		 * Skeleton.FxTrack audio sprites would never complete
		 * if they get outside the map and stuck as constantly
		 * loading and never playing. Either way, when
		 * loading, the likely quite slight deviation between
		 * origin-Z and map-Z level probably doesn't matter a
		 * whole lot, but solve pl0x. */
	    }
	    return(orig);
	}, Homo3D.loc, RenderContext.slot);

    private final Map<RenderTree.Node, RenderTree.Slot> rweather = new HashMap<>();
    private void updweather() {
	Glob.Weather[] wls = glob.weather().toArray(new Glob.Weather[0]);
	Pipe.Op[] wst = new Pipe.Op[wls.length];
	for(int i = 0; i < wls.length; i++)
	    wst[i] = wls[i].state();
	try {
	    basic(Glob.Weather.class, Pipe.Op.compose(wst));
	} catch(Loading l) {
	}
	Collection<RenderTree.Node> old =new ArrayList<>(rweather.keySet());
	for(Glob.Weather w : wls) {
	    if(w instanceof RenderTree.Node) {
		RenderTree.Node n = (RenderTree.Node)w;
		old.remove(n);
		if(rweather.get(n) == null) {
		    try {
			rweather.put(n, basic.add(n));
		    } catch(Loading l) {
		    }
		}
	    }
	}
	for(RenderTree.Node rem : old)
	    rweather.remove(rem).remove();
    }

    public RenderTree.Slot drawadd(RenderTree.Node extra) {
	return(basic.add(extra));
    }

    public Gob player() {
	return((plgob < 0) ? null : glob.oc.getgob(plgob));
    }
    
    public Coord3f getcc() {
	Gob pl = player();
	if(pl != null)
	    return(pl.getc());
	else
	    return(glob.map.getzp(cc));
    }

    public static class Clicklist implements RenderList<Rendered>, RenderList.Adapter {
	public static final Pipe.Op clickbasic = Pipe.Op.compose(new States.Depthtest(States.Depthtest.Test.LE),
								 new States.Facecull(),
								 Homo3D.state);
	private static final int MAXID = 0xffffff;
	private final RenderList.Adapter master;
	private final boolean doinst;
	private final ProxyPipe basic = new ProxyPipe();
	private final Map<Slot<? extends Rendered>, Clickslot> slots = new HashMap<>();
	private final Map<Integer, Clickslot> idmap = new HashMap<>();
	private DefPipe curbasic = null;
	private RenderList<Rendered> back;
	private DrawList draw;
	private InstanceList instancer;
	private int nextid = 1;

	public class Clickslot implements Slot<Rendered> {
	    public final Slot<? extends Rendered> bk;
	    public final int id;
	    final Pipe idp;
	    private GroupPipe state;

	    public Clickslot(Slot<? extends Rendered> bk, int id) {
		this.bk = bk;
		this.id = id;
		this.idp = new SinglePipe<>(FragID.id, new FragID.ID(id));
	    }

	    public Rendered obj() {
		return(bk.obj());
	    }

	    public GroupPipe state() {
		if(state == null)
		    state = new IDState(bk.state());
		return(state);
	    }

	    private class IDState implements GroupPipe {
		static final int idx_bas = 0, idx_idp = 1, idx_back = 2;
		final GroupPipe back;

		IDState(GroupPipe back) {
		    this.back = back;
		}

		public Pipe group(int idx) {
		    switch(idx) {
		    case idx_bas: return(basic);
		    case idx_idp: return(idp);
		    default: return(back.group(idx - idx_back));
		    }
		}

		public int gstate(int id) {
		    if(id == FragID.id.id)
			return(idx_idp);
		    if(State.Slot.byid(id).type == State.Slot.Type.GEOM) {
			int ret = back.gstate(id);
			if(ret >= 0)
			    return(ret + idx_back);
		    }
		    if((id < curbasic.mask.length) && curbasic.mask[id])
			return(idx_bas);
		    return(-1);
		}

		public int nstates() {
		    return(Math.max(Math.max(back.nstates(), curbasic.mask.length), FragID.id.id + 1));
		}
	    }
	}

	public Clicklist(RenderList.Adapter master, boolean doinst) {
	    this.master = master;
	    this.doinst = doinst;
	    asyncadd(this.master, Rendered.class);
	}

	public void add(Slot<? extends Rendered> slot) {
	    if(slot.state().get(Clickable.slot) == null)
		return;
	    int id;
	    while(idmap.get(id = nextid) != null) {
		if(++nextid > MAXID)
		    nextid = 1;
	    }
	    Clickslot ns = new Clickslot(slot, id);
	    if(back != null)
		back.add(ns);
	    if(((slots.put(slot, ns)) != null) || (idmap.put(id, ns) != null))
		throw(new AssertionError());
	}

	public void remove(Slot<? extends Rendered> slot) {
	    Clickslot cs = slots.remove(slot);
	    if(cs != null) {
		if(idmap.remove(cs.id) != cs)
		    throw(new AssertionError());
		if(back != null)
		    back.remove(cs);
	    }
	}

	public void update(Slot<? extends Rendered> slot) {
	    if(back != null) {
		Clickslot cs = slots.get(slot);
		if(cs != null) {
		    cs.state = null;
		    back.update(cs);
		}
	    }
	}

	public void update(Pipe group, int[] statemask) {
	    if(back != null)
		back.update(group, statemask);
	}

	public Locked lock() {
	    return(master.lock());
	}

	public Iterable<? extends Slot<?>> slots() {
	    return(slots.values());
	}

	/* Shouldn't have to care. */
	public <R> void add(RenderList<R> list, Class<? extends R> type) {}
	public void remove(RenderList<?> list) {}

	public void basic(Pipe.Op st) {
	    try(Locked lk = lock()) {
		DefPipe buf = new DefPipe();
		buf.prep(st);
		if(curbasic != null) {
		    if(curbasic.maskdiff(buf).length != 0)
			throw(new RuntimeException("changing clickbasic definition mask is not supported"));
		}
		int[] mask = basic.dupdate(buf);
		curbasic = buf;
		if(back != null)
		    back.update(basic, mask);
	    }
	}

	public Coord sz() {
	    return(basic.get(States.viewport).area.sz());
	}

	public void draw(Render out) {
	    if((draw == null) || !draw.compatible(out.env())) {
		if(draw != null)
		    dispose();
		draw = out.env().drawlist();
		if(doinst) {
		    instancer = new InstanceList(this);
		    instancer.add(draw, Rendered.class);
		    instancer.asyncadd(this, Rendered.class);
		    back = instancer;
		} else {
		    draw.asyncadd(this, Rendered.class);
		    back = draw;
		}
	    }
	    try(Locked lk = lock()) {
		if(instancer != null)
		    instancer.commit(out);
		draw.draw(out);
	    }
	}

	public void get(Render out, Coord c, Consumer<ClickData> cb) {
	    out.pget(basic, FragID.fragid, Area.sized(c, new Coord(1, 1)), new VectorFormat(1, NumberFormat.SINT32), data -> {
		    int id = data.getInt(0);
		    if(id == 0) {
			cb.accept(null);
			return;
		    }
		    Clickslot cs = idmap.get(id);
		    if(cs == null) {
			cb.accept(null);
			return;
		    }
		    cb.accept(new ClickData(cs.bk.state().get(Clickable.slot), (RenderTree.Slot)cs.bk.cast(RenderTree.Node.class)));
		});
	}

	public void fuzzyget(Render out, Coord c, int rad, Consumer<ClickData> cb) {
	    Area area = new Area(c.sub(rad, rad), c.add(rad + 1, rad + 1)).overlap(Area.sized(Coord.z, this.sz()));
	    out.pget(basic, FragID.fragid, area, new VectorFormat(1, NumberFormat.SINT32), data -> {
		    Clickslot cs;
		    {
			int id = data.getInt(area.ridx(c) * 4);
			if((id != 0) && ((cs = idmap.get(id)) != null)) {
			    cb.accept(new ClickData(cs.bk.state().get(Clickable.slot), (RenderTree.Slot)cs.bk.cast(RenderTree.Node.class)));
			    return;
			}
		    }
		    int maxr = Integer.MAX_VALUE;
		    Map<Clickslot, Integer> score = new HashMap<>();
		    for(Coord fc : area) {
			int id = data.getInt(area.ridx(fc) * 4);
			if((id == 0) || ((cs = idmap.get(id)) == null))
			    continue;
			int r = (int)Math.round(fc.dist(c) * 10);
			if(r < maxr) {
			    score.clear();
			    maxr = r;
			} else if(r > maxr) {
			    continue;
			}
			score.put(cs, score.getOrDefault(cs, 0) + 1);
		    }
		    int maxscore = 0;
		    cs = null;
		    for(Map.Entry<Clickslot, Integer> ent : score.entrySet()) {
			if((cs == null) || (ent.getValue() > maxscore)) {
			    maxscore = ent.getValue();
			    cs = ent.getKey();
			}
		    }
		    if(cs == null) {
			cb.accept(null);
			return;
		    }
		    cb.accept(new ClickData(cs.bk.state().get(Clickable.slot), (RenderTree.Slot)cs.bk.cast(RenderTree.Node.class)));
		});
	}

	public void dispose() {
	    if(instancer != null) {
		instancer.dispose();
		instancer = null;
	    }
	    if(draw != null) {
		draw.dispose();
		draw = null;
	    }
	    back = null;
	}

	public String stats() {
	    if(back == null)
		return("");
	    return(String.format("Tree %s, Inst %s, Draw %s, Map %d", master.stats(), (instancer == null) ? null : instancer.stats(), draw.stats(), idmap.size()));
	}
    }

    private final RenderTree clmaptree = new RenderTree();
    private final Clicklist clmaplist = new Clicklist(clmaptree, false);
    private final Clicklist clobjlist = new Clicklist(tree, true);
    private FragID<Texture.Image<Texture2D>> clickid;
    private ClickLocation<Texture.Image<Texture2D>> clickloc;
    private DepthBuffer<Texture.Image<Texture2D>> clickdepth;
    private Pipe.Op curclickbasic;
    private Pipe.Op clickbasic(Coord sz) {
	if((curclickbasic == null) || !clickid.image.tex.sz().equals(sz)) {
	    if(clickid != null) {
		clickid.image.tex.dispose();
		clickloc.image.tex.dispose();
		clickdepth.image.tex.dispose();
	    }
	    clickid = new FragID<>(new Texture2D(sz, DataBuffer.Usage.STATIC, new VectorFormat(1, NumberFormat.SINT32), null).image(0));
	    clickloc = new ClickLocation<>(new Texture2D(sz, DataBuffer.Usage.STATIC, new VectorFormat(2, NumberFormat.UNORM16), null).image(0));
	    clickdepth = new DepthBuffer<>(new Texture2D(sz, DataBuffer.Usage.STATIC, Texture.DEPTH, new VectorFormat(1, NumberFormat.FLOAT32), null).image(0));
	    curclickbasic = Pipe.Op.compose(Clicklist.clickbasic, clickid, clickdepth, new States.Viewport(Area.sized(Coord.z, sz)));
	}
	/* XXX: FrameInfo shouldn't be treated specially. Is a new
	 * Slot.Type in order, perhaps? */
	return(Pipe.Op.compose(curclickbasic, camera, conf.state().get(FrameInfo.slot)));
    }

    private void checkmapclick(Render out, Pipe.Op basic, Coord c, Consumer<Coord2d> cb) {
	new Object() {
	    MapMesh cut;
	    Coord2d pos;

	    {
		clmaplist.basic(Pipe.Op.compose(basic, clickloc));
		clmaplist.draw(out);
		if(clickdb) {
		    GOut.debugimage(out, clmaplist.basic, FragID.fragid, Area.sized(Coord.z, clmaplist.sz()), new VectorFormat(1, NumberFormat.SINT32),
				    img -> Debug.dumpimage(img, Debug.somedir("click1.png")));
		    GOut.debugimage(out, clmaplist.basic, ClickLocation.fragloc, Area.sized(Coord.z, clmaplist.sz()), new VectorFormat(3, NumberFormat.UNORM16),
				    img -> Debug.dumpimage(img, Debug.somedir("click2.png")));
		}
		clmaplist.get(out, c, cd -> {
			if(clickdb)
			    Debug.log.printf("map-id: %s\n", cd);
			if(cd != null)
			    this.cut = ((MapClick)cd.ci).cut;
			ckdone(1);
		    });
		out.pget(clmaplist.basic, ClickLocation.fragloc, Area.sized(c, new Coord(1, 1)), new VectorFormat(2, NumberFormat.FLOAT32), data -> {
			pos = new Coord2d(data.getFloat(0), data.getFloat(4));
			if(clickdb)
			    Debug.log.printf("map-pos: %s\n", pos);
			ckdone(2);
		    });
	    }

	    int dfl = 0;
	    void ckdone(int fl) {
		synchronized(this) {
		    if((dfl |= fl) == 3) {
			if(cut == null)
			    cb.accept(null);
			else
			    cb.accept(new Coord2d(cut.ul).add(pos.mul(new Coord2d(cut.sz))).mul(tilesz));
		    }
		}
	    }
	};
    }
    
    private static int gobclfuzz = 3;
    private void checkgobclick(Render out, Pipe.Op basic, Coord c, Consumer<ClickData> cb) {
	clobjlist.basic(basic);
	clobjlist.draw(out);
	if(clickdb) {
	    GOut.debugimage(out, clobjlist.basic, FragID.fragid, Area.sized(Coord.z, clobjlist.sz()), new VectorFormat(1, NumberFormat.SINT32),
			  img -> Debug.dumpimage(img, Debug.somedir("click3.png")));
	    Consumer<ClickData> ocb = cb;
	    cb = cl -> {
		Debug.log.printf("obj-id: %s\n", cl);
		ocb.accept(cl);
	    };
	}
	clobjlist.fuzzyget(out, c, gobclfuzz, cb);
    }
    
    public void delay(Delayed d) {
	synchronized(delayed) {
	    delayed.add(d);
	}
    }

    public void delay2(Delayed d) {
	synchronized(delayed2) {
	    delayed2.add(d);
	}
    }

    protected void undelay(Collection<Delayed> list, GOut g) {
	synchronized(list) {
	    for(Delayed d : list)
		d.run(g);
	    list.clear();
	}
    }

    static class PolText {
	Text text; double tm;
	PolText(Text text, double tm) {this.text = text; this.tm = tm;}
    }

    private static final Text.Furnace polownertf = new PUtils.BlurFurn(new Text.Foundry(Text.serif, 30).aa(true), 3, 1, Color.BLACK);
    private final Map<Integer, PolText> polowners = new HashMap<Integer, PolText>();

    public void setpoltext(int id, String text) {
	synchronized(polowners) {
	    polowners.put(id, new PolText(polownertf.render(text), Utils.rtime()));
	}
    }

    private void poldraw(GOut g) {
	if(polowners.isEmpty())
	    return;
	double now = Utils.rtime();
	synchronized(polowners) {
	    int y = (sz.y / 3) - (polowners.values().stream().map(t -> t.text.sz().y).reduce(0, (a, b) -> a + b + 10) / 2);
	    for(Iterator<PolText> i = polowners.values().iterator(); i.hasNext();) {
		PolText t = i.next();
		double poldt = now - t.tm;
		if(poldt < 6.0) {
		    int a;
		    if(poldt < 1.0)
			a = (int)(255 * poldt);
		    else if(poldt < 4.0)
			a = 255;
		    else
			a = (int)((255 * (2.0 - (poldt - 4.0))) / 2.0);
		    g.chcolor(255, 255, 255, a);
		    g.aimage(t.text.tex(), new Coord((sz.x - t.text.sz().x) / 2, y), 0.0, 0.0);
		    y += t.text.sz().y + 10;
		    g.chcolor();
		} else {
		    i.remove();
		}
	    }
	}
    }
    
    private void drawarrow(GOut g, double a) {
	Coord hsz = sz.div(2);
	double ca = -Coord.z.angle(hsz);
	Coord ac;
	if((a > ca) && (a < -ca)) {
	    ac = new Coord(sz.x, hsz.y - (int)(Math.tan(a) * hsz.x));
	} else if((a > -ca) && (a < Math.PI + ca)) {
	    ac = new Coord(hsz.x - (int)(Math.tan(a - Math.PI / 2) * hsz.y), 0);
	} else if((a > -Math.PI - ca) && (a < ca)) {
	    ac = new Coord(hsz.x + (int)(Math.tan(a + Math.PI / 2) * hsz.y), sz.y);
	} else {
	    ac = new Coord(0, hsz.y + (int)(Math.tan(a) * hsz.x));
	}
	Coord bc = ac.add(Coord.sc(a, -10));
	g.line(bc, bc.add(Coord.sc(a, -40)), 2);
	g.line(bc, bc.add(Coord.sc(a + Math.PI / 4, -10)), 2);
	g.line(bc, bc.add(Coord.sc(a - Math.PI / 4, -10)), 2);
    }

    public HomoCoord4f clipxf(Coord3f mc, boolean doclip) {
	HomoCoord4f ret = Homo3D.obj2clip(new Coord3f(mc.x, -mc.y, mc.z), basic.state());
	if(doclip && ret.clipped()) {
	    Projection s_prj = basic.state().get(Homo3D.prj);
	    Matrix4f prj = (s_prj == null) ? Matrix4f.id : s_prj.fin(Matrix4f.id);
	    ret = HomoCoord4f.lineclip(HomoCoord4f.fromclip(prj, Coord3f.o), ret);
	}
	return(ret);
    }

    public Coord3f screenxf(Coord3f mc) {
	return(clipxf(mc, false).toview(Area.sized(this.sz)));
    }

    public Coord3f screenxf(Coord2d mc) {
	Coord3f cc;
	try {
	    cc = getcc();
	} catch(Loading e) {
	    return(null);
	}
	return(screenxf(new Coord3f((float)mc.x, (float)mc.y, cc.z)));
    }

    public double screenangle(Coord2d mc, boolean clip) {
	Coord3f cc;
	try {
	    cc = getcc();
	} catch(Loading e) {
	    return(Double.NaN);
	}
	Coord3f mloc = new Coord3f((float)mc.x, -(float)mc.y, cc.z);
	float[] sloc = camera.proj.toclip(camera.view.fin(Matrix4f.id).mul4(mloc));
	if(clip) {
	    float w = sloc[3];
	    if((sloc[0] > -w) && (sloc[0] < w) && (sloc[1] > -w) && (sloc[1] < w))
		return(Double.NaN);
	}
	float a = ((float)sz.y) / ((float)sz.x);
	return(Math.atan2(sloc[1] * a, sloc[0]));
    }

    private void partydraw(GOut g) {
	for(Party.Member m : ui.sess.glob.party.memb.values()) {
	    if(m.gobid == this.plgob)
		continue;
	    Coord2d mc = m.getc();
	    if(mc == null)
		continue;
	    double a = screenangle(mc, true);
	    if(Double.isNaN(a))
		continue;
	    g.chcolor(m.col);
	    drawarrow(g, a);
	}
	g.chcolor();
    }

    protected void maindraw(Render out) {
	drawsmap(out);
	super.maindraw(out);
    }

    private Loading camload = null, lastload = null;
    public void draw(GOut g) {
	Loader.Future<Plob> placing = this.placing;
	if((placing != null) && placing.done())
	    placing.get().gtick(g.out);
	if((olftimer != 0) && (olftimer < Utils.rtime()))
	    unflashol();
	try {
	    if(camload != null)
		throw(new Loading(camload));
	    undelay(delayed, g);
	    super.draw(g);
	    undelay(delayed2, g);
	    poldraw(g);
	    partydraw(g);
	    glob.map.reqarea(cc.floor(tilesz).sub(MCache.cutsz.mul(view + 1)),
			     cc.floor(tilesz).add(MCache.cutsz.mul(view + 1)));
	} catch(Loading e) {
	    lastload = e;
	    String text = e.getMessage();
	    if(text == null)
		text = "Loading...";
	    g.chcolor(Color.BLACK);
	    g.frect(Coord.z, sz);
	    g.chcolor(Color.WHITE);
	    g.atext(text, sz.div(2), 0.5, 0.5);
	    if(e instanceof Resource.Loading) {
		((Resource.Loading)e).boostprio(5);
	    }
	}
    }

    /************* Queued Movement / Pathfinding *************/
    private void updateSpeed(final double dt) {
	final Gob pl = ui.sess.glob.oc.getgob(plgob);
	if (pl != null) {
	    final Coord2d plc = new Coord2d(pl.getc());
	    if (lastrc != null) {
		totaldist += plc.dist(lastrc);
		totaldt += dt;
		if(totaldt >= 1) {
		    mspeedavg = totaldist/totaldt;
		    totaldt = 0;
		    totaldist = 0;
		}
		mspeed = plc.dist(lastrc) / dt;
	    } else {
		mspeedavg = 0;
		totaldist = 0;
		totaldt = 0;
		mspeed = 0;
	    }
	    lastrc = plc;
	}
    }

    public double speed() {
	return mspeedavg;
    }

    public double rspeed() {
	if (ui != null) {
	    final Gob g = ui.sess.glob.oc.getgob(plgob);
	    if (g != null) {
		return g.getv();
	    } else {
		return 0.0;
	    }
	} else {
	    return 0.0;
	}
    }

    /**
     * 1) If you made it to your destination within a reasonable limit
     * a) Exactly on target destination
     * b) Not moving anymore and within 5 units of it
     * c) Predictive model said it was okay
     */
    private boolean triggermove() {
	final Gob pl = ui.sess.glob.oc.getgob(plgob);
	if (pl != null) {
	    if (movingto != null && pl.getattr(Moving.class) != null) {
		final Coord2d plc = new Coord2d(pl.getc());
		final double left = plc.dist(movingto.dest()) / mspeed;
		//Only predictive models can trigger here
		return movingto.dest().dist(pl.rc) <= 5 || left == 0;
	    } else if (movingto == null || movingto.dest().dist(pl.rc) <= 5) {
		return true;
	    } else {
		//Way off target and not moving, cancel
		clearmovequeue();
		return false;
	    }
	} else {
	    return false;
	}
    }

    @SuppressWarnings("unused")
    public boolean hasmoves() {
	return movequeue.size() > 0 || movingto != null;
    }

    public void clearmovequeue() {
	synchronized (movequeue) {
	    movequeue.clear();
	    movingto = null;
	    //TODO: Pointer
	    //ui.gui.pointer.update(null);
	}
    }

    public void queuemove(final Move c) {
	synchronized (movequeue) {
	    movequeue.add(c);
	}
    }

    public boolean los(final Coord2d c) {
	final NBAPathfinder finder = new NBAPathfinder(ui);
	return finder.walk(new Coord(ui.sess.glob.oc.getgob(plgob).getc()), c.floor());
    }

    public void los(final Gob g) {

    }

    public Move[] findpath(final Coord2d c) {
	final NBAPathfinder finder = new NBAPathfinder(ui);
	final List<Move> moves = finder.path(new Coord(ui.sess.glob.oc.getgob(plgob).getc()), c.floor());

	if (moves != null && ui.gui.settings.RESEARCHUNTILGOAL.get() && moves.get(moves.size() - 1).dest().dist(c) > 1.0) {
	    moves.add(new Move.Repath(moves.get(moves.size() - 1).dest(), c, null));
	}

	return moves != null ? moves.toArray(new Move[0]) : null;
    }

    public Move[] findpath(final Gob g) {
	final Coord2d c = new Coord2d(g.getc());
	g.updatePathfindingBlackout(true);
	final NBAPathfinder finder = new NBAPathfinder(ui);
	final List<Move> moves = finder.path(new Coord(ui.sess.glob.oc.getgob(plgob).getc()), c.floor());

	if (moves != null && ui.gui.settings.RESEARCHUNTILGOAL.get() && moves.get(moves.size() - 1).dest().dist(c) > 1.0) {
	    moves.add(new Move.Repath(moves.get(moves.size() - 1).dest(), c, null));
	}
	g.updatePathfindingBlackout(false);
	return moves != null ? moves.toArray(new Move[0]) : null;
    }

    public void pathto(final Coord2d c) {
        final Gob me = player();
        if(me != null) {
            me.updatePathfindingBlackout(true);
	    final Move[] moves = findpath(c);
	    if (moves != null) {
		clearmovequeue();
		for (final Move m : moves) {
		    queuemove(m);
		}
	    }
	    me.updatePathfindingBlackout(false);
	}
    }

    public void pathto(final Gob g) {
	final Gob me = player();
	if(me != null) {
	    me.updatePathfindingBlackout(true);
	    final Move[] moves = findpath(g);
	    if (moves != null) {
		clearmovequeue();
		for (final Move m : moves) {
		    queuemove(m);
		}
	    }
	    me.updatePathfindingBlackout(false);
	}
    }

    public void moveto(final Coord2d c) {
	clearmovequeue();
	wdgmsg("click", new Coord(1, 1), c.floor(posres), 1, 0);
    }

    public void relMove(final double x, final double y) {
	relMove(new Coord2d(x, y));
    }

    public void relMove(final Coord2d c) {
	if (ui != null) {
	    final Gob g = ui.sess.glob.oc.getgob(plgob);
	    if (g != null) {
		final Coord gc = new Coord2d(g.getc()).add(c).floor(posres);
		wdgmsg("click", new Coord(1, 1), gc, 1, 0);
	    }
	}
    }

    public Move movingto() {
	return movingto;
    }

    public Iterator<Move> movequeue() {
	return movequeue.iterator();
    }
    /*****/

    public void tick(double dt) {
	super.tick(dt);
	glob.map.sendreqs();
	camload = null;
	try {
	    if((shake = shake * Math.pow(100, -dt)) < 0.01)
		shake = 0;
	    camoff.x = (float)((Math.random() - 0.5) * shake);
	    camoff.y = (float)((Math.random() - 0.5) * shake);
	    camoff.z = (float)((Math.random() - 0.5) * shake);
	    camera.tick(dt);
	} catch(Loading e) {
	    camload = e;
	}
	basic(Camera.class, camera);
	amblight();
	updsmap(amblight);
	updweather();
	synchronized(glob.map) {
	    terrain.tick();
	    oltick();
	    if(gridlines != null)
		gridlines.tick();
	    clickmap.tick();
	}
	Loader.Future<Plob> placing = this.placing;
	if((placing != null) && placing.done())
	    placing.get().ctick(dt);

	synchronized (movequeue) {
	    if (movequeue.size() > 0 && (System.currentTimeMillis() - lastMove > 500) && triggermove()) {
		movingto = movequeue.poll();
		if (movingto != null) {
		    //TODO: Pointer
		    //ui.gui.pointer.update(movingto.dest());
		    movingto.apply(this);
		    lastMove = System.currentTimeMillis();
		}
	    }
	}
    }
    
    public void resize(Coord sz) {
	super.resize(sz);
	camera.resized();
    }

    public static interface PlobAdjust {
	public void adjust(Plob plob, Coord pc, Coord2d mc, int modflags);
	public boolean rotate(Plob plob, int amount, int modflags);
    }

    public static class StdPlace implements PlobAdjust {
	boolean freerot = false;
	Coord2d gran = (plobgran == 0)?null:new Coord2d(1.0 / plobgran, 1.0 / plobgran).mul(tilesz);

	public void adjust(Plob plob, Coord pc, Coord2d mc, int modflags) {
	    Coord2d nc;
	    if((modflags & 2) == 0)
		nc = mc.floor(tilesz).mul(tilesz).add(tilesz.div(2));
	    else if(gran != null)
		nc = mc.add(gran.div(2)).floor(gran).mul(gran);
	    else
		nc = mc;
	    Gob pl = plob.mv().player();
	    if((pl != null) && !freerot)
		plob.move(nc, Math.round(plob.rc.angle(pl.rc) / (Math.PI / 2)) * (Math.PI / 2));
	    else
		plob.move(nc);
	}

	public boolean rotate(Plob plob, int amount, int modflags) {
	    if((modflags & 1) == 0)
		return(false);
	    freerot = true;
	    double na;
	    if((modflags & 2) == 0)
		na = (Math.PI / 4) * Math.round((plob.a + (amount * Math.PI / 4)) / (Math.PI / 4));
	    else
		na = plob.a + amount * Math.PI / 16;
	    na = Utils.cangle(na);
	    plob.move(na);
	    return(true);
	}
    }

    public class Plob extends Gob {
	public PlobAdjust adjust = new StdPlace();
	Coord lastmc = null;
	RenderTree.Slot slot;

	private Plob(Indir<Resource> res, Message sdt) {
	    super(MapView.this.glob, MapView.this.cc);
	    setattr(new ResDrawable(this, res, sdt));
	}

	public MapView mv() {return(MapView.this);}

	public void move(Coord2d c, double a) {
	    super.move(c, a);
	    updated();
	}

	public void move(Coord2d c) {
	    move(c, this.a);
	}

	public void move(double a) {
	    move(this.rc, a);
	}

	void place() {
	    if(ui.mc.isect(rootpos(), sz))
		new Adjust(ui.mc.sub(rootpos()), 0).run();
	    this.slot = basic.add(this.placed);
	}

	private class Adjust extends Maptest {
	    int modflags;
	    
	    Adjust(Coord c, int modflags) {
		super(c);
		this.modflags = modflags;
	    }
	    
	    public void hit(Coord pc, Coord2d mc) {
		adjust.adjust(Plob.this, pc, mc, modflags);
		lastmc = pc;
	    }
	}
    }

    private Collection<String> olflash = null;
    private double olftimer;

    private void unflashol() {
	if(olflash != null) {
	    olflash.forEach(this::disol);
	}
	olflash = null;
	olftimer = 0;
    }

    private void flashol(Collection<String> ols, double tm) {
	unflashol();
	ols.forEach(this::enol);
	olflash = ols;
	olftimer = Utils.rtime() + tm;
    }

    public void uimsg(String msg, Object... args) {
	if(msg == "place") {
	    Loader.Future<Plob> placing = this.placing;
	    if(placing != null) {
		if(!placing.cancel()) {
		    Plob ob = placing.get();
		    synchronized(ob) {
			ob.slot.remove();
		    }
		}
		this.placing = null;
	    }
	    int a = 0;
	    Indir<Resource> res = ui.sess.getres((Integer)args[a++]);
	    Message sdt;
	    if((args.length > a) && (args[a] instanceof byte[]))
		sdt = new MessageBuf((byte[])args[a++]);
	    else
		sdt = Message.nil;
	    int oa = a;
	    this.placing = glob.loader.defer(new Supplier<Plob>() {
		    int a = oa;
		    Plob ret = null;
		    public Plob get() {
			if(ret == null)
			    ret = new Plob(res, new MessageBuf(sdt));
			while(a < args.length) {
			    int a2 = a;
			    Indir<Resource> ores = ui.sess.getres((Integer)args[a2++]);
			    Message odt;
			    if((args.length > a2) && (args[a2] instanceof byte[]))
				odt = new MessageBuf((byte[])args[a2++]);
			    else
				odt = Message.nil;
			    ret.addol(ores, odt);
			    a = a2;
			}
			ret.place();
			return(ret);
		    }
		});
	} else if(msg == "unplace") {
	    Loader.Future<Plob> placing = this.placing;
	    if(placing != null) {
		if(!placing.cancel()) {
		    Plob ob = placing.get();
		    synchronized(ob) {
			ob.slot.remove();
		    }
		}
		this.placing = null;
	    }
	} else if(msg == "move") {
	    cc = ((Coord)args[0]).mul(posres);
	} else if(msg == "plob") {
	    if(args[0] == null)
		plgob = -1;
	    else {
		plgob = Utils.uint32((Integer) args[0]);
		rlplgob = plgob;
	    }
	} else if(msg == "flashol") {
	    Collection<String> ols = new ArrayList<>();
	    int olflash = (Integer)args[0];
	    for(int i = 0; i < 32; i++) {
		if((olflash & (1 << i)) != 0)
		    ols.add(oltag(i));
	    }
	    double tm = ((Number)args[1]).doubleValue() / 1000.0;
	    flashol(ols, tm);
	} else if(msg == "flashol2") {
	    Collection<String> ols = new LinkedList<>();
	    double tm = ((Number)args[0]).doubleValue() / 100.0;
	    for(int a = 1; a < args.length; a++)
		ols.add((String)args[a]);
	    flashol(ols, tm);
	} else if(msg == "sel") {
	    boolean sel = ((Integer)args[0]) != 0;
	    synchronized(this) {
		if(sel && (selection == null)) {
		    selection = new Selector();
		} else if(!sel && (selection != null)) {
		    selection.destroy();
		    selection = null;
		}
	    }
	} else if(msg == "shake") {
	    shake += ((Number)args[0]).doubleValue();
	} else {
	    super.uimsg(msg, args);
	}
    }

    private UI.Grab camdrag = null;
    
    public abstract class Maptest {
	private final Coord pc;

	public Maptest(Coord c) {
	    this.pc = c;
	}

	public void run() {
	    Environment env = ui.env;
	    Render out = env.render();
	    Pipe.Op basic = clickbasic(MapView.this.sz);
	    Pipe bstate = new BufPipe().prep(basic);
	    out.clear(bstate, FragID.fragid, FColor.BLACK);
	    out.clear(bstate, 1.0);
	    checkmapclick(out, basic, pc, mc -> {
		    /* XXX: This is somewhat doubtfully nice, but running
		     * it in the defer group would cause unnecessary
		     * latency, and it shouldn't really be a problem. */
		    new HackThread(() -> {
			    synchronized(ui) {
				if(mc != null)
				    hit(pc, mc);
				else
				    nohit(pc);
			    }
		    }, "Hit-test callback").start();
		});
	    env.submit(out);
	}

	protected abstract void hit(Coord pc, Coord2d mc);
	protected void nohit(Coord pc) {}
    }

    public abstract class Hittest {
	private final Coord pc;
	private Coord2d mapcl;
	private ClickData objcl;
	private int dfl = 0;
	
	public Hittest(Coord c) {
	    pc = c;
	}
	
	public void run() {
	    Environment env = ui.env;
	    Render out = env.render();
	    Pipe.Op basic = clickbasic(MapView.this.sz);
	    Pipe bstate = new BufPipe().prep(basic);
	    out.clear(bstate, FragID.fragid, FColor.BLACK);
	    out.clear(bstate, 1.0);
	    checkmapclick(out, basic, pc, mc -> {mapcl = mc; ckdone(1);});
	    out.clear(bstate, FragID.fragid, FColor.BLACK);
	    checkgobclick(out, basic, pc, cl -> {objcl = cl; ckdone(2);});
	    env.submit(out);
	}

	private void ckdone(int fl) {
	    boolean done = false;
	    synchronized(this) {
		    if((dfl |= fl) == 3)
			done = true;
	    }
	    if(done) {
		/* XXX: This is somewhat doubtfully nice, but running
		 * it in the defer group would cause unnecessary
		 * latency, and it shouldn't really be a problem. */
		new HackThread(() -> {
			synchronized(ui) {
			    if(mapcl != null) {
				if(objcl == null)
				    hit(pc, mapcl, null);
				else
				    hit(pc, mapcl, objcl);
			    } else {
				nohit(pc);
			    }
			}
		}, "Hit-test callback").start();
	    }
	}
	
	protected abstract void hit(Coord pc, Coord2d mc, ClickData inf);
	protected void nohit(Coord pc) {}
    }

    private class Click extends Hittest {
	int clickb;
	
	private Click(Coord c, int b) {
	    super(c);
	    clickb = b;
	}
	
	protected void hit(Coord pc, Coord2d mc, ClickData inf) {
	    final String seq = MouseBind.generateSequence(ui, clickb);
	    Object[] args = {pc, mc.floor(posres), clickb, ui.modflags()};
	    // For gob clicks
	    //[ 0, id, rc.floor(posres), 0 ,-1 ]
	    //  ^-- Contains overlay     ^   ^
	    //                           |   |- FastMesh Res ID
	    //                           |
	    //                           +-- Overlay id
	    if (inf != null)
		args = Utils.extend(args, inf.clickargs());

	    Object[] clickargs = args;
	    if (MV_SHOW_SPEC_MENU.match(seq)) {
		//TODO: impl later
	    } else if (MV_QUEUE_MOVE.match(seq)) {
		movequeue.add(new Move(mc));
	    } else if (MV_PATHFIND_MOVE.match(seq)) {
		if (clickargs.length > 4) {
		    final Gob g = ui.sess.glob.oc.getgob((int) clickargs[5]);
		    if (g != null)
			pathto(g);
		} else {
		    pathto(mc);
		}
	    } else {
		if (clickb == 1 || clickargs.length > 4)
		    clearmovequeue();
		wdgmsg("click", clickargs);
	    }
	}
    }

    private class Hover extends Hittest {
	private Hover(Coord c) {
	    super(c);
	}

	private Optional<Gob> gobFromClick(final ClickData inf) {
	    if (inf == null)
		return Optional.empty();
	    if (inf.ci instanceof Gob.GobClick) {
		return Optional.of(((Gob.GobClick) inf.ci).gob);
	    }
	    if (inf.ci instanceof Composited.CompositeClick) {
		return Optional.of(((Composited.CompositeClick) inf.ci).gi.gob);
	    } else {
		return Optional.empty();
	    }
	}

	private void updatett(final String ntt) {
	    if (!ntt.equals(lasttt)) {
		lasttt = null;
		try {
		    tt = RichText.render(ntt, 300);
		} catch (Exception e) {
		    e.printStackTrace();
		    tt = null;
		}
	    }
	}

	protected void hit(Coord pc, Coord2d mc, ClickData inf) {
	    if (inf != null) {
		final Optional<Gob> gob = gobFromClick(inf);
		if (gob.isPresent()) {
		    updatett(gob.get().details());
		} else {
		    try {
			final int tile_id = ui.sess.glob.map.gettile_safe(mc.div(MCache.tilesz).floor());
			final MCache.Grid grid = ui.sess.glob.map.getgrid(mc.floor(tilesz).div(MCache.cmaps));
			final Resource res = ui.sess.glob.map.tilesetr(tile_id);
			final String name = ui.sess.glob.map.tiler(tile_id).getClass().getSimpleName();
			updatett("Tile: " + res.name + "[" + tile_id + "] of type " + name + "\n" + mc + " [" + grid.id + "]");
		    } catch (Exception e) {
			e.printStackTrace();
			lasttt = "";
			tt = null;
		    }
		}
	    } else {
		try {
		    final int tile_id = ui.sess.glob.map.gettile_safe(mc.div(MCache.tilesz).floor());
		    final MCache.Grid grid = ui.sess.glob.map.getgrid(mc.floor(tilesz).div(MCache.cmaps));
		    final Resource res = ui.sess.glob.map.tilesetr(tile_id);
		    final String name = ui.sess.glob.map.tiler(tile_id).getClass().getSimpleName();
		    updatett("Tile: " + res.name + "[" + tile_id + "] of type " + name + "\n" + mc + " [" + grid.id + "]");
		} catch (Exception e) {
		    e.printStackTrace();
		    lasttt = "";
		    tt = null;
		}
	    }
	}
    }

    public void grab(Grabber grab) {
	this.grab = grab;
    }
    
    public void release(Grabber grab) {
	if(this.grab == grab)
	    this.grab = null;
    }
    
    public boolean mousedown(Coord c, int button) {
	parent.setfocus(this);
	Loader.Future<Plob> placing_l = this.placing;
	if(button == 2) {
	    if(((Camera)camera).click(c)) {
		camdrag = ui.grabmouse(this);
	    }
	} else if((placing_l != null) && placing_l.done()) {
	    Plob placing = placing_l.get();
	    if(placing.lastmc != null)
		wdgmsg("place", placing.rc.floor(posres), (int)Math.round(placing.a * 32768 / Math.PI), button, ui.modflags());
	} else if((grab != null) && grab.mmousedown(c, button)) {
	} else {
	    new Click(c, button).run();
	}
	return(true);
    }
    
    public void mousemove(Coord c) {
	if(grab != null)
	    grab.mmousemove(c);
	Loader.Future<Plob> placing_l = this.placing;
	if(camdrag != null) {
	    ((Camera)camera).drag(c);
	} else if((placing_l != null) && placing_l.done()) {
	    Plob placing = placing_l.get();
	    if((placing.lastmc == null) || !placing.lastmc.equals(c)) {
		placing.new Adjust(c, ui.modflags()).run();
	    }
	} else if (ui.gui.settings.SHOWHOVERTOOLTIPS.get()) {
	    new Hover(c).run();
	} else {
	    lasttt = "";
	    tt = null;
	}
    }
    
    public boolean mouseup(Coord c, int button) {
	if(button == 2) {
	    if(camdrag != null) {
		camera.release();
		camdrag.remove();
		camdrag = null;
	    }
	} else if(grab != null) {
	    grab.mmouseup(c, button);
	}
	return(true);
    }

    public boolean mousewheel(Coord c, int amount) {
	Loader.Future<Plob> placing_l = this.placing;
	if((grab != null) && grab.mmousewheel(c, amount))
	    return(true);
	if((placing_l != null) && placing_l.done()) {
	    Plob placing = placing_l.get();
	    if(placing.adjust.rotate(placing, amount, ui.modflags()))
		return(true);
	}
	return(((Camera)camera).wheel(c, amount));
    }
    
    public boolean drop(final Coord cc, Coord ul) {
	new Hittest(cc) {
	    public void hit(Coord pc, Coord2d mc, ClickData inf) {
		wdgmsg("drop", pc, mc.floor(posres), ui.modflags());
	    }
	}.run();
	return(true);
    }
    
    public boolean iteminteract(Coord cc, Coord ul) {
	new Hittest(cc) {
	    public void hit(Coord pc, Coord2d mc, ClickData inf) {
		Object[] args = {pc, mc.floor(posres), ui.modflags()};
		if(inf != null)
		    args = Utils.extend(args, inf.clickargs());
		wdgmsg("itemact", args);
	    }
	}.run();
	return(true);
    }

    public boolean keydown(KeyEvent ev) {
	Loader.Future<Plob> placing_l = this.placing;
	if((placing_l != null) && placing_l.done()) {
	    Plob placing = placing_l.get();
	    if((ev.getKeyCode() == KeyEvent.VK_LEFT) && placing.adjust.rotate(placing, -1, ui.modflags()))
		return(true);
	    if((ev.getKeyCode() == KeyEvent.VK_RIGHT) && placing.adjust.rotate(placing, 1, ui.modflags()))
		return(true);
	}
	if(camera.keydown(ev))
	    return(true);
	return(super.keydown(ev));
    }


    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
    private void setupKeyBinds() {
        binds.put(KB_TOGGLE_GRID, () -> { showgrid(gridlines == null); return true; });
        binds.put(KB_TOGGLE_TIPS, () -> {
	    ui.gui.settings.SHOWHOVERTOOLTIPS.set(!ui.gui.settings.SHOWHOVERTOOLTIPS.get());
            return true;
	});
        binds.put(KB_TOGGLE_HIDDEN, () -> {
	    ui.gui.settings.SHOWHIDDEN.set(!ui.gui.settings.SHOWHIDDEN.get());
	    //TODO: This can result in very buggy behavior and needs reexamined at some point
	    ui.sess.glob.oc.mbRefreshGobs.mail(new OCache.RefreshGobByAttr(Hidden.class));
            return true;
	});
        binds.put(KB_TOGGLE_HITBOXES, () -> {
	    ui.gui.settings.SHOWHITBOX.set(!ui.gui.settings.SHOWHITBOX.get());
	    return true;
        });
    }

    public boolean globtype(char c, KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
        return false;
    }

    public Object tooltip(Coord c, Widget prev) {
	if(selection != null) {
	    if(selection.tt != null)
		return(selection.tt);
	} else if (tt != null) {
	    return tt;
	}
	return(super.tooltip(c, prev));
    }

    public class GrabXL implements Grabber {
	private final Grabber bk;
	public boolean mv = false;

	public GrabXL(Grabber bk) {
	    this.bk = bk;
	}

	public boolean mmousedown(Coord cc, final int button) {
	    new Maptest(cc) {
		public void hit(Coord pc, Coord2d mc) {
		    bk.mmousedown(mc.round(), button);
		}
	    }.run();
	    return(true);
	}

	public boolean mmouseup(Coord cc, final int button) {
	    new Maptest(cc) {
		public void hit(Coord pc, Coord2d mc) {
		    bk.mmouseup(mc.round(), button);
		}
	    }.run();
	    return(true);
	}

	public boolean mmousewheel(Coord cc, final int amount) {
	    new Maptest(cc) {
		public void hit(Coord pc, Coord2d mc) {
		    bk.mmousewheel(mc.round(), amount);
		}
	    }.run();
	    return(true);
	}

	public void mmousemove(Coord cc) {
	    if(mv) {
		new Maptest(cc) {
		    public void hit(Coord pc, Coord2d mc) {
			bk.mmousemove(mc.round());
		    }
		}.run();
	    }
	}
    }

    public static final OverlayInfo selol = new OverlayInfo() {
	    final Material mat = new Material(new BaseColor(255, 255, 0, 32), States.maskdepth);

	    public Collection<String> tags() {
		return(Arrays.asList("show"));
	    }

	    public Material mat() {return(mat);}
	};
    private class Selector implements Grabber {
	Coord sc;
	MCache.Overlay ol;
	UI.Grab mgrab;
	int modflags;
	Text tt;
	final GrabXL xl = new GrabXL(this) {
		public boolean mmousedown(Coord cc, int button) {
		    if(button != 1)
			return(false);
		    return(super.mmousedown(cc, button));
		}
		public boolean mmousewheel(Coord cc, int amount) {
		    return(false);
		}
	    };

	{
	    grab(xl);
	}

	public boolean mmousedown(Coord mc, int button) {
	    synchronized(MapView.this) {
		if(selection != this)
		    return(false);
		if(sc != null) {
		    ol.destroy();
		    mgrab.remove();
		}
		sc = mc.div(MCache.tilesz2);
		modflags = ui.modflags();
		xl.mv = true;
		mgrab = ui.grabmouse(MapView.this);
		ol = glob.map.new Overlay(Area.sized(sc, new Coord(1, 1)), selol);
		return(true);
	    }
	}

	public boolean mmouseup(Coord mc, int button) {
	    synchronized(MapView.this) {
		if(sc != null) {
		    Coord ec = mc.div(MCache.tilesz2);
		    xl.mv = false;
		    tt = null;
		    ol.destroy();
		    mgrab.remove();
		    wdgmsg("sel", sc, ec, modflags);
		    sc = null;
		}
		return(true);
	    }
	}

	public boolean mmousewheel(Coord mc, int amount) {
	    return(false);
	}

	public void mmousemove(Coord mc) {
	    synchronized(MapView.this) {
		if(sc != null) {
		    Coord tc = mc.div(MCache.tilesz2);
		    Coord c1 = new Coord(Math.min(tc.x, sc.x), Math.min(tc.y, sc.y));
		    Coord c2 = new Coord(Math.max(tc.x, sc.x), Math.max(tc.y, sc.y));
		    ol.update(new Area(c1, c2.add(1, 1)));
		    tt = Text.render(String.format("%d\u00d7%d", c2.x - c1.x + 1, c2.y - c1.y + 1));
		}
	    }
	}

	public void destroy() {
	    synchronized(MapView.this) {
		if(sc != null) {
		    ol.destroy();
		    mgrab.remove();
		}
		release(xl);
	    }
	}
    }

    private Camera makecam(Class<? extends Camera> ct, String... args) {
	try {
	    try {
		Constructor<? extends Camera> cons = ct.getConstructor(MapView.class, String[].class);
		return(cons.newInstance(new Object[] {this, args}));
	    } catch(IllegalAccessException e) {
	    } catch(NoSuchMethodException e) {
	    }
	    try {
		Constructor<? extends Camera> cons = ct.getConstructor(MapView.class);
		return(cons.newInstance(new Object[] {this}));
	    } catch(IllegalAccessException e) {
	    } catch(NoSuchMethodException e) {
	    }
	} catch(InstantiationException e) {
	    throw(new Error(e));
	} catch(InvocationTargetException e) {
	    if(e.getCause() instanceof RuntimeException)
		throw((RuntimeException)e.getCause());
	    throw(new RuntimeException(e));
	}
	throw(new RuntimeException("No valid constructor found for camera " + ct.getName()));
    }

    public void setcam(final String cam) {
	Class<? extends Camera> ct = camtypes.get(cam);
	if (ct != null) {
	    camera = makecam(ct);
	} else {
	    camera = new SOrthoCam(true, false);
	}
    }

    private Camera restorecam() {
	final UI ui = glob.ui.get();
	if(ui != null) {
	    Class<? extends Camera> ct = camtypes.get(ui.gui.settings.CAMERA.get());
	    if (ct != null) {
		return makecam(ct);
	    } else {
		return new SOrthoCam(true, false);
	    }
	} else {
	    return new SOrthoCam(true, false);
	}
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    {
	cmdmap.put("dump-hitmap", (cons, args) -> {
            final var buf = ui.sess.glob.gobhitmap.debug2(new Coord(Integer.MAX_VALUE, Integer.MAX_VALUE),
		    new Coord(Integer.MIN_VALUE, Integer.MIN_VALUE));
	    try {
		javax.imageio.ImageIO.write(buf, "png", new File("hitmap_gobs.png"));
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	});
	cmdmap.put("cam", new Console.Command() {
		public void run(Console cons, String[] args) throws Exception {
		    if(args.length >= 2) {
			Class<? extends Camera> ct = camtypes.get(args[1]);
			String[] cargs = Utils.splice(args, 2);
			if(ct != null) {
				camera = makecam(ct, cargs);
				Utils.setpref("defcam", args[1]);
				Utils.setprefb("camargs", Utils.serialize(cargs));
			} else {
			    throw(new Exception("no such camera: " + args[1]));
			}
		    }
		}
	    });
	cmdmap.put("whyload", new Console.Command() {
		public void run(Console cons, String[] args) throws Exception {
		    Loading l = lastload;
		    if(l == null)
			throw(new Exception("Not loading"));
		    l.printStackTrace(cons.out);
		}
	    });
    }
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }

    static {
	Console.setscmd("placegrid", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if((plobgran = Integer.parseInt(args[1])) < 0)
			plobgran = 0;
		    Utils.setprefi("plobgran", plobgran);
		}
	    });
	Console.setscmd("clickfuzz", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if((gobclfuzz = Integer.parseInt(args[1])) < 0)
			gobclfuzz = 0;
		}
	    });
	Console.setscmd("clickdb", new Console.Command() {
		public void run(Console cons, String[] args) {
		    clickdb = Utils.parsebool(args[1], false);
		}
	    });
    }
}
