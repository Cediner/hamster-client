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

import java.util.*;
import java.util.function.*;

import hamster.GlobalSettings;
import hamster.data.map.MarkerData;
import hamster.data.gob.ObjData;
import hamster.gob.*;
import hamster.gob.attrs.draw2d.Speed;
import hamster.gob.attrs.info.ScreenLocation;
import hamster.gob.attrs.mods.CheeseRackStatus;
import hamster.gob.attrs.mods.CupboardStatus;
import hamster.gob.attrs.mods.DryingFrameStatus;
import hamster.gob.attrs.mods.TanTubStatus;
import hamster.gob.attrs.monitors.*;
import hamster.gob.sprites.Mark;
import hamster.gob.sprites.TargetSprite;
import hamster.script.pathfinding.Hitbox;
import hamster.util.JobSystem;
import haven.render.*;
import integrations.mapv4.MapConfig;
import integrations.mapv4.MappingClient;

public class Gob implements RenderTree.Node, Sprite.Owner, Skeleton.ModOwner, Skeleton.HasPose {
    public Coord2d rc;
    public double a;
    public boolean virtual = false;
    int clprio = 0;
    public long id;
    public final Glob glob;
    public Map<Class<? extends GAttrib>, GAttrib> attr = new HashMap<Class<? extends GAttrib>, GAttrib>();
    public final Collection<Overlay> ols = new ArrayList<Overlay>();
    public final Collection<RenderTree.Slot> slots = new ArrayList<>(1);
    private final Collection<SetupMod> setupmods = new ArrayList<>();
    private final Collection<ResAttr.Cell<?>> rdata = new LinkedList<ResAttr.Cell<?>>();
    private final Collection<ResAttr.Load> lrdata = new LinkedList<ResAttr.Load>();

    //Overlays / GAttribs that need to be added after a tick, delayed
    private final List<Overlay> dols = new ArrayList<>();
    private final List<Pair<GAttrib, Consumer<Gob>>> dattrs = new ArrayList<>();

    // Gob tags that help define what it is
    private Set<Tag> tags;
    //A Gob can be holding many things (see: boats)
    private final Set<Long> holding = new HashSet<>();
    //A Gob can only be held by one thing
    private long heldby = -1;
    //Account for this gob during hit checks or not in pathfinding
    private Hitbox hitbox = null;

    public static class Overlay implements RenderTree.Node {
	public final int id;
	public final Gob gob;
	public final Indir<Resource> res;
	public MessageBuf sdt;
	public Sprite spr;
	public boolean delign = false;
	private Collection<RenderTree.Slot> slots = null;
	private boolean added = false;

	public Overlay(Gob gob, int id, Indir<Resource> res, Message sdt) {
	    this.gob = gob;
	    this.id = id;
	    this.res = res;
	    this.sdt = new MessageBuf(sdt);
	    this.spr = null;
	}

	public Overlay(Gob gob, int id, Sprite spr) {
	    this.gob = gob;
	    this.id = id;
	    this.res = null;
	    this.sdt = null;
	    this.spr = spr;
	}

	public Overlay(Gob gob, Sprite spr) {
	    this.gob = gob;
	    this.id = -1;
	    this.res = null;
	    this.sdt = null;
	    this.spr = spr;
	}

	public String name() {
	    try {
		if (res instanceof Session.CachedRes.Ref) {
		    return ((Session.CachedRes.Ref) res).name();
		} else if (res instanceof Resource.Named) {
		    return ((Resource.Spec) res).name;
		} else if (res != null) {
		    return res.get().name;
		} else {
		    return "";
		}
	    } catch (Loading l) {
		return "";
	    }
	}

	private void init() {
	    if(spr == null) {
		spr = Sprite.create(gob, res.get(), sdt);
		if(added && (spr instanceof SetupMod))
		    gob.setupmods.add((SetupMod)spr);
	    }
	    if(slots == null)
		RUtils.multiadd(gob.slots, this);
	}

	private void add0() {
	    if(added)
		throw(new IllegalStateException());
	    if(spr instanceof SetupMod)
		gob.setupmods.add((SetupMod)spr);
	    added = true;
	}

	private void remove0() {
	    if(!added)
		throw(new IllegalStateException());
	    if(slots != null) {
		RUtils.multirem(new ArrayList<>(slots));
		slots = null;
	    }
	    if(spr instanceof SetupMod)
		gob.setupmods.remove(spr);
	    added = false;
	}

	public void remove() {
	    remove0();
	    gob.ols.remove(this);
	}

	public void added(RenderTree.Slot slot) {
	    slot.add(spr);
	    if(slots == null)
		slots = new ArrayList<>(1);
	    slots.add(slot);
	}

	public void removed(RenderTree.Slot slot) {
	    if(slots != null)
		slots.remove(slot);
	}
    }

    public static interface SetupMod {
	public default Pipe.Op gobstate() {return(null);}
	public default Pipe.Op placestate() {return(null);}
    }

    /* XXX: This whole thing didn't turn out quite as nice as I had
     * hoped, but hopefully it can at least serve as a source of
     * inspiration to redo attributes properly in the future. There
     * have already long been arguments for remaking GAttribs as
     * well. */
    public static class ResAttr {
	public boolean update(Message dat) {
	    return(false);
	}

	public void dispose() {
	}

	public static class Cell<T extends ResAttr> {
	    final Class<T> clsid;
	    Indir<Resource> resid = null;
	    MessageBuf odat;
	    public T attr = null;

	    public Cell(Class<T> clsid) {
		this.clsid = clsid;
	    }

	    public void set(ResAttr attr) {
		if(this.attr != null)
		    this.attr.dispose();
		this.attr = clsid.cast(attr);
	    }
	}

	private static class Load {
	    final Indir<Resource> resid;
	    final MessageBuf dat;

	    Load(Indir<Resource> resid, Message dat) {
		this.resid = resid;
		this.dat = new MessageBuf(dat);
	    }
	}

	@Resource.PublishedCode(name = "gattr", instancer = FactMaker.class)
	public static interface Factory {
	    public ResAttr mkattr(Gob gob, Message dat);
	}

	public static class FactMaker implements Resource.PublishedCode.Instancer {
	    public Factory make(Class<?> cl, Resource ires, Object... argv) {
		if(Factory.class.isAssignableFrom(cl))
		    return(Resource.PublishedCode.Instancer.stdmake(cl.asSubclass(Factory.class), ires, argv));
		if(ResAttr.class.isAssignableFrom(cl)) {
		    try {
			final java.lang.reflect.Constructor<? extends ResAttr> cons = cl.asSubclass(ResAttr.class).getConstructor(Gob.class, Message.class);
			return(new Factory() {
				public ResAttr mkattr(Gob gob, Message dat) {
				    return(Utils.construct(cons, gob, dat));
				}
			    });
		    } catch(NoSuchMethodException e) {
		    }
		}
		return(null);
	    }
	}
    }

    public Gob(Glob glob, Coord2d c, long id) {
	this.glob = glob;
	this.rc = c;
	this.id = id;
	if(id < 0)
	    virtual = true;
    }

    public Gob(Glob glob, Coord2d c) {
	this(glob, c, -1);
    }

    public void ctick(double dt) {
	for(GAttrib a : attr.values())
	    a.ctick(dt);
	loadrattr();
	final Hidden hidden = getattr(Hidden.class);
	for(Iterator<Overlay> i = ols.iterator(); i.hasNext();) {
	    Overlay ol = i.next();
	    if(ol.slots == null) {
		if(hidden == null || GlobalSettings.SHOWHIDDEN.get()) {
		    try {
			ol.init();
		    } catch (Loading ignored) {
		    }
		}
	    } else {
		boolean done = ol.spr.tick(dt);
		if((!ol.delign || (ol.spr instanceof Sprite.CDel)) && done) {
		    ol.remove0();
		    i.remove();
		}
	    }
	}
	//This is to avoid Iterator state conflicts of adding overlays while in an overlay tick
	for (Iterator<Overlay> i = dols.iterator(); i.hasNext(); ) {
	    Overlay ol = i.next();
	    addol(ol);
	    i.remove();
	}
	updstate();
	if(virtual && ols.isEmpty() && (getattr(Drawable.class) == null))
	    glob.oc.remove(this);
    }

    public void gtick(Render g) {
	Drawable d = getattr(Drawable.class);
	if(d != null)
	    d.gtick(g);
	for(Overlay ol : ols) {
	    if(ol.spr != null)
		ol.spr.gtick(g);
	}
    }

    public void addol(Overlay ol, boolean async) {
	if(!async)
	    ol.init();
	ol.add0();
	ols.add(ol);
    }
    public void addol(Overlay ol) {
	addol(ol, true);
    }
    public void addol(Sprite ol) {
	addol(new Overlay(this, ol));
    }
    public void addol(Indir<Resource> res, Message sdt) {
	addol(new Overlay(this, -1, res, sdt));
    }

    public Overlay findol(int id) {
	for(Overlay ol : ols) {
	    if(ol.id == id)
		return(ol);
	}
	return(null);
    }

    public void dispose() {
        if(glob != null)
		glob.gobhitmap.remove(this);
	for(GAttrib a : attr.values())
	    a.dispose();
	for(ResAttr.Cell rd : rdata) {
	    if(rd.attr != null)
		rd.attr.dispose();
	}
    }

    public boolean moving() {
	return getattr(Moving.class) != null;
    }

    public void move(Coord2d c, double a) {
	Moving m = getattr(Moving.class);
	if(m != null)
	    m.move(c);
	this.rc = c;
	if (isplayer()) {
	    if (glob.ui != null) {
		UI ui = glob.ui.get();
		if (ui != null && ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    if (MapConfig.loadMapSetting(ui.sess.username, "mapper")) {
			MappingClient.getInstance(ui.sess.username).CheckGridCoord(c);
		    }
		}
	    }
	}
	this.a = a;
	if(hitbox != null) {
	    glob.gobhitmap.update(this);
	}
    }

    // For scripting api
    public Coord2d getDest() {
	Moving m = getattr(Moving.class);
	if(m != null) {
	    return m.getDest().orElse(rc);
	} else {
	    return rc;
	}
    }

    public Coord3f getc() {
	Moving m = getattr(Moving.class);
	Coord3f ret = (m != null)?m.getc():getrc();
	DrawOffset df = getattr(DrawOffset.class);
	if(df != null)
	    ret = ret.add(df.off);
	return(ret);
    }

    public Coord3f getrc() {
	return(glob.map.getzp(rc));
    }

    protected Pipe.Op getmapstate(Coord3f pc) {
	Tiler tile = glob.map.tiler(glob.map.gettile(new Coord2d(pc).floor(MCache.tilesz)));
	return(tile.drawstate(glob, pc));
    }

    private Class<? extends GAttrib> attrclass(Class<? extends GAttrib> cl) {
	while(true) {
	    Class<?> p = cl.getSuperclass();
	    if(p == GAttrib.class)
		return(cl);
	    cl = p.asSubclass(GAttrib.class);
	}
    }

    public <C extends GAttrib> C getattr(Class<C> c) {
	GAttrib attr = this.attr.get(attrclass(c));
	if(!c.isInstance(attr))
	    return(null);
	return(c.cast(attr));
    }

    private void setattr(Class<? extends GAttrib> ac, GAttrib a) {
	final Hidden hidden = getattr(Hidden.class);
	GAttrib prev = attr.remove(ac);
	if(prev != null) {
	    if((prev instanceof RenderTree.Node) && (prev.slots != null))
		RUtils.multirem(new ArrayList<>(prev.slots));
	    if(prev instanceof SetupMod)
		setupmods.remove(prev);
	}
	if(a != null) {
	    if(a instanceof RenderTree.Node && (hidden == null || GlobalSettings.SHOWHIDDEN.get())) {
		try {
		    RUtils.multiadd(this.slots, (RenderTree.Node)a);
		} catch(Loading l) {
		    if(prev instanceof RenderTree.Node) {
			RUtils.multiadd(this.slots, (RenderTree.Node)prev);
			attr.put(ac, prev);
		    }
		    if(prev instanceof SetupMod)
			setupmods.add((SetupMod)prev);
		    throw(l);
		}
	    }
	    if(a instanceof SetupMod)
		setupmods.add((SetupMod)a);
	    attr.put(ac, a);
	}

	if(a instanceof Hidden && !(this instanceof MapView.Plob)) {
	    //TODO: This can result in very buggy behavior and needs reexamined
	    glob.oc.mailbox.mail(new OCache.RefreshGobByObject(this));
	}

	if(prev != null)
	    prev.dispose();
    }

    public void setattr(GAttrib a) {
	setattr(attrclass(a.getClass()), a);
    }

    public void delattr(Class<? extends GAttrib> c) {
	setattr(attrclass(c), null);
    }

    private Class<? extends ResAttr> rattrclass(Class<? extends ResAttr> cl) {
	while(true) {
	    Class<?> p = cl.getSuperclass();
	    if(p == ResAttr.class)
		return(cl);
	    cl = p.asSubclass(ResAttr.class);
	}
    }

    @SuppressWarnings("unchecked")
    public <T extends ResAttr> ResAttr.Cell<T> getrattr(Class<T> c) {
	for(ResAttr.Cell<?> rd : rdata) {
	    if(rd.clsid == c)
		return((ResAttr.Cell<T>)rd);
	}
	ResAttr.Cell<T> rd = new ResAttr.Cell<T>(c);
	rdata.add(rd);
	return(rd);
    }

    public static <T extends ResAttr> ResAttr.Cell<T> getrattr(Object obj, Class<T> c) {
	if(!(obj instanceof Gob))
	    return(new ResAttr.Cell<T>(c));
	return(((Gob)obj).getrattr(c));
    }

    private void loadrattr() {
	boolean upd = false;
	for(Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext();) {
	    ResAttr.Load rd = i.next();
	    ResAttr attr;
	    try {
		attr = rd.resid.get().getcode(ResAttr.Factory.class, true).mkattr(this, rd.dat.clone());
	    } catch(Loading l) {
		continue;
	    }
	    ResAttr.Cell<?> rc = getrattr(rattrclass(attr.getClass()));
	    if(rc.resid == null)
		rc.resid = rd.resid;
	    else if(rc.resid != rd.resid)
		throw(new RuntimeException("Conflicting resattr resource IDs on " + rc.clsid + ": " + rc.resid + " -> " + rd.resid));
	    rc.odat = rd.dat;
	    rc.set(attr);
	    i.remove();
	    upd = true;
	}
    }

    public void setrattr(Indir<Resource> resid, Message dat) {
	for(Iterator<ResAttr.Cell<?>> i = rdata.iterator(); i.hasNext();) {
	    ResAttr.Cell<?> rd = i.next();
	    if(rd.resid == resid) {
		if(dat.equals(rd.odat))
		    return;
		if((rd.attr != null) && rd.attr.update(dat))
		    return;
		break;
	    }
	}
	for(Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext();) {
	    ResAttr.Load rd = i.next();
	    if(rd.resid == resid) {
		i.remove();
		break;
	    }
	}
	lrdata.add(new ResAttr.Load(resid, dat));
	loadrattr();
    }

    public void delrattr(Indir<Resource> resid) {
	for(Iterator<ResAttr.Cell<?>> i = rdata.iterator(); i.hasNext();) {
	    ResAttr.Cell<?> rd = i.next();
	    if(rd.resid == resid) {
		i.remove();
		rd.attr.dispose();
		break;
	    }
	}
	for(Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext();) {
	    ResAttr.Load rd = i.next();
	    if(rd.resid == resid) {
		i.remove();
		break;
	    }
	}
    }

    public void draw(GOut g) {}

    public static class GobClick extends Clickable {
	public final Gob gob;

	public GobClick(Gob gob) {
	    this.gob = gob;
	}

	public Object[] clickargs(ClickData cd) {
	    Object[] ret = {0, (int)gob.id, gob.rc.floor(OCache.posres), 0, -1};
	    for(Object node : cd.array()) {
		if(node instanceof Gob.Overlay) {
		    ret[0] = 1;
		    ret[3] = ((Gob.Overlay)node).id;
		}
		if(node instanceof FastMesh.ResourceMesh)
		    ret[4] = ((FastMesh.ResourceMesh)node).id;
	    }
	    return(ret);
	}

	public String toString() {
	    return(String.format("#<gob-click %d %s>", gob.id, gob.getres()));
	}
    }

    private class GobState implements Pipe.Op {
	final Pipe.Op mods;

	private GobState() {
	    if(setupmods.isEmpty()) {
		this.mods = null;
	    } else {
		Pipe.Op[] mods = new Pipe.Op[setupmods.size()];
		int n = 0;
		for(SetupMod mod : setupmods) {
		    if((mods[n] = mod.gobstate()) != null)
			n++;
		}
		this.mods = (n > 0) ? Pipe.Op.compose(mods) : null;
	    }
	}

	public void apply(Pipe buf) {
	    if(!virtual)
		buf.prep(new GobClick(Gob.this));
	    buf.prep(new TickList.Monitor(Gob.this));
	    if(mods != null)
		buf.prep(mods);
	}

	public boolean equals(GobState that) {
	    return(Utils.eq(this.mods, that.mods));
	}
	public boolean equals(Object o) {
	    return((o instanceof GobState) && equals((GobState)o));
	}
    }
    private GobState curstate = null;
    private GobState curstate() {
	if(curstate == null)
	    curstate = new GobState();
	return(curstate);
    }

    private void updstate() {
	GobState nst;
	try {
	    nst = new GobState();
	} catch(Loading l) {
	    return;
	}
	if(!Utils.eq(nst, curstate)) {
	    for(RenderTree.Slot slot : slots)
		slot.ostate(nst);
	    this.curstate = nst;
	}
    }

    /* Loftar's original added for all Ol's and GAttribs */
    public void _added(RenderTree.Slot slot) {
	slot.ostate(curstate());
	for (Overlay ol : ols) {
	    if (ol.slots != null)
		slot.add(ol);
	}
	for (GAttrib a : attr.values()) {
	    if (a instanceof RenderTree.Node)
		slot.add((RenderTree.Node) a);
	}
    }

    /* this is basically the new `setup`, but only happens once */
    public void added(RenderTree.Slot slot) {
	if (!virtual)
	    slot.ostate(curstate());
	final Hidden hidden = getattr(Hidden.class);
	if (GlobalSettings.SHOWHIDDEN.get() || hidden == null) {
	    _added(slot);
	} else {
	    slot.add(hidden);
	}
	slots.add(slot);
    }

    public void removed(RenderTree.Slot slot) {
	slots.remove(slot);
    }

    private Waitable.Queue updwait = null;
    private int updateseq = 0;
    void updated() {
	synchronized(this) {
	    updateseq++;
	    if(updwait != null)
		updwait.wnotify();
	}
    }

    public void updwait(Runnable callback, Consumer<Waitable.Waiting> reg) {
	/* Caller should probably synchronize on this already for a
	 * call like this to even be meaningful, but just in case. */
	synchronized(this) {
	    if(updwait == null)
		updwait = new Waitable.Queue();
	    reg.accept(updwait.add(callback));
	}
    }

    public static class DataLoading extends Loading {
	public final transient Gob gob;
	public final int updseq;

	/* It would be assumed that the caller has synchronized on gob
	 * while creating this exception. */
	public DataLoading(Gob gob, String message) {
	    super(message);
	    this.gob = gob;
	    this.updseq = gob.updateseq;
	}

	public void waitfor(Runnable callback, Consumer<Waitable.Waiting> reg) {
	    synchronized(gob) {
		if(gob.updateseq != this.updseq) {
		    reg.accept(Waitable.Waiting.dummy);
		    callback.run();
		} else {
		    gob.updwait(callback, reg);
		}
	    }
	}
    }

    public Random mkrandoom() {
	return(Utils.mkrandoom(id));
    }

    public Resource getres() {
	Drawable d = getattr(Drawable.class);
	if(d != null)
	    return(d.getres());
	return(null);
    }

    public Skeleton.Pose getpose() {
	Drawable d = getattr(Drawable.class);
	if(d != null)
	    return(d.getpose());
	return(null);
    }

    private static final ClassResolver<Gob> ctxr = new ClassResolver<Gob>()
	.add(Glob.class, g -> g.glob)
	.add(Session.class, g -> g.glob.sess);
    public <T> T context(Class<T> cl) {return(ctxr.context(cl, this));}

    @Deprecated
    public Glob glob() {return(context(Glob.class));}

    /* Because generic functions are too nice a thing for Java. */
    public double getv() {
	Moving m = getattr(Moving.class);
	if(m == null)
	    return(0);
	return(m.getv());
    }

    public class Placed implements RenderTree.Node, TickList.Ticking, TickList.TickNode {
	private final Collection<RenderTree.Slot> slots = new ArrayList<>(1);
	private Placement cur;

	private Placed() {}

	private class Placement implements Pipe.Op {
	    final Pipe.Op flw, tilestate, mods;
	    final Coord3f oc, rc;
	    final double a;

	    Placement() {
		try {
		    Following flw = Gob.this.getattr(Following.class);
		    Pipe.Op flwxf = (flw == null) ? null : flw.xf();
		    Pipe.Op tilestate = null;
		    if(flwxf == null) {
			Coord3f oc = Gob.this.getc();
			Coord3f rc = new Coord3f(oc);
			rc.y = -rc.y;
			this.flw = null;
			this.oc = oc;
			this.rc = rc;
			this.a = Gob.this.a;
			tilestate = Gob.this.getmapstate(oc);
		    } else {
			this.flw = flwxf;
			this.oc = this.rc = null;
			this.a = Double.NaN;
		    }
		    this.tilestate = tilestate;
		    if(setupmods.isEmpty()) {
			this.mods = null;
		    } else {
			Pipe.Op[] mods = new Pipe.Op[setupmods.size()];
			int n = 0;
			for(SetupMod mod : setupmods) {
			    if((mods[n] = mod.placestate()) != null)
				n++;
			}
			this.mods = (n > 0) ? Pipe.Op.compose(mods) : null;
		    }
		} catch(Loading bl) {
		    throw(new Loading(bl) {
			    public String getMessage() {return(bl.getMessage());}

			    public void waitfor(Runnable callback, Consumer<Waitable.Waiting> reg) {
				Waitable.or(callback, reg, bl, Gob.this::updwait);
			    }
			});
		}
	    }

	    public boolean equals(Placement that) {
		if(this.flw != null) {
		    if(!Utils.eq(this.flw, that.flw))
			return(false);
		} else {
		    if(!(Utils.eq(this.oc, that.oc) && (this.a == that.a)))
			return(false);
		}
		if(!Utils.eq(this.tilestate, that.tilestate))
		    return(false);
		if(!Utils.eq(this.mods, that.mods))
		    return(false);
		return(true);
	    }

	    public boolean equals(Object o) {
		return((o instanceof Placement) && equals((Placement)o));
	    }

	    Pipe.Op gndst = null;
	    public void apply(Pipe buf) {
		if(this.flw != null) {
		    this.flw.apply(buf);
		} else {
		    if(gndst == null)
			gndst = Pipe.Op.compose(new Location(Transform.makexlate(new Matrix4f(), this.rc), "gobx"),
						new Location(Transform.makerot(new Matrix4f(), Coord3f.zu, (float)-this.a), "gob"));
		    gndst.apply(buf);
		}
		if(tilestate != null)
		    tilestate.apply(buf);
		if(mods != null)
		    mods.apply(buf);
	    }
	}

	public Pipe.Op placement() {
	    return(new Placement());
	}

	public void autotick(double dt) {
	    synchronized(Gob.this) {
		Placement np;
		try {
		    np = new Placement();
		} catch(Loading l) {
		    return;
		}
		if(!Utils.eq(this.cur, np))
		    update(np);
	    }
	}

	private void update(Placement np) {
	    for(RenderTree.Slot slot : slots)
		slot.ostate(np);
	    this.cur = np;
	}

	public void added(RenderTree.Slot slot) {
	    slot.ostate(curplace());
	    slot.add(Gob.this);
	    slots.add(slot);
	}

	public void removed(RenderTree.Slot slot) {
	    slots.remove(slot);
	}

	public Pipe.Op curplace() {
	    if(cur == null)
		cur = new Placement();
	    return(cur);
	}

	public Coord3f getc() {
	    return((this.cur != null) ? this.cur.oc : null);
	}

	public TickList.Ticking ticker() {return(this);}
    }
    public final Placed placed = new Placed();


    // For Player Gobs this will get their Kin name if mem'd or id if not
    public String gobname() {
	final KinInfo ki = getattr(KinInfo.class);
	if (ki != null) {
	    return ki.name;
	} else {
	    return "" + id;
	}
    }

    /*
     * Various ways to get the Resource / Resource name of a gob
     */
    public Optional<String> resname() {
	return res().map((res) -> res.name);
    }

    public String name() {
	return resname().orElse("");
    }

    public Optional<Resource> res() {
	final Drawable d = getattr(Drawable.class);
	try {
	    return d != null ? Optional.of(d.getres()) : Optional.empty();
	} catch (Exception e) {
	    return Optional.empty();
	}
    }

    public Optional<String> getresname() {
	final Drawable d = getattr(Drawable.class);
	if (d != null) {
	    try {
		return Optional.of(d.getresname());
	    } catch (Resource.Loading l) {
		return Optional.empty();
	    }
	} else {
	    return Optional.empty();
	}
    }

    public String rnm(final Indir<Resource> res) {
	final String nm;
	if (res instanceof Session.CachedRes.Ref) {
	    nm = ((Session.CachedRes.Ref) res).name();
	} else if (res instanceof Resource.Named) {
	    nm = ((Resource.Spec) res).name;
	} else {
	    if (res != null && res.get() != null) {
		nm = res.get().name;
	    } else {
		nm = "";
	    }
	}

	return nm != null ? nm : "";
    }

    //Useful for getting stage information or model type
    public int sdt() {
	ResDrawable dw = getattr(ResDrawable.class);
	if (dw != null)
	    return dw.sdtnum();
	return 0;
    }

    /*
     * Tag System
     */
    public boolean hasTag(final Tag t) {
	return tags != null && tags.contains(t);
    }

    /*
     * Holding/Held By stuff
     */
    public boolean isHolding(final long id) {
	return holding.contains(id);
    }

    public void isNoLongerHolding(final long id) {
	holding.remove(id);
    }

    public void hold(final long id) {
	holding.add(id);
    }

    public boolean isHeldBySomething() {
	return heldby != -1;
    }

    public boolean isHeldBy(final long id) {
	return heldby == id;
    }

    public long whoIsHoldingMe() {
	return heldby;
    }

    public void isNoLongerHeld() {
	heldby = -1;
    }

    public void heldby(final long id) {
	heldby = id;
    }

    public int howManyGobsHeld() {
        return holding.size();
    }

    /*
     * Pathfinding Related
     */
    public void updatePathfindingBlackout(final boolean val) {
	if(val) {
	    glob.gobhitmap.remove(this);
	} else {
	    glob.gobhitmap.add(this);
	}
    }

    public Hitbox hitbox() {
        return hitbox;
    }

    public Optional<Hitbox> hitboxo() { return Optional.ofNullable(hitbox); }

    public void updateHitbox(final Coord2d off, final Coord2d sz) {
        if(hitbox != null)
	    glob.gobhitmap.add(this);
        hitbox = new Hitbox.Rectangular(off, sz, true);
	glob.gobhitmap.add(this);
    }

    /*
     * This is more useful for getting Bush/Tree max stages
     */
    public int getMaxStage(final int guess) {
	int max = guess;
	for (FastMesh.MeshRes layer : getres().layers(FastMesh.MeshRes.class)) {
	    final int stg = layer.id / 10;
	    max = Math.max(stg, max);
	}
	return max;
    }

    public boolean multistageplant() {
	return tags.contains(Tag.MULTISTAGE_PLANT) || name().endsWith("terobjs/plants/carrot") ||
		name().endsWith("terobjs/plants/hemp");
    }

    public boolean fallowplant() {
	return name().endsWith("/fallowplant");
    }

    /*
     * Overlay/Attrib extra functionality for adding/removing/getting
     */
    public void queueDeltas(final List<OCache.Delta> deltas) {
	if (deltas.size() > 0) {
	    final OCache.GobInfo ng = glob.oc.netget(id);
	    if (ng != null) {
		synchronized (ng) {
		    ng.pending.addAll(deltas);
		    ng.checkdirty(false);
		}
	    }
	}
    }

    public Overlay daddol(final Overlay ol) {
	dols.add(ol);
	return ol;
    }

    public Overlay daddol(int id, Sprite spr) {
	final Overlay ol = new Overlay(this, id, spr);
	daddol(ol);
	return ol;
    }

    public void mark(final int life) {
	queueDeltas(Collections.singletonList((gob) -> {
	    final Overlay ol = gob.findol(Mark.id);
	    if (ol != null) {
		((Mark) ol.spr).setLife(life);
	    } else {
		gob.addol(new Overlay(gob, Mark.id, new Mark(life)));
	    }
	}));
    }

    public void unmark() {
	queueDeltas(Collections.singletonList((gob) -> {
	    final Overlay ol = gob.findol(Mark.id);
	    if (ol != null) {
		((Mark) ol.spr).revoke();
	    }
	}));
    }

    /*
     * Misc
     */
    public boolean isDead() {
	Drawable d = getattr(Drawable.class);
	if (d instanceof Composite) {
	    Composite comp = (Composite) d;
	    if (comp.oldposes != null) {
		for (ResData res : comp.oldposes) {
		    final String nm = rnm(res.res).toLowerCase();
		    if (nm.endsWith("knock") || nm.endsWith("dead")) {
			return true;
		    }
		}
	    }
	}
	return false;
    }

    @SuppressWarnings("unused") // For scripting api
    public Overlay[] overlays() {
	synchronized (ols) {
	    return ols.toArray(new Overlay[0]);
	}
    }

    @SuppressWarnings("unused") // For scripting api
    public String kinname() {
	if(getattr(KinInfo.class) != null) {
	    return getattr(KinInfo.class).name;
	} else {
	    return "???";
	}
    }

    @SuppressWarnings("unused") // For scripting api
    public String[] equipment() {
	if(hasTag(Tag.HUMAN)) {
	    Drawable d = getattr(Drawable.class);
	    if (d instanceof Composite) {
		Composite comp = (Composite) d;
		if(comp.lastnmod != null && comp.lastnequ != null) {
		    final List<String> equs = new ArrayList<>();
		    for (Composited.ED eq : comp.lastnequ) {
			equs.add(rnm(eq.res.res));
		    }
		    for (Composited.MD md : comp.lastnmod) {
			for (ResData rd : md.tex) {
			    equs.add(rnm(rd.res));
			}
		    }
		    return equs.toArray(new String[0]);
		} else {
		    return null;
		}
	    } else {
		return null;
	    }
	} else {
	    return null;
	}
    }

    @SuppressWarnings("unused") // For scripting api
    public String[] poses(){
	Drawable d = getattr(Drawable.class);
	if (d instanceof Composite) {
	    Composite comp = (Composite) d;
	    final List<String> poses = new ArrayList<>();

	    if (comp.oldposes != null) {
		for (ResData res : comp.oldposes) {
		    poses.add(rnm(res.res));
		}
	    }
	    if (comp.oldtposes != null) {
		for (ResData res : comp.oldtposes) {
		    poses.add(rnm(res.res));
		}
	    }
	    return poses.toArray(new String[0]);
	}
	return null;
    }

    @SuppressWarnings("unused") // For scripting api
    public boolean isFriendly() {
	final KinInfo kin = getattr(KinInfo.class);
	final int badkin = GlobalSettings.BADKIN.get();
	if (kin != null) {
	    return badkin != kin.group || (kin.isVillager() && (kin.name == null || kin.name.equals("") || kin.name.equals(" ")));
	} else {
	    return false;
	}
    }

    @SuppressWarnings("unused") // For scripting api
    public boolean isDangerous() {
        return hasTag(Tag.CAN_AGGRO) || hasTag(Tag.HUMAN);
    }


    /*
     * Details about our Gob
     */
    public String eq() {
	Drawable d = getattr(Drawable.class);
	if (d instanceof Composite) {
	    Composite comp = (Composite) d;

	    final StringBuilder sb = new StringBuilder();
	    sb.append("Equipment:");
	    if (comp.lastnequ != null)
		for (Composited.ED eq : comp.lastnequ) {
		    sb.append("\nEqu: ");
		    sb.append(rnm(eq.res.res));
		    sb.append(" @ ");
		    sb.append(eq.at);
		}

	    if (comp.lastnmod != null)
		for (Composited.MD md : comp.lastnmod) {
		    sb.append("\nMod: ");
		    sb.append(rnm(md.mod));
		    for (ResData rd : md.tex) {
			sb.append("\n  Tex: ");
			sb.append(rnm(rd.res));
		    }
		}

	    sb.append("\nPoses:");
	    if (comp.oldposes != null) {
		for (ResData res : comp.oldposes) {
		    sb.append("\nPose: ");
		    sb.append(rnm(res.res));
		}
	    }
	    if (comp.oldtposes != null) {
		for (ResData res : comp.oldtposes) {
		    sb.append("\nTPose: ");
		    sb.append(rnm(res.res));
		}
	    }
	    return sb.toString();
	}
	return "";
    }

    public String details() {
	StringBuilder sb = new StringBuilder();
	sb.append("Res: ");
	sb.append(resname().orElse(""));
	sb.append(" [");
	sb.append(id);
	sb.append("]\n");
	if (tags != null) {
	    for (final Tag t : tags) {
		sb.append("Tag: ");
		sb.append(t);
		sb.append("\n");
	    }
	} else {
	    sb.append("Tag: None\n");
	}
	for (final long hold : holding) {
	    sb.append("Holding: ");
	    sb.append(hold);
	    sb.append("\n");
	}
	if (heldby != -1) {
	    sb.append("Held By: ");
	    sb.append(heldby);
	    sb.append("\n");
	}
	ResDrawable dw = getattr(ResDrawable.class);
	sb.append("Angle: ");
	sb.append(Math.toDegrees(a));
	sb.append("\n");
	if (dw != null) {
	    sb.append("sdt: ");
	    sb.append(dw.sdtnum());
	    sb.append("\n");
	} else {
	    Composite comp = getattr(Composite.class);
	    if (comp != null) {
		sb.append(eq());
		sb.append("\n");
	    }
	}
	for (GAttrib a : attr.values()) {
	    sb.append("GAttrib: ");
	    sb.append(a);
	    sb.append("\n");
	}
	for (Overlay ol : ols) {
	    sb.append("Overlay: ");
	    sb.append(ol.spr);
	    sb.append(" - ");
	    sb.append(ol.name());
	    sb.append("\n");
	}
	sb.append("Position: ");
	sb.append(getc());
	sb.append("\n");
	return sb.toString();
    }

    /*
     * Gob Discovery and Info gathering, Occurs only once after
     * its res name has been found
     */
    public void discover(final String name) {
        //Ensure we have everything needed to do a discovery first
	final UI ui = glob.ui.get();
	final Optional<Resource> res = res();
	if(ui == null || res.isEmpty()) {
	    throw new JobSystem.DependencyNotMet();
	}
	final GameUI gui = ui.gui;
	if(gui == null) {
	    throw new JobSystem.DependencyNotMet();
	}
	final MapView map = gui.map;
	final MapWnd mapfile = gui.mapfile;
	if(map == null || mapfile == null) {
	    throw new JobSystem.DependencyNotMet();
	}
	final long plgobid = map.rlplgob;


	if (!Deleted.isDeleted(name)) {

	    final List<OCache.Delta> deltas = new ArrayList<>();
	    tags = ObjData.getTags(name);
	    hitbox = Hitbox.hbfor(this);
	    glob.gobhitmap.add(this);

	    if ((tags.contains(Tag.HUMAN) || tags.contains(Tag.ANIMAL) || name.startsWith("gfx/kritter"))
		    && !tags.contains(Tag.TAMED_ANIMAL)) {
		deltas.add((gob) -> gob.setattr(new SpeedMonitor(gob)));
	    }

	    Alerted.checkAlert(name, plgobid, this, ui);

	    //Target Sprite if targeted
	    if (id == gui.curtar) {
		deltas.add((gob) -> gob.addol(new Overlay(gob, TargetSprite.id, new TargetSprite(gob))));
	    }

	    //Add monitors and informational stuff
	    deltas.add((gob) -> gob.setattr(new HitboxMonitor(gob)));
	    deltas.add((gob) -> gob.setattr(new HighlightMonitor(gob)));
	    if (hasTag(Tag.HUMAN)) {
		deltas.add((gob) -> gob.setattr(new Halo(gob)));
		if (id == plgobid) {
		    deltas.add((gob) -> gob.setattr(new MyGobIndicator(gob)));
		}
	    }
	    if (ObjData.isACrop(name)) {
		deltas.add((gob) -> gob.setattr(new GrowthMonitor(gob, name)));
	    }
	    if (hasTag(Tag.HUMAN) || hasTag(Tag.ANIMAL)) {
		deltas.add((gob) -> gob.setattr(new ScreenLocation(gob)));
	    }
	    if(hasTag(Tag.HUMAN) || hasTag(Tag.ANIMAL) || hasTag(Tag.VEHICLE)) {
		deltas.add((gob) -> gob.setattr(new PathMonitor(gob)));
	    }

	    //Add mods if needed
	    if (name.startsWith("gfx/terobjs/dframe")) {
		deltas.add((gob) -> gob.setattr(new DryingFrameStatus(gob)));
	    } else if (name.equals("gfx/terobjs/ttub")) {
		deltas.add((gob) -> gob.setattr(new TanTubStatus(gob)));
	    } else if (name.equals("gfx/terobjs/cupboard")) {
		deltas.add((gob) -> gob.setattr(new CupboardStatus(gob)));
	    } else if (name.equals("gfx/terobjs/cheeserack")) {
		deltas.add((gob) -> gob.setattr(new CheeseRackStatus(gob)));
	    }

	    if (Hidden.isHidden(name)) {
		deltas.add((gob) -> gob.setattr(new Hidden(gob)));
	    }

	    if(ObjData.hasRange(name)) {
	        deltas.add((gob) -> gob.setattr(new RangeMonitor(gob)));
	    }

	    MarkerData.marker(name).ifPresent(mark -> mapfile.markobj(mark, rc));


	    //If we have changes to Attrs they need to be done via the GobInfo thread
	    queueDeltas(deltas);
	} else {
	    ui.sess.glob.oc.mailbox.mail(new OCache.RemoveGobById(this.id));
	}
    }

    public boolean isplayer() {
	try {
	    final UI ui = glob.ui.get();
	    final Optional<Resource> res = res();
	    if(ui == null || res.isEmpty()) {
		throw new JobSystem.DependencyNotMet();
	    }
	    final GameUI gui = ui.gui;
	    if(gui == null) {
		throw new JobSystem.DependencyNotMet();
	    }
	    final MapView map = gui.map;
	    final MapWnd mapfile = gui.mapfile;
	    if(map == null || mapfile == null) {
		throw new JobSystem.DependencyNotMet();
	    }
	    final long plgobid = map.rlplgob;
	    return plgobid == id;
	} catch (Exception e) {
	    return false;
	}
    }
}
