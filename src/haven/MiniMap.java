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
import java.awt.Color;

import hamster.GlobalSettings;
import hamster.MouseBind;
import hamster.data.map.MarkerData;
import hamster.gob.Tag;
import hamster.script.pathfinding.Move;
import hamster.ui.minimap.*;
import haven.MapFile.Segment;
import haven.MapFile.DataGrid;
import haven.MapFile.GridInfo;

import static hamster.MouseBind.MV_PATHFIND_MOVE;
import static hamster.MouseBind.MV_QUEUE_MOVE;
import static haven.MCache.cmaps;
import static haven.MCache.tilesz;
import static haven.OCache.posres;

public class MiniMap extends Widget {
    public static final Tex bg = Resource.loadtex("gfx/hud/mmap/ptex");
    public static final Tex nomap = Resource.loadtex("gfx/hud/mmap/nomap");
    public static final Tex plp = ((TexI)Resource.loadtex("gfx/hud/mmap/plp")).filter(haven.render.Texture.Filter.LINEAR);
    public final MapFile file;
    public Location curloc;
    public Location sessloc;
    public GobIcon.Settings iconconf;
    public List<DisplayIcon> icons = Collections.emptyList();
    protected Locator setloc;
    protected boolean follow;
    protected int zoomlevel = 0;
    protected DisplayGrid[] display;
    protected Area dgext, dtext;
    protected Segment dseg;
    protected int dlvl;
    protected Location dloc;

    public MiniMap(Coord sz, MapFile file) {
	super(sz);
	this.file = file;
    }

    public MiniMap(MapFile file) {
	this(Coord.z, file);
    }

    protected void attached() {
	if(iconconf == null) {
	    GameUI gui = getparent(GameUI.class);
	    if(gui != null)
		iconconf = gui.iconconf;
	}
	super.attached();
    }

    public static class Location {
	public final Segment seg;
	public final Coord tc;

	public Location(Segment seg, Coord tc) {
	    Objects.requireNonNull(seg);
	    Objects.requireNonNull(tc);
	    this.seg = seg; this.tc = tc;
	}
    }

    public interface Locator {
	Location locate(MapFile file) throws Loading;
    }

    public static class SessionLocator implements Locator {
	public final Session sess;
	private MCache.Grid lastgrid = null;
	private Location lastloc;

	public SessionLocator(Session sess) {this.sess = sess;}

	public Location locate(MapFile file) {
	    MCache map = sess.glob.map;
	    if(lastgrid != null) {
		synchronized(map.grids) {
		    if(map.grids.get(lastgrid.gc) == lastgrid)
			return(lastloc);
		}
		lastgrid = null;
		lastloc = null;
	    }
	    Collection<MCache.Grid> grids = new ArrayList<>();
	    synchronized(map.grids) {
		grids.addAll(map.grids.values());
	    }
	    for(MCache.Grid grid : grids) {
		GridInfo info = file.gridinfo.get(grid.id);
		if(info == null)
		    continue;
		Segment seg = file.segments.get(info.seg);
		if(seg != null) {
		    Location ret = new Location(seg, info.sc.sub(grid.gc).mul(cmaps));
		    lastgrid = grid;
		    lastloc = ret;
		    return(ret);
		}
	    }
	    throw(new Loading("No mapped grids found."));
	}
    }

    public static class MapLocator implements Locator {
	public final MapView mv;

	public MapLocator(MapView mv) {this.mv = mv;}

	public Location locate(MapFile file) {
	    Coord mc = new Coord2d(mv.getcc()).floor(MCache.tilesz);
	    if(mc == null)
		throw(new Loading("Waiting for initial location"));
	    MCache.Grid plg = mv.ui.sess.glob.map.getgrid(mc.div(cmaps));
	    GridInfo info = file.gridinfo.get(plg.id);
	    if(info == null)
		throw(new Loading("No grid info, probably coming soon"));
	    Segment seg = file.segments.get(info.seg);
	    if(seg == null)
		throw(new Loading("No segment info, probably coming soon"));
	    return(new Location(seg, info.sc.mul(cmaps).add(mc.sub(plg.ul))));
	}
    }

    public static class SpecLocator implements Locator {
	public final long seg;
	public final Coord tc;

	public SpecLocator(long seg, Coord tc) {this.seg = seg; this.tc = tc;}

	public Location locate(MapFile file) {
	    Segment seg = file.segments.get(this.seg);
	    if(seg == null)
		return(null);
	    return(new Location(seg, tc));
	}
    }

    public void center(Location loc) {
	curloc = loc;
    }

    public Location resolve(Locator loc) {
	if(!file.lock.readLock().tryLock())
	    throw(new Loading("Map file is busy"));
	try {
	    return(loc.locate(file));
	} finally {
	    file.lock.readLock().unlock();
	}
    }

    public Optional<Location> resolveo(final Locator loc) {
	try {
	    return Optional.ofNullable(resolve(loc));
	} catch (Loading l) {
	    return Optional.empty();
	}
    }

    public Coord xlate(Location loc) {
	Location dloc = this.dloc;
	if((dloc == null) || (dloc.seg != loc.seg))
	    return(null);
	return(loc.tc.sub(dloc.tc).div(scalef()).add(sz.div(2)));
    }

    public Location xlate(Coord sc) {
	Location dloc = this.dloc;
	if(dloc == null)
	    return(null);
	Coord tc = sc.sub(sz.div(2)).mul(scalef()).add(dloc.tc);
	return(new Location(dloc.seg, tc));
    }

    private Locator sesslocator;
    public void tick(double dt) {
	if(setloc != null) {
	    try {
		Location loc = resolve(setloc);
		center(loc);
		if(!follow)
		    setloc = null;
	    } catch(Loading l) {
	    }
	}
	if((sesslocator == null) && (ui != null) && (ui.sess != null))
	    sesslocator = new SessionLocator(ui.sess);
	if(sesslocator != null) {
	    try {
		sessloc = resolve(sesslocator);
	    } catch(Loading l) {
	    }
	}
	icons = findicons(icons);
    }

    public void center(Locator loc) {
	setloc = loc;
	follow = false;
    }

    public void follow(Locator loc) {
	setloc = loc;
	follow = true;
    }

    public class DisplayIcon {
	public final GobIcon icon;
	public final Gob gob;
	public final GobIcon.Image img;
	public Coord2d rc = null;
	public Coord sc = null;
	public double ang = 0.0;
	public Color col = Color.WHITE;
	public int z;
	public double stime;

	public DisplayIcon(GobIcon icon) {
	    this.icon = icon;
	    this.gob = icon.gob;
	    this.img = icon.img();
	    this.z = this.img.z;
	    this.stime = Utils.rtime();
	}

	public void update(Coord2d rc, double ang) {
	    this.rc = rc;
	    this.ang = ang;
	}

	public void dispupdate() {
	    if((this.rc == null) || (sessloc == null) || (dloc == null) || (dloc.seg != sessloc.seg))
		this.sc = null;
	    else
		this.sc = p2c(this.rc);
	}
    }

    public static class MarkerID extends GAttrib {
	public final long id;

	public MarkerID(Gob gob, long id) {
	    super(gob);
	    this.id = id;
	}

	public static Gob find(OCache oc, long id) {
	    synchronized(oc) {
		for(Gob gob : oc) {
		    MarkerID iattr = gob.getattr(MarkerID.class);
		    if((iattr != null) && (iattr.id == id))
			return(gob);
		}
	    }
	    return(null);
	}

	@Override
	public String toString() {
	    return "MarkerID(" + "id=" + id + ')';
	}
    }

    private enum Type {
	NATURAL, PLACED, CUSTOM, LINKED, KINGDOM, VILLAGE
    }

    public static class DisplayMarker {
	public final Marker m;
	public Text tip;
	public Area hit;
	private final Type type;

	private void checkTip(final String nm) {
	    if (tip == null || !tip.text.equals(nm)) {
		tip = Text.renderstroked(nm, Color.WHITE, Color.BLACK);
	    }
	}

	public DisplayMarker(Marker marker, final UI ui) {
	    this.m = marker;
	    checkTip(marker.tip(ui));

	    if (marker instanceof VillageMarker)
		type = Type.VILLAGE;
	    else if (marker instanceof RealmMarker)
		type = Type.KINGDOM;
	    else if (marker instanceof LinkedMarker)
		type = Type.LINKED;
	    else if (marker instanceof CustomMarker)
		type = Type.CUSTOM;
	    else if (marker instanceof PMarker)
		type = Type.PLACED;
	    else
		type = Type.NATURAL;
	}

	private boolean visible() {
	    return GlobalSettings.SHOWMMMARKERS.get() &&
		    ((type == Type.PLACED && GlobalSettings.SHOWPMARKERS.get()) ||
			    (type == Type.KINGDOM && GlobalSettings.SHOWKMARKERS.get()) ||
			    (type == Type.LINKED && GlobalSettings.SHOWLMARKERS.get()) ||
			    (type == Type.CUSTOM && GlobalSettings.SHOWCMARKERS.get()) ||
			    (type == Type.NATURAL && GlobalSettings.SHOWNMARKERS.get()) ||
			    (type == Type.VILLAGE && GlobalSettings.SHOWVMARKERS.get()));
	}

	private Area hit(final UI ui) {
	    if (visible()) {
	        if(hit == null)
	            hit = m.area();
	        return hit;
	    } else {
		return null;
	    }
	}

	public void draw(GOut g, Coord c, final float scale, final UI ui, final MapFile file) {
	    if (visible()) {
		checkTip(m.tip(ui));
		m.draw(g, c, tip, scale, file);
	    }
	}
    }

    public static class DisplayGrid {
	public final MapFile file;
	public final Segment seg;
	public final Coord sc;
	public final Area mapext;
	public final Indir<? extends DataGrid> gref;
	private DataGrid cgrid = null;
	private Tex img = null;
	private Defer.Future<Tex> nextimg = null;

	public DisplayGrid(Segment seg, Coord sc, int lvl, Indir<? extends DataGrid> gref) {
	    this.file = seg.file();
	    this.seg = seg;
	    this.sc = sc;
	    this.gref = gref;
	    mapext = Area.sized(sc.mul(cmaps.mul(1 << lvl)), cmaps.mul(1 << lvl));
	}

	class CachedImage {
	    final Function<DataGrid, Defer.Future<Tex>> src;
	    DataGrid cgrid;
	    Defer.Future<Tex> next;
	    Tex img;

	    CachedImage(Function<DataGrid, Defer.Future<Tex>> src) {
		this.src = src;
	    }

	    public Tex get() {
		DataGrid grid = gref.get();
		if(grid != cgrid) {
		    if(next != null)
			next.cancel();
		    next = src.apply(grid);
		    cgrid = grid;
		}
		if(next != null) {
		    try {
			img = next.get();
		    } catch(Loading l) {}
		}
		return(img);
	    }
	}

	private CachedImage img_c;
	public Tex img() {
	    if(img_c == null) {
		img_c = new CachedImage(grid -> {
			if(grid instanceof MapFile.ZoomGrid) {
			    return(Defer.later(() -> new TexI(grid.render(sc.mul(cmaps)))));
			} else {
			    return(Defer.later(new Defer.Callable<Tex>() {
				    MapFile.View view = new MapFile.View(seg);

				    public TexI call() {
					try(Locked lk = new Locked(file.lock.readLock())) {
					    for(int y = -1; y <= 1; y++) {
						for(int x = -1; x <= 1; x++) {
						    view.addgrid(sc.add(x, y));
						}
					    }
					    view.fin();
					    return(new TexI(MapSource.drawmap(view, Area.sized(sc.mul(cmaps), cmaps))));
					}
				    }
				}));
			}
		});
	    }
	    return(img_c.get());
	}

	private Map<String, CachedImage> olimg_c = new HashMap<>();
	public Tex olimg(String tag) {
	    CachedImage ret;
	    synchronized(olimg_c) {
		if((ret = olimg_c.get(tag)) == null)
		    olimg_c.put(tag, ret = new CachedImage(grid -> Defer.later(() -> new TexI(grid.olrender(sc.mul(cmaps), tag)))));
	    }
	    return(ret.get());
	}

	private Collection<DisplayMarker> markers = Collections.emptyList();
	private int markerseq = -1;
	public Collection<DisplayMarker> markers(boolean remark, final UI ui) {
	    if(remark && (markerseq != file.markerseq)) {
		if(file.lock.readLock().tryLock()) {
		    try {
			ArrayList<DisplayMarker> marks = new ArrayList<>();
			for(Marker mark : file.markers) {
			    if((mark.seg == this.seg.id) && mapext.contains(mark.tc))
				marks.add(new DisplayMarker(mark, ui));
			}
			marks.trimToSize();
			markers = (marks.size() == 0) ? Collections.emptyList() : marks;
			markerseq = file.markerseq;
		    } finally {
			file.lock.readLock().unlock();
		    }
		}
	    }
	    return(markers);
	}
    }

    public float scalef() {
	return(UI.unscale((float)(1 << dlvl)));
    }

    public Coord st2c(Coord tc) {
	return(UI.scale(tc.add(sessloc.tc).sub(dloc.tc).div(1 << dlvl)).add(sz.div(2)));
    }

    public Coord p2c(Coord2d pc) {
	return(st2c(pc.floor(tilesz)));
    }

    private void redisplay(Location loc) {
	Coord hsz = sz.div(2);
	Coord zmaps = cmaps.mul(1 << zoomlevel);
	Area next = Area.sized(loc.tc.sub(hsz.mul(UI.unscale((float)(1 << zoomlevel)))).div(zmaps),
	    UI.unscale(sz).div(cmaps).add(2, 2));
	if((display == null) || (loc.seg != dseg) || (zoomlevel != dlvl) || !next.equals(dgext)) {
	    DisplayGrid[] nd = new DisplayGrid[next.rsz()];
	    if((display != null) && (loc.seg == dseg) && (zoomlevel == dlvl)) {
		for(Coord c : dgext) {
		    if(next.contains(c))
			nd[next.ri(c)] = display[dgext.ri(c)];
		}
	    }
	    display = nd;
	    dseg = loc.seg;
	    dlvl = zoomlevel;
	    dgext = next;
	    dtext = Area.sized(next.ul.mul(zmaps), next.sz().mul(zmaps));
	}
	dloc = loc;
	if(file.lock.readLock().tryLock()) {
	    try {
		for(Coord c : dgext) {
		    if(display[dgext.ri(c)] == null)
			display[dgext.ri(c)] = new DisplayGrid(dloc.seg, c, dlvl, dloc.seg.grid(dlvl, c.mul(1 << dlvl)));
		}
	    } finally {
		file.lock.readLock().unlock();
	    }
	}
	for(DisplayIcon icon : icons)
	    icon.dispupdate();
    }

    public void drawgrid(GOut g, Coord ul, DisplayGrid disp) {
	try {
	    Tex img = disp.img();
	    if(img != null)
		g.image(img, ul, UI.scale(img.sz()));
	} catch(Loading l) {
	}
    }

    public void drawmap(GOut g) {
	Coord hsz = sz.div(2);
	for(Coord c : dgext) {
	    Coord ul = UI.scale(c.mul(cmaps)).sub(dloc.tc.div(scalef())).add(hsz);
	    DisplayGrid disp = display[dgext.ri(c)];
	    if(disp == null)
		continue;
	    drawgrid(g, ul, disp);
	}
    }

    public void drawgrid(final GOut g, final Location loc) {
	if (GlobalSettings.MMSHOWGRID.get()) {
	    final Coord hsz = sz.div(2);
	    //Grid view is weird due to how zoommaps work, the only guarantee is that if we have one zoommap done
	    //we know it's ul is on a grid edge. gc is the ul of SOME grid
	    //Normal grids are 100x100 boxes, factor in zoomlevels and we're closer to
	    // (100,100).div(1 << dlvl)
	    final Coord sc = UI.scale(dgext.ul.mul(cmaps).sub(loc.tc.div(1 << dlvl))).add(hsz);
	    final Coord step = UI.scale(cmaps.div(1 << dlvl));
	    Coord tlc = new Coord(sc); //Top left grid that we can see within our window view
	    while (tlc.y - step.y >= 0) tlc.y -= step.y;
	    while (tlc.x - step.x >= 0) tlc.x -= step.x;
	    g.chcolor(Color.RED);
	    //Make horizontal lines
	    for (int y = tlc.y; y < sz.y; y += step.y) {
		g.line(new Coord(0, y), new Coord(sz.x, y), 1);
	    }
	    //Make vertical lines
	    for (int x = tlc.x; x < sz.x; x += step.x) {
		g.line(new Coord(x, 0), new Coord(x, sz.y), 1);
	    }
	    g.chcolor();
	}
    }

    public void drawmarkers(GOut g) {
	Coord hsz = sz.div(2);
	for(Coord c : dgext) {
	    DisplayGrid dgrid = display[dgext.ri(c)];
	    if(dgrid == null)
		continue;
	    for(DisplayMarker mark : dgrid.markers(true, ui)) {
		if(filter(mark))
		    continue;
		mark.draw(g, mark.m.tc.sub(dloc.tc).div(scalef()).add(hsz), scalef(), ui, file);
	    }
	}
    }

    public List<DisplayIcon> findicons(Collection<? extends DisplayIcon> prev) {
	if((ui.sess == null) || (iconconf == null))
	    return(Collections.emptyList());
	Map<Gob, DisplayIcon> pmap = Collections.emptyMap();
	if(prev != null) {
	    pmap = new HashMap<>();
	    for(DisplayIcon disp : prev)
		pmap.put(disp.gob, disp);
	}
	List<DisplayIcon> ret = new ArrayList<>();
	OCache oc = ui.sess.glob.oc;
	synchronized(oc) {
	    for(Gob gob : oc) {
		try {
		    GobIcon icon = gob.getattr(GobIcon.class);
		    if(icon != null) {
			GobIcon.Setting conf = iconconf.get(icon.res.get());
			if((conf != null) && conf.show) {
			    DisplayIcon disp = pmap.get(gob);
			    if(disp == null)
				disp = new DisplayIcon(icon);
			    disp.update(gob.rc, gob.a);
			    KinInfo kin = gob.getattr(KinInfo.class);
			    if((kin != null) && (kin.group < BuddyWnd.gc.length))
				disp.col = BuddyWnd.gc[kin.group];
			    ret.add(disp);
			}
		    }
		} catch(Loading l) {}
	    }
	}
	Collections.sort(ret, (a, b) -> a.z - b.z);
	if(ret.size() == 0)
	    return(Collections.emptyList());
	return(ret);
    }

    public void drawicons(GOut g) {
	if((sessloc == null) || (dloc.seg != sessloc.seg))
	    return;
	if(GlobalSettings.SHOWMMGOBS.get()) {
	    for (DisplayIcon disp : icons) {
		if ((disp.sc == null) || filter(disp))
		    continue;
		GobIcon.Image img = disp.img;
		if (disp.col != null)
		    g.chcolor(disp.col);
		else
		    g.chcolor();
		if (!img.rot)
		    g.image(img.tex, disp.sc.sub(img.cc));
		else
		    g.rotimage(img.tex, disp.sc, img.cc, -disp.ang + img.ao);
		if(GlobalSettings.SHOWMMGOBNAMES.get() && disp.gob.hasTag(Tag.HUMAN)) {
		    final KinInfo kin = disp.gob.getattr(KinInfo.class);
		    if (kin != null) {
			g.chcolor(BuddyWnd.gc[kin.group]);
			g.image(kin.rendered(), disp.sc.sub(img.cc).sub(kin.rendered().sz().div(2, 1)));
			g.chcolor();
		    }
		}
	    }
	    g.chcolor();
	}
    }

    public void remparty() {
	Set<Gob> memb = new HashSet<>();
	synchronized(ui.sess.glob.party.memb) {
	    for(Party.Member m : ui.sess.glob.party.memb.values()) {
		Gob gob = m.getgob();
		if(gob != null)
		    memb.add(gob);
	    }
	}
	for(Iterator<DisplayIcon> it = icons.iterator(); it.hasNext();) {
	    DisplayIcon icon = it.next();
	    if(memb.contains(icon.gob))
		it.remove();
	}
    }

    private void drawparty(final GOut g) {
	synchronized(ui.sess.glob.party.memb) {
	    for(Party.Member m : ui.sess.glob.party.memb.values()) {
		try {
		    Coord2d ppc = m.getc();
		    if(ppc == null)
			continue;
		    g.chcolor(m.col.getRed(), m.col.getGreen(), m.col.getBlue(), 255);
		    g.rotimage(plp, p2c(ppc), plp.sz().div(2), -m.geta() - (Math.PI / 2));
		    if(GlobalSettings.SHOWMMGOBNAMES.get()) {
			final Gob gob = m.getgob();
			final KinInfo kin = gob != null ? gob.getattr(KinInfo.class) : null;
			if (kin != null) {
			    g.image(kin.rendered(), p2c(ppc).sub(plp.sz().div(2)).sub(kin.rendered().sz().div(2, 1)));
			}
		    }
		    g.chcolor();
		} catch(Loading ignored) {}
	    }
	}
    }

    public void drawparts(GOut g, final Location loc){
	drawmap(g);
	drawmarkers(g);
	drawicons(g);
	drawgrid(g, loc);
	drawparty(g);
    }

    public void draw(GOut g) {
	Location loc = this.curloc;
	if(loc == null)
	    return;
	redisplay(loc);
	remparty();
	drawparts(g, loc);
    }

    private static boolean hascomplete(DisplayGrid[] disp, Area dext, Coord c) {
	DisplayGrid dg = disp[dext.ri(c)];
	if(dg == null)
	    return(false);
	return(dg.gref.get() != null);
    }

    protected boolean allowzoomout() {
	DisplayGrid[] disp = this.display;
	Area dext = this.dgext;
	try {
	    for(int x = dext.ul.x; x < dext.br.x; x++) {
		if(hascomplete(disp, dext, new Coord(x, dext.ul.y)) ||
		   hascomplete(disp, dext, new Coord(x, dext.br.y - 1)))
		    return(true);
	    }
	    for(int y = dext.ul.y; y < dext.br.y; y++) {
		if(hascomplete(disp, dext, new Coord(dext.ul.x, y)) ||
		   hascomplete(disp, dext, new Coord(dext.br.x - 1, y)))
		    return(true);
	    }
	} catch(Loading l) {
	    return(false);
	}
	return(false);
    }

    public DisplayIcon iconat(Coord c) {
	for(ListIterator<DisplayIcon> it = icons.listIterator(icons.size()); it.hasPrevious();) {
	    DisplayIcon disp = it.previous();
	    GobIcon.Image img = disp.img;
	    if((disp.sc != null) && c.isect(disp.sc.sub(img.cc), img.tex.sz()) && !filter(disp))
		return(disp);
	}
	return(null);
    }

    public DisplayMarker findmarker(long id) {
	for(DisplayGrid dgrid : display) {
	    if(dgrid == null)
		continue;
	    for(DisplayMarker mark : dgrid.markers(false, ui)) {
		if((mark.m instanceof SMarker) && (((SMarker)mark.m).oid == id))
		    return(mark);
	    }
	}
	return(null);
    }

    public DisplayMarker markerat(Coord tc) {
	for(DisplayGrid dgrid : display) {
	    if(dgrid == null)
		continue;
	    for(DisplayMarker mark : dgrid.markers(false, ui)) {
		if((mark.hit(ui) != null) && mark.hit(ui).contains(tc.sub(mark.m.tc).div(scalef())) && !filter(mark))
		    return(mark);
	    }
	}
	return(null);
    }

    public boolean filter(DisplayIcon icon) {
	MarkerID iattr = icon.gob.getattr(MarkerID.class);
	if((iattr != null) && (findmarker(iattr.id) != null))
	    return(true);
	return(false);
    }

    public boolean filter(DisplayMarker marker) {
	return(false);
    }

    public boolean clickloc(Location loc, int button, boolean press) {
	return(false);
    }

    public boolean clickicon(DisplayIcon icon, Location loc, int button, boolean press) {
	return(false);
    }

    public boolean clickmarker(DisplayMarker mark, Location loc, int button, boolean press) {
	return(false);
    }

    private UI.Grab drag;
    private boolean dragging;
    private Coord dsc, dmc;
    public boolean dragp(int button) {
	return(button == 1);
    }

    private Location dsloc;
    private DisplayIcon dsicon;
    private DisplayMarker dsmark;
    public boolean mousedown(Coord c, int button) {
	dsloc = xlate(c);
	if(dsloc != null) {
	    dsicon = iconat(c);
	    dsmark = markerat(dsloc.tc);
	    if((dsicon != null) && clickicon(dsicon, dsloc, button, true))
		return(true);
	    if((dsmark != null) && clickmarker(dsmark, dsloc, button, true))
		return(true);
	    if(clickloc(dsloc, button, true))
		return(true);
	} else {
	    dsloc = null;
	    dsicon = null;
	    dsmark = null;
	}
	if(dragp(button)) {
	    Location loc = curloc;
	    if((drag == null) && (loc != null)) {
		drag = ui.grabmouse(this);
		dsc = c;
		dmc = loc.tc;
		dragging = false;
	    }
	    return(true);
	}
	return(super.mousedown(c, button));
    }

    public void mousemove(Coord c) {
	if(drag != null) {
	    if(dragging) {
		setloc = null;
		follow = false;
		curloc = new Location(curloc.seg, dmc.add(dsc.sub(c).mul(scalef())));
	    } else if(c.dist(dsc) > 5) {
		dragging = true;
	    }
	}
	super.mousemove(c);
    }

    public boolean mouseup(Coord c, int button) {
	if((drag != null) && (button == 1)) {
	    drag.remove();
	    drag = null;
	}
	release: if(!dragging && (dsloc != null)) {
	    if((dsicon != null) && clickicon(dsicon, dsloc, button, false))
		break release;
	    if((dsmark != null) && clickmarker(dsmark, dsloc, button, false))
		break release;
	    if(clickloc(dsloc, button, false))
		break release;
	}
	dsloc = null;
	dsicon = null;
	dsmark = null;
	dragging = false;
	return(super.mouseup(c, button));
    }

    public boolean mousewheel(Coord c, int amount) {
	if(amount > 0) {
	    zoomlevel = Math.min(Math.min(zoomlevel + 1, dlvl + 1), 6);
	} else {
	    zoomlevel = Math.max(zoomlevel - 1, 0);
	}
	return(true);
    }

    public Object tooltip(Coord c, Widget prev) {
	if(dloc != null) {
	    Coord tc = c.sub(sz.div(2)).mul(scalef()).add(dloc.tc);
	    DisplayMarker mark = markerat(tc);
	    if(mark != null) {
		return(mark.tip);
	    } else if (GlobalSettings.SHOWHOVERTOOLTIPS.get()) {
		final Coord gc = tc.div(cmaps);
		try {
		    return tc + " - " + gc + " - " + curloc.seg.gridid(gc) + " [" + curloc.seg.gridtilename(tc, gc) + "]";
		} catch (Exception e) {
		    //Ignore
		}
	    }
	}
	return(super.tooltip(c, prev));
    }

    //TODO Would be nice to do pathfinding but interact with gob once at location
    public void mvclick(MapView mv, Coord mc, Location loc, Gob gob, int button) {
        final String bind = MouseBind.generateSequence(ui, button);
	if(mc == null) mc = ui.mc;
	if((sessloc != null) && (sessloc.seg == loc.seg)) {
	    if(MV_QUEUE_MOVE.match(bind)) {
		ui.gui.map.queuemove(new Move(loc.tc.sub(sessloc.tc).mul(tilesz).add(tilesz.div(2))));
	    } else if(MV_PATHFIND_MOVE.match(bind)) {
	        if(gob == null)
	            ui.gui.map.pathto(loc.tc.sub(sessloc.tc).mul(tilesz).add(tilesz.div(2)));
	        else
	            ui.gui.map.pathto(gob);
	    } else if(gob == null) {
		mv.wdgmsg("click", mc,
			loc.tc.sub(sessloc.tc).mul(tilesz).add(tilesz.div(2)).floor(posres),
			button, ui.modflags());
	    } else {
		mv.wdgmsg("click", mc,
			loc.tc.sub(sessloc.tc).mul(tilesz).add(tilesz.div(2)).floor(posres),
			button, ui.modflags(), 0,
			(int)gob.id,
			gob.rc.floor(posres),
			0, -1);
	    }
	}
    }
}
