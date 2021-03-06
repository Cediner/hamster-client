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

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.nio.channels.*;
import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.function.Consumer;

import com.google.common.flogger.FluentLogger;
import hamster.GlobalSettings;
import hamster.IndirSetting;
import hamster.KeyBind;
import hamster.MouseBind;
import hamster.data.DirMap;
import hamster.data.map.MarkerData;
import hamster.script.pathfinding.Move;
import hamster.script.pathfinding.waypoint.WaypointPathfinder;
import hamster.ui.DowseWnd;
import hamster.ui.minimap.MapMarkerWnd;
import hamster.ui.core.ResizableWnd;
import hamster.ui.core.Theme;
import hamster.ui.minimap.*;
import haven.MiniMap.*;
import integrations.mapv4.MapConfig;
import integrations.mapv4.MappingClient;

import static hamster.KeyBind.*;
import static haven.MCache.tilesz;
import static haven.MCache.cmaps;
import static haven.Utils.eq;
import javax.swing.JFileChooser;
import javax.swing.filechooser.*;

//TODO: Readd export / import via window buttons
public class MapWnd extends ResizableWnd implements Console.Directory {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final Resource markcurs = Resource.local().loadwait("gfx/hud/curs/flag");
    public static final Resource wpconcurs = Resource.local().loadwait("custom/curs/wpconnect");
    public static final Resource wpdisconcurs = Resource.local().loadwait("custom/curs/wpdisconnect");
    private static final Tex viewbox = Theme.tex("buttons/wnd/view", 3);
    public final MapFile file;
    public final MiniMap view;
    public final MapView mv;
    public final Collection<String> overlays = new java.util.concurrent.CopyOnWriteArraySet<>();
    public boolean hmarkers = false;
    public final Locator player;
    private final Widget toolbar;
    private final Frame viewf;
    private MapMarkerWnd markers = null;
    private boolean domark = false;
    private boolean dowpconnect = false, dowpcont = false;
    private boolean dowpdisconnect = false;
    private final Collection<Runnable> deferred = new LinkedList<>();
    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();

    public MapWnd(MapFile file, MapView mv, Coord sz, String title) {
	super(sz, title, true);
	this.file = file;
	this.mv = mv;
	this.player = new MapLocator(mv);
	viewf = add(new ViewFrame());
	view = viewf.add(new View(file, mv));
	recenter();
	toolbar = add(new Widget(Coord.z));
	toolbar.add(new Img(Resource.loadtex("gfx/hud/mmap/fgwdg")), Coord.z);
	toolbar.add(new IButton("gfx/hud/mmap/home", "", "-d", "-h") {
		{settip("Follow");}
		public void click() {
		    recenter();
		}
	    }, Coord.z);
	toolbar.add(new ICheckBox("gfx/hud/mmap/mark", "", "-d", "-h", "-dh"), Coord.z)
	    .state(() -> domark).set(a -> domark = a)
	    .settip("Add marker");
	toolbar.add(new ICheckBox("gfx/hud/mmap/hmark", "", "-d", "-h", "-dh"))
	    .state(() -> hmarkers).set(a -> hmarkers = a)
	    .settip("Hide markers");
	final var chk = toolbar.add(new ICheckBox("gfx/hud/lbtn-ico", "", "-d", "-h", "-dh"))
		.state(() -> ui.gui != null && ui.gui.wndstate(ui.gui.iconwnd))
		.click(() -> {
		    if(ui.gui.iconconf == null)
			return;
		    if(ui.gui.iconwnd == null) {
			ui.gui.iconwnd = new GobIcon.SettingsWindow(ui.gui.iconconf, () -> Utils.defer(ui.gui::saveiconconf));
			ui.gui.add(ui.gui.iconwnd);
		    } else {
			ui.destroy(ui.gui.iconwnd);
			ui.gui.iconwnd = null;
		    }
		}).settip("Icon settings");
	toolbar.add(new ICheckBox("gfx/hud/mmap/prov", "", "-d", "-h", "-dh"))
		.changed(a -> toggleol("realm", a))
		.settip("Display provinces");
	chk.move(new Coord(UI.scale(49), -UI.scale(18)));
	toolbar.pack();

	makeHidable();

	binds.put(KB_RECALL_MAP_ONE, () -> { recall(GlobalSettings.MMMEMSIZEONE, GlobalSettings.MMMEMPOSONE); return true; });
	binds.put(KB_RECALL_MAP_TWO, () -> { recall(GlobalSettings.MMMEMSIZETWO, GlobalSettings.MMMEMPOSTWO); return true; });
	binds.put(KB_MAP_HOME, () -> {recenter(); return true;});
	binds.put(KB_MAP_MARK, () -> {domark = !domark; return true; });
	binds.put(KB_MAP_HIDE_MARKERS, () -> {hmarkers = !hmarkers; return true;});
	binds.put(KB_MAP_COMPACT, () -> {toggleHide(); return true;});

	resize(sz);
    }

    public void toggleol(String tag, boolean a) {
	if(a)
	    overlays.add(tag);
	else
	    overlays.remove(tag);
    }

    @Override
    protected void added() {
	super.added();
	setVisible(GlobalSettings.SHOWMINIMAP.get());
	addBtn(new ICheckBox("buttons/wnd/view", "Toggle view range"))
		.state(GlobalSettings.MMSHOWVIEW::get)
		.changed(GlobalSettings.MMSHOWVIEW::set)
		.set(GlobalSettings.MMSHOWVIEW.get());
	addBtn(new ICheckBox("buttons/wnd/grid", "Toggle grid on minimap"))
		.state(GlobalSettings.MMSHOWGRID::get)
		.changed(GlobalSettings.MMSHOWGRID::set)
		.set(GlobalSettings.MMSHOWGRID.get());
	addBtn("buttons/wnd/markers", "Open Markers list", () -> ui.gui.mapmarkers.toggleVisibility());
	addBtn(new ICheckBox("buttons/wnd/realm", "Show Kingdom Claims"))
		.state(GlobalSettings.SHOWKCLAIM::get)
		.set(a -> {
		    GlobalSettings.SHOWKCLAIM.set(a);
		    MapView.MessageBus.send(new MapView.ToggleOverlay("realm", a));
		});
	addBtn(new ICheckBox("buttons/wnd/vclaim", "Show Village Claims"))
		.state(GlobalSettings.SHOWVCLAIM::get)
		.set(a -> {
		    GlobalSettings.SHOWVCLAIM.set(a);
		    MapView.MessageBus.send(new MapView.ToggleOverlay("vlg", a));
		});
	addBtn(new ICheckBox("buttons/wnd/claim", "Show Personal Claims"))
		.state(GlobalSettings.SHOWPCLAIM::get)
		.set(a -> {
		    GlobalSettings.SHOWPCLAIM.set(a);
		    MapView.MessageBus.send(new MapView.ToggleOverlay("cplot", a));
		});

	addBtn("buttons/wnd/two", "2nd remembered window size",
		() -> recall(GlobalSettings.MMMEMSIZETWO, GlobalSettings.MMMEMPOSTWO),
		() -> remember(GlobalSettings.MMMEMSIZETWO, GlobalSettings.MMMEMPOSTWO));
	addBtn("buttons/wnd/one", "1st remembered window size",
		() -> recall(GlobalSettings.MMMEMSIZEONE, GlobalSettings.MMMEMPOSONE),
		() -> remember(GlobalSettings.MMMEMSIZEONE, GlobalSettings.MMMEMPOSONE));
	IButton geoloc = new IButton("custom/geoloc/geoloc", "", "", "") {
	    private Coord2d locatedAC = null;
	    private Coord2d detectedAC = null;
	    private BufferedImage green = Resource.loadimg("custom/geoloc/geoloc-green");
	    private BufferedImage red = Resource.loadimg("custom/geoloc/geoloc-red");
	    private BufferedImage up  = Resource.loadimg("custom/geoloc/geoloc");

	    private int state = 0;

	    @Override
	    public Object tooltip(Coord c, Widget prev) {
		if (this.locatedAC != null) {
		    tooltip = Text.render("Located absolute coordinates: " + this.locatedAC.toGridCoordinate());
		} else if (this.detectedAC != null) {
		    tooltip = Text.render("Detected login absolute coordinates: " + this.detectedAC.toGridCoordinate());
		} else {
		    tooltip = Text.render("Unable to determine your current location.");
		}
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    if (MapConfig.loadMapSetting(ui.sess.username, "mapper")) {
			MappingClient.MapRef mr = MappingClient.getInstance(ui.sess.username).lastMapRef;
			if (mr != null) {
			    tooltip = Text.render("Coordinates: " + mr);
			}
		    }
		}
		return super.tooltip(c, prev);
	    }

	    @Override
	    public void click() {
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    if (MapConfig.loadMapSetting(ui.sess.username, "mapper")) {
			MappingClient.MapRef mr = MappingClient.getInstance(ui.sess.username).GetMapRef(true);
			if (mr != null) {
			    MappingClient.getInstance(ui.sess.username).OpenMap(mr);
			    return;
			}
		    }
		}
	    }

	    @Override
	    public void draw(GOut g) {
		boolean redraw = false;
		if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
		    if (MapConfig.loadMapSetting(ui.sess.username, "mapper")) {
			MappingClient.MapRef mr = MappingClient.getInstance(ui.sess.username).lastMapRef;
			if (state != 2 && mr != null) {
			    state = 2;
			    redraw = true;
			}
			if (state != 0 && mr == null) {
			    state = 0;
			    redraw = true;
			}
		    }
		}
		if (redraw) this.redraw();
		super.draw(g);
	    }

	    @Override
	    public void draw(BufferedImage buf) {
		Graphics2D g = (Graphics2D) buf.getGraphics();
		if (state == 2) {
		    g.drawImage(green, 0, 0, null);
		} else if (state == 1) {
		    g.drawImage(red, 0, 0, null);
		} else {
		    g.drawImage(up, 0, 0, null);
		}
		g.dispose();
	    }
	};
	add(geoloc);
	btns.add(geoloc);
	pack();
    }

    @Override
    public void toggleVisibility() {
	super.toggleVisibility();
	GlobalSettings.SHOWMINIMAP.set(visible);
    }

    @Override
    public void close() {
	hide();
	GlobalSettings.SHOWMINIMAP.set(false);
    }

    private void remember(final IndirSetting<Coord> size, final IndirSetting<Coord> pos) {
	size.set(asz);
	pos.set(c);
    }

    public void recall(final IndirSetting<Coord> size, final IndirSetting<Coord> pos) {
	resize(size.get());
	move(pos.get());
	savePosition();
	saveSize();
	Utils.setprefc("wndsz-map", asz);
	savePosition();
    }

    @Override
    public boolean globtype(char key, KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
        return super.globtype(key, ev);
    }

    private class ViewFrame extends Frame {
	Coord sc = Coord.z;

	ViewFrame() {
	    super(Coord.z, true);
	}

	public void resize(Coord sz) {
	    super.resize(sz);
	    sc = sz.sub(box.bisz()).add(box.btloff());
	}

	public void draw(GOut g) {
	    super.draw(g);
	}

	private UI.Grab drag;
	private Coord dragc;
	public boolean mousedown(Coord c, int button) {
	    Coord cc = c.sub(sc);
	    return(super.mousedown(c, button));
	}

	public void mousemove(Coord c) {
	    if(drag != null) {
		Coord nsz = parentpos(MapWnd.this, c).add(dragc);
		nsz.x = Math.max(nsz.x, UI.scale(150));
		nsz.y = Math.max(nsz.y, UI.scale(150));
		MapWnd.this.resize(nsz);
	    }
	    super.mousemove(c);
	}

	public boolean mouseup(Coord c, int button) {
	    if((button == 1) && (drag != null)) {
		drag.remove();
		drag = null;
		return(true);
	    }
	    return(super.mouseup(c, button));
	}
    }

    private class View extends MiniMap {
        private WaypointMarker src = null;

	View(MapFile file, MapView mv) {
	    super(file, mv);
	}

	public void drawgrid(GOut g, Coord ul, DisplayGrid disp) {
	    super.drawgrid(g, ul, disp);
	    for(String tag : overlays) {
		try {
		    Tex img = disp.olimg(tag);
		    if(img != null) {
			g.chcolor(255, 255, 255, 64);
			g.image(img, ul, UI.scale(img.sz()));
		    }
		} catch(Loading ignored) {}
	    }
	    g.chcolor();
	}

	public void drawmarkers(GOut g) {
	    if(!hmarkers)
		super.drawmarkers(g);
	}

	public boolean clickmarker(final DisplayMarker mark, Location loc, int button, boolean press) {
	    final var bind = MouseBind.generateSequence(ui, button);
	    if(MouseBind.MM_EXAMINE_MARKER.match(bind)) {
		if(!press && !domark && !dowpconnect && !dowpdisconnect) {
		    ui.gui.mapmarkers.list.change(mark.m);
		    return(true);
		} else if(!press && dowpconnect && mark.m instanceof WaypointMarker && src.seg == mark.m.seg) {
		    final var dest = (WaypointMarker) mark.m;
		    src.links.add(dest.id);
		    dest.links.add(src.id);
		    file.update(src);
		    file.update(dest);
		    if(dowpcont)
		        src = dest;
		    return true;
		} else if(!press && dowpdisconnect && mark.m instanceof WaypointMarker && src.seg == mark.m.seg) {
		    final var dest = (WaypointMarker) mark.m;
		    src.links.remove(dest.id);
		    dest.links.remove(src.id);
		    file.update(src);
		    file.update(dest);
		    return true;
		}
	    } else if(MouseBind.MM_GOTO_MARKER.match(bind)) {
	        if(press) {
		    if (mark.m instanceof SMarker) {
			Gob gob = MarkerID.find(ui.sess.glob.oc, ((SMarker) mark.m).oid);
			if (gob != null) {
			    mvclick(mv, null, loc, gob, button);
			    return (true);
			}
		    }
		}
	    } else if(MouseBind.MM_FOLLOW_LINK.match(bind) && mark.m instanceof LinkedMarker && ((LinkedMarker) mark.m).lid != LinkedMarker.NOLINK) {
		if (!press) {
		    final Marker target = view.file.lmarkers.get(((LinkedMarker) mark.m).lid);
		    if (target != null) {
			view.center(new MiniMap.SpecLocator(target.seg, target.tc));
		    }
		    return (true);
		}
	    } else if(MouseBind.MM_SPECIAL_MENU.match(bind)) {
		if (!press) {
		    final var opts = new HashMap<String, Consumer<String>>();
		    opts.put("Remove", (opt) -> file.remove(mark.m));
		    if(mark.m instanceof WaypointMarker) {
		        opts.put("Create connections", (opt) -> {
		            dowpconnect = true;
		            src = (WaypointMarker) mark.m;
			});
		        opts.put("Create continuous connections", (opt) -> {
		            dowpconnect = true;
		            dowpcont = true;
		            src = (WaypointMarker) mark.m;
			});
		        opts.put("Remove connections", (opt) -> {
			    dowpdisconnect = true;
			    src = (WaypointMarker) mark.m;
			});
		        opts.put("Path to waypoint", (opt) -> {
		            try {
				final var pc = ui.gui.map.player().rc;
				resolveo(player).ifPresent(ploc -> {
				    if(ploc.seg.id == mark.m.seg) {
					final var map = file.generateWaypointMap(ploc.seg, (WaypointMarker) mark.m,
						ploc.tc, pc);
					final var pathfinder = new WaypointPathfinder(map, map.start, map.goal);
					final var moves = pathfinder.path();
					if(moves != null) {
					    ui.gui.map.clearmovequeue();
					    for(final var mv : moves) {
					        ui.gui.map.queuemove(mv);
					    }
					} else {
					    ui.gui.error("Couldn't find path to waypoint");
					}
				    } else {
				        ui.gui.error("Can't path to waypoints on different map segments than player");
				    }
				});
			    } catch (Exception e) {
				logger.atWarning().withCause(e).log("Failed to path to waypoint");
			    }
			});
		        opts.put("Select for script", (opt) -> {
		            ui.sess.details.context.dispatchmsg(this, "waypoint-select", mark.m);
			});
		    }
		    ui.gui.add(new FlowerMenu(opts), ui.mc);
		    return true;
		}
	    }
	    return(false);
	}

	public boolean clickicon(DisplayIcon icon, Location loc, int button, boolean press) {
	    if(!press && !domark && !dowpconnect && !dowpdisconnect) {
		mvclick(mv, null, loc, icon.gob, button);
		return(true);
	    }
	    return(false);
	}

	public boolean clickloc(Location loc, int button, boolean press) {
	    if(domark && (button == 1) && !press) {
		Marker nm = new PMarker(loc.seg.id, loc.tc, "New marker", BuddyWnd.gc[new Random().nextInt(BuddyWnd.gc.length)]);
		file.add(nm);
		ui.gui.mapmarkers.list.change(nm);
		domark = false;
		uploadMarks();
		return(true);
	    }
	    if(!press && (sessloc != null) && (loc.seg == sessloc.seg)) {
		mvclick(mv, null, loc, null, button);
		return(true);
	    }
	    return(false);
	}

	public boolean mousedown(Coord c, int button) {
	    if(domark && (button == 3)) {
		domark = false;
		return(true);
	    } else if(dowpconnect && button == 3) {
	        dowpconnect = false;
	        dowpcont = false;
	        return true;
	    } else if(dowpdisconnect && button == 3) {
	        dowpdisconnect = false;
	        return true;
	    }
	    super.mousedown(c, button);
	    return(true);
	}

	private Optional<Coord> xlateo(final Location loc) {
	    return Optional.ofNullable(xlate(loc));
	}

	private void drawview(final GOut g, final Coord ploc) {
	    if (GlobalSettings.MMSHOWVIEW.get()) {
		final Coord vsz = viewbox.sz().div(UI.scale(scalef()));
		g.image(viewbox, ploc.sub(vsz.div(2)), vsz);
	    }
	}

	private void drawTracking(GOut g, final Location ploc) {
	    final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
	    final double dist = 90000.0D;
	    if(ui.gui != null) {
		synchronized (ui.gui.dowsewnds) {
		    for (final DowseWnd wnd : ui.gui.dowsewnds) {
			final Coord mc = new Coord2d(wnd.startc).floor(tilesz);
			final Coord lc = mc.add((int) (Math.cos(Math.toRadians(wnd.a1())) * dist), (int) (Math.sin(Math.toRadians(wnd.a1())) * dist));
			final Coord rc = mc.add((int) (Math.cos(Math.toRadians(wnd.a2())) * dist), (int) (Math.sin(Math.toRadians(wnd.a2())) * dist));
			final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
			final Coord mlc = xlate(new Location(ploc.seg, ploc.tc.add(lc.sub(pc))));
			final Coord mrc = xlate(new Location(ploc.seg, ploc.tc.add(rc.sub(pc))));
			if (gc != null && mlc != null && mrc != null) {
			    g.chcolor(Color.MAGENTA);
			    g.dottedline(gc, mlc, 1);
			    g.dottedline(gc, mrc, 1);
			    g.chcolor();
			}
		    }
		}
	    }
	}


	/**
	 * Ideally this will be a line -> X -> line -> X
	 * Where X is some icon for destinations
	 * Start at map.moveto
	 * Then follow map.movequeue
	 * XXX: does it need an icon?
	 */
	private void drawmovement(GOut g, final Location ploc) {
	    synchronized (mv.movequeue) {
		final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
		final Move movingto = mv.movingto();
		final Iterator<Move> queue = mv.movequeue();
		Coord last;
		if (movingto != null) {
		    //Make the line first
		    g.chcolor(GlobalSettings.MMPATHCOL.get());
		    final Coord cloc = xlate(ploc);
		    last = xlate(new Location(ploc.seg, ploc.tc.add(movingto.dest().floor(tilesz).sub(pc))));
		    if (last != null && cloc != null) {
			g.dottedline(cloc, last, 2);
			if (queue.hasNext()) {
			    while (queue.hasNext()) {
				final Coord next = xlate(new Location(ploc.seg, ploc.tc.add(queue.next().dest().floor(tilesz).sub(pc))));
				if (next != null) {
				    g.dottedline(last, next, 2);
				    last = next;
				} else {
				    break;
				}
			    }
			}
		    }
		}
	    }
	}

	public void draw(GOut g) {
	    g.chcolor(0, 0, 0, 128);
	    g.frect(Coord.z, sz);
	    g.chcolor();
	    //Draw map, icons, grid
	    super.draw(g);

	    //Draw anything relative to player
	    try {
		resolveo(player).ifPresent(loc ->
			xlateo(loc).ifPresent(ploc -> {
			    //Draw our view
			    drawview(g, ploc);
			    //Draw out queued moves if any
			    drawmovement(g.reclip(view.c, view.sz), loc);
			    //Draw tracking
			    drawTracking(g, loc);
			    //Draw nearest thingwall name
			    file.getNearestThingwall(loc).ifPresent(mark ->
				    FastText.asprintf(g, sz.sub(Window.wbox.bbroff().x, 0), 1.0, 1.0,
					    "%s - %s %d", mark.nm,
					    DirMap.anglename(loc.tc.angle(mark.tc)),
					    Math.round(loc.tc.dist(mark.tc))));
		}));
	    } catch (Loading ignored){}
	}

	public Resource getcurs(Coord c) {
	    if(domark)
		return(markcurs);
	    else if(dowpconnect)
	        return wpconcurs;
	    else if(dowpdisconnect)
	        return wpdisconcurs;
	    return(super.getcurs(c));
	}
    }

    public void tick(double dt) {
	super.tick(dt);
	synchronized(deferred) {
	    for(Iterator<Runnable> i = deferred.iterator(); i.hasNext();) {
		Runnable task = i.next();
		try {
		    task.run();
		} catch(Loading l) {
		    continue;
		}
		i.remove();
	    }
	}
    }

    public void resize(Coord sz) {
        sz = new Coord(Math.max(sz.x, UI.scale(350)), Math.max(sz.y, UI.scale(150)));
	super.resize(sz);
	viewf.resize(sz);
	view.resize(viewf.inner());
	toolbar.c = viewf.c.add(0, viewf.sz.y - toolbar.sz.y).add(UI.scale(2), UI.scale(-2));
    }

    public void recenter() {
	view.follow(player);
    }

    protected void drawframe(GOut g) {
	super.drawframe(g);
    }

    @SuppressWarnings("unused") // For scripting API
    public void mark(final String nm, final Color col, final Coord2d mc) {
        final MarkerData.Marker marker = MarkerData.scriptmarker;
	synchronized (deferred) {
	    deferred.add(() -> {
	        final Coord2d prc = ui.sess.glob.oc.getgob(ui.gui.map.rlplgob).rc;
	        final Coord offset = mc.sub(prc).floor(tilesz);
		view.resolveo(player).ifPresent(loc -> {
		    if (!view.file.lock.writeLock().tryLock())
			throw (new Loading());
		    try {
			final Marker mark = new CustomMarker(loc.seg.id, loc.tc.add(offset), nm, col,
				new Resource.Spec(Resource.remote(), marker.res));
			view.file.add(mark);
			uploadMarks();
		    } finally {
			view.file.lock.writeLock().unlock();
		    }
		});
	    });
	}
    }

    @SuppressWarnings("unused") // For scripting API with custom icons
    public void mark(final String icon, final String nm, final Color col, final Coord2d mc) {
	synchronized (deferred) {
	    deferred.add(() -> {
		final Coord2d prc = ui.sess.glob.oc.getgob(ui.gui.map.rlplgob).rc;
		final Coord offset = mc.sub(prc).floor(tilesz);
		view.resolveo(player).ifPresent(loc -> {
		    if (!view.file.lock.writeLock().tryLock())
			throw (new Loading());
		    try {
			final Marker mark = new CustomMarker(loc.seg.id, loc.tc.add(offset), nm, col,
				new Resource.Spec(Resource.remote(), icon));
			view.file.add(mark);
			uploadMarks();
		    } finally {
			view.file.lock.writeLock().unlock();
		    }
		});
	    });
	}
    }

    public void markWaypoint(final Coord2d mc) {
        final var pl = ui.gui.map.player();
        if(pl != null) {
	    view.resolveo(player).ifPresent(loc -> {
	        final var wid = view.file.nextWaypointID();
		synchronized (deferred) {
		    deferred.add(() -> {
			final Coord2d prc = pl.rc;
			final Coord offset = mc.sub(prc).floor(tilesz);
			final Marker mark = new WaypointMarker(loc.seg.id, loc.tc.add(offset),
				"Waypoint", Color.WHITE, new Resource.Spec(Resource.remote(), MarkerData.waypointmarker.res),
				wid);
			view.file.add(mark);
		    });
		}
	    });
	}
    }

    void markobj(MarkerData.Marker marker, Coord2d mc) {
	if (marker instanceof MarkerData.LinkedMarker) {
	    markobj((MarkerData.LinkedMarker) marker, mc);
	} else {
	    synchronized (deferred) {
		deferred.add(() -> {
		    final Coord tc = mc.floor(tilesz);
		    MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
		    if (!view.file.lock.writeLock().tryLock())
			throw (new Loading());
		    try {
			MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
			if (info == null)
			    throw (new Loading());
			Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
			//Check for duplicate
			for (final Marker mark : view.file.markers) {
			    if (marker.type == MarkerData.Type.CUSTOM && mark instanceof CustomMarker &&
				    mark.seg == info.seg && sc.equals(mark.tc))
				return; //Duplicate
			    else if (marker.type == MarkerData.Type.REALM && mark instanceof RealmMarker &&
				    mark.seg == info.seg && sc.equals(mark.tc))
				return; //Duplicate
			    else if (marker.type == MarkerData.Type.VILLAGE && mark instanceof VillageMarker &&
				    mark.seg == info.seg && sc.equals(mark.tc))
				return; //Duplicate
			}

			final Marker mark;
			if (marker.type == MarkerData.Type.CUSTOM) {
			    mark = new CustomMarker(info.seg, sc, marker.defname,
				    Color.WHITE, new Resource.Spec(Resource.remote(), marker.res));
			} else if (marker.type == MarkerData.Type.REALM) {
			    mark = new RealmMarker(info.seg, sc, marker.defname,
				    new Resource.Spec(Resource.remote(), marker.res),
				    "???");
			    //TODO: Auto name realm based off buff
			} else {
			    //Village
			    mark = new VillageMarker(info.seg, sc, marker.defname,
				    new Resource.Spec(Resource.remote(), marker.res), ui.gui.curvil);
			}
			view.file.add(mark);
			uploadMarks();
		    } finally {
			view.file.lock.writeLock().unlock();
		    }
		});
	    }
	}
    }

    private void markobj(MarkerData.LinkedMarker marker, Coord2d mc) {
	synchronized (deferred) {
	    deferred.add(() -> {
		final Coord tc = mc.floor(tilesz);
		MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
		if (!view.file.lock.writeLock().tryLock())
		    throw (new Loading());
		try {
		    MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
		    if (info == null)
			throw (new Loading());
		    Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
		    //Check for duplicate
		    for (final Marker mark : view.file.markers) {
			if (mark instanceof LinkedMarker && mark.seg == info.seg && sc.equals(mark.tc))
			    return; //Duplicate
		    }

		    final Marker mark = new LinkedMarker(info.seg, sc, marker.defname,
			    Color.WHITE, new Resource.Spec(Resource.remote(), marker.res), view.file.markerids.next(), marker.ltype);
		    view.file.add(mark);
		    uploadMarks();
		} finally {
		    view.file.lock.writeLock().unlock();
		}
	    });
	}
    }

    public void markobj(long gobid, long oid, Indir<Resource> resid, String nm) {
	synchronized(deferred) {
	    deferred.add(new Runnable() {
		    double f = 0;
		    public void run() {
			Resource res = resid.get();
			String rnm = nm;
			if(rnm == null) {
			    Resource.Tooltip tt = res.layer(Resource.tooltip);
			    if(tt == null)
				return;
			    rnm = tt.t;
			}
			double now = Utils.rtime();
			if(f == 0)
			    f = now;
			Gob gob = ui.sess.glob.oc.getgob(gobid);
			if(gob == null) {
			    if(now - f < 1.0)
				throw(new Loading());
			    return;
			}
			synchronized (gob) {
			    gob.setattr(new MarkerID(gob, oid));
			}
			Coord tc = gob.rc.floor(tilesz);
			MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
			if(!view.file.lock.writeLock().tryLock())
			    throw(new Loading());
			try {
			    MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
			    if(info == null)
				throw(new Loading());
			    Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
			    SMarker prev = view.file.smarkers.get(oid);
			    if(prev == null) {
				view.file.add(new SMarker(info.seg, sc, rnm, oid, new Resource.Spec(Resource.remote(), res.name, res.ver)));
			    } else {
				if((prev.seg != info.seg) || !eq(prev.tc, sc) || !eq(prev.nm, rnm)) {
				    prev.seg = info.seg;
				    prev.tc = sc;
				    prev.nm = rnm;
				    view.file.update(prev);
				}
			    }
			    uploadMarks();
			} finally {
			    view.file.lock.writeLock().unlock();
			}
		    }
		});
	}
    }

    public void uploadMarks() {
	if (ui.sess != null && ui.sess.alive() && ui.sess.username != null) {
	    if (MapConfig.loadMapSetting(ui.sess.username, "mapper")) {
		MappingClient.getInstance(ui.sess.username).ProcessMap(view.file, (m) -> {
		    if (m instanceof PMarker && MapConfig.loadMapSetting(ui.sess.username, "green")) {
			return ((PMarker) m).color.equals(Color.GREEN) && !m.name().equals("");
		    }
		    return true;
		});
	    }
	}
    }

    public static class ExportWindow extends Window implements MapFile.ExportStatus {
	private Thread th;
	private volatile String prog = "Exporting map...";

	public ExportWindow() {
	    super(UI.scale(new Coord(300, 65)), "Exporting map...", true);
	    adda(new Button(UI.scale(100), "Cancel", false, this::cancel), asz.x / 2, UI.scale(40), 0.5, 0.0);
	}

	public void run(Thread th) {
	    (this.th = th).start();
	}

	public void cdraw(GOut g) {
	    g.text(prog, UI.scale(new Coord(10, 10)));
	}

	public void cancel() {
	    th.interrupt();
	}

	public void tick(double dt) {
	    if(!th.isAlive())
		destroy();
	}

	public void grid(int cs, int ns, int cg, int ng) {
	    this.prog = String.format("Exporting map cut %,d/%,d in segment %,d/%,d", cg, ng, cs, ns);
	}

	public void mark(int cm, int nm) {
	    this.prog = String.format("Exporting marker", cm, nm);
	}
    }

    public static class ImportWindow extends Window {
	private Thread th;
	private volatile String prog = "Initializing";
	private double sprog = -1;

	public ImportWindow() {
	    super(UI.scale(new Coord(300, 65)), "Importing map...", true);
	    adda(new Button(UI.scale(100), "Cancel", false, this::cancel), asz.x / 2, UI.scale(40), 0.5, 0.0);
	}

	public void run(Thread th) {
	    (this.th = th).start();
	}

	public void cdraw(GOut g) {
	    String prog = this.prog;
	    if(sprog >= 0)
		prog = String.format("%s: %d%%", prog, (int)Math.floor(sprog * 100));
	    else
		prog = prog + "...";
	    g.text(prog, UI.scale(new Coord(10, 10)));
	}

	public void cancel() {
	    th.interrupt();
	}

	public void tick(double dt) {
	    if(!th.isAlive())
		destroy();
	}

	public void prog(String prog) {
	    this.prog = prog;
	    this.sprog = -1;
	}

	public void sprog(double sprog) {
	    this.sprog = sprog;
	}
    }

    public void exportmap(Path path) {
	GameUI gui = getparent(GameUI.class);
	ExportWindow prog = new ExportWindow();
	Thread th = new HackThread(() -> {
		boolean complete = false;
		try {
		    try {
			try(OutputStream out = new BufferedOutputStream(Files.newOutputStream(path))) {
			    file.export(out, MapFile.ExportFilter.all, prog);
			}
			complete = true;
		    } finally {
			if(!complete)
			    Files.deleteIfExists(path);
		    }
		} catch(IOException e) {
		    e.printStackTrace(Debug.log);
		    gui.error("Unexpected error occurred when exporting map.");
		} catch(InterruptedException ignored) {
		}
	}, "Mapfile exporter");
	prog.run(th);
	gui.adda(prog, gui.sz.div(2), 0.5, 1.0);
    }

    public void importmap(Path path) {
	GameUI gui = getparent(GameUI.class);
	ImportWindow prog = new ImportWindow();
	Thread th = new HackThread(() -> {
		try {
		    try(SeekableByteChannel fp = Files.newByteChannel(path)) {
			long size = fp.size();
			class Updater extends CountingInputStream {
			    Updater(InputStream bk) {super(bk);}

			    protected void update(long val) {
				super.update(val);
				prog.sprog((double)pos / (double)size);
			    }
			}
			prog.prog("Validating map data");
			file.reimport(new Updater(new BufferedInputStream(Channels.newInputStream(fp))), MapFile.ImportFilter.readonly);
			prog.prog("Importing map data");
			fp.position(0);
			file.reimport(new Updater(new BufferedInputStream(Channels.newInputStream(fp))), MapFile.ImportFilter.all);
		    }
		} catch(InterruptedException ignored) {
		} catch(Exception e) {
		    e.printStackTrace(Debug.log);
		    gui.error("Could not import map: " + e.getMessage());
		}
	}, "Mapfile importer");
	prog.run(th);
	gui.adda(prog, gui.sz.div(2), 0.5, 1.0);
    }

    public void exportmap() {
	java.awt.EventQueue.invokeLater(() -> {
		JFileChooser fc = new JFileChooser();
		fc.setFileFilter(new FileNameExtensionFilter("Exported Haven map data", "hmap"));
		if(fc.showSaveDialog(null) != JFileChooser.APPROVE_OPTION)
		    return;
		Path path = fc.getSelectedFile().toPath();
		if(path.getFileName().toString().indexOf('.') < 0)
		    path = path.resolveSibling(path.getFileName() + ".hmap");
		exportmap(path);
	    });
    }

    public void importmap() {
	java.awt.EventQueue.invokeLater(() -> {
		JFileChooser fc = new JFileChooser();
		fc.setFileFilter(new FileNameExtensionFilter("Exported Haven map data", "hmap"));
		if(fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION)
		    return;
		importmap(fc.getSelectedFile().toPath());
	    });
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    {
	cmdmap.put("exportmap", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if(args.length > 1)
			exportmap(Utils.path(args[1]));
		    else
			exportmap();
		}
	    });
	cmdmap.put("importmap", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if(args.length > 1)
			importmap(Utils.path(args[1]));
		    else
			importmap();
		}
	    });
    }
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }
}
