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
import java.io.*;
import java.awt.Color;
import java.awt.event.KeyEvent;

import hamster.ui.MapMarkerWnd;
import hamster.ui.core.ResizableWnd;
import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.MiniMap.*;
import haven.BuddyWnd.GroupSelector;
import static haven.MCache.tilesz;
import static haven.MCache.cmaps;
import static haven.Utils.eq;
import javax.swing.JFileChooser;
import javax.swing.filechooser.*;

//TODO: Readd export / import via window buttons
public class MapWnd extends ResizableWnd implements Console.Directory {
    public static final Resource markcurs = Resource.local().loadwait("gfx/hud/curs/flag");
    public final MapFile file;
    public final MiniMap view;
    public final MapView mv;
    public boolean hmarkers = false;
    private final Locator player;
    private final Widget toolbar;
    private final Frame viewf;
    private MapMarkerWnd markers = null;
    private boolean domark = false;
    private final Collection<Runnable> deferred = new LinkedList<>();

    public static final KeyBinding kb_home = KeyBinding.get("mapwnd/home", KeyMatch.forcode(KeyEvent.VK_HOME, 0));
    public static final KeyBinding kb_mark = KeyBinding.get("mapwnd/mark", KeyMatch.nil);
    public static final KeyBinding kb_hmark = KeyBinding.get("mapwnd/hmark", KeyMatch.forchar('M', KeyMatch.C));
    public static final KeyBinding kb_compact = KeyBinding.get("mapwnd/compact", KeyMatch.forchar('A', KeyMatch.M));
    public MapWnd(MapFile file, MapView mv, Coord sz, String title) {
	super(sz, title, true);
	this.file = file;
	this.mv = mv;
	this.player = new MapLocator(mv);
	viewf = add(new ViewFrame());
	view = viewf.add(new View(file));
	recenter();
	toolbar = add(new Widget(Coord.z));
	toolbar.add(new Img(Resource.loadtex("gfx/hud/mmap/fgwdg")), Coord.z);
	toolbar.add(new IButton("gfx/hud/mmap/home", "", "-d", "-h") {
		{settip("Follow"); setgkey(kb_home);}
		public void click() {
		    recenter();
		}
	    }, Coord.z);
	toolbar.add(new ICheckBox("gfx/hud/mmap/mark", "", "-d", "-h", "-dh"), Coord.z)
	    .state(() -> domark).set(a -> domark = a)
	    .settip("Add marker").setgkey(kb_mark);
	toolbar.add(new ICheckBox("gfx/hud/mmap/hmark", "", "-d", "-h", "-dh"))
	    .state(() -> hmarkers).set(a -> hmarkers = a)
	    .settip("Hide markers").setgkey(kb_hmark);
	toolbar.add(new ICheckBox("gfx/hud/mmap/wnd", "", "-d", "-h", "-dh"))
	    .changed((a) ->  markers = markers == null ? ui.gui.add(new MapMarkerWnd(this)) : markers).settip("Compact mode").setgkey(kb_compact);
	toolbar.pack();
	resize(sz);
	makeHidable();
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
	View(MapFile file) {
	    super(file);
	}

	public void drawmarkers(GOut g) {
	    if(!hmarkers)
		super.drawmarkers(g);
	}

	public boolean clickmarker(DisplayMarker mark, Location loc, int button, boolean press) {
	    if(button == 1) {
		if(!press && !domark) {
		    if(markers != null)
		        markers.list.change(mark.m);
		    return(true);
		}
	    } else if(mark.m instanceof SMarker) {
		Gob gob = MarkerID.find(ui.sess.glob.oc, ((SMarker)mark.m).oid);
		if(gob != null)
		    mvclick(mv, null, loc, gob, button);
	    }
	    return(false);
	}

	public boolean clickicon(DisplayIcon icon, Location loc, int button, boolean press) {
	    if(!press && !domark) {
		mvclick(mv, null, loc, icon.gob, button);
		return(true);
	    }
	    return(false);
	}

	public boolean clickloc(Location loc, int button, boolean press) {
	    if(domark && (button == 1) && !press) {
		Marker nm = new PMarker(loc.seg.id, loc.tc, "New marker", BuddyWnd.gc[new Random().nextInt(BuddyWnd.gc.length)]);
		file.add(nm);
		if(markers != null)
		    markers.list.change(nm);
		domark = false;
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
	    }
	    super.mousedown(c, button);
	    return(true);
	}

	public void draw(GOut g) {
	    g.chcolor(0, 0, 0, 128);
	    g.frect(Coord.z, sz);
	    g.chcolor();
	    super.draw(g);
	}

	public Resource getcurs(Coord c) {
	    if(domark)
		return(markcurs);
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
			gob.setattr(new MarkerID(gob, oid));
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
			} finally {
			    view.file.lock.writeLock().unlock();
			}
		    }
		});
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

    public void exportmap(File path) {
	GameUI gui = getparent(GameUI.class);
	ExportWindow prog = new ExportWindow();
	Thread th = new HackThread(() -> {
		try {
		    try(OutputStream out = new BufferedOutputStream(new FileOutputStream(path))) {
			file.export(out, MapFile.ExportFilter.all, prog);
		    }
		} catch(IOException e) {
		    e.printStackTrace(Debug.log);
		    gui.error("Unexpected error occurred when exporting map.");
		} catch(InterruptedException e) {
		}
	}, "Mapfile exporter");
	prog.run(th);
	gui.adda(prog, gui.sz.div(2), 0.5, 1.0);
    }

    public void importmap(File path) {
	GameUI gui = getparent(GameUI.class);
	ImportWindow prog = new ImportWindow();
	Thread th = new HackThread(() -> {
		long size = path.length();
		class Updater extends CountingInputStream {
		    Updater(InputStream bk) {super(bk);}

		    protected void update(long val) {
			super.update(val);
			prog.sprog((double)pos / (double)size);
		    }
		}
		try {
		    prog.prog("Validating map data");
		    try(InputStream in = new Updater(new FileInputStream(path))) {
			file.reimport(in, MapFile.ImportFilter.readonly);
		    }
		    prog.prog("Importing map data");
		    try(InputStream in = new Updater(new FileInputStream(path))) {
			file.reimport(in, MapFile.ImportFilter.all);
		    }
		} catch(InterruptedException e) {
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
		File path = fc.getSelectedFile();
		if(path.getName().indexOf('.') < 0)
		    path = new File(path.toString() + ".hmap");
		exportmap(path);
	    });
    }

    public void importmap() {
	java.awt.EventQueue.invokeLater(() -> {
		JFileChooser fc = new JFileChooser();
		fc.setFileFilter(new FileNameExtensionFilter("Exported Haven map data", "hmap"));
		if(fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION)
		    return;
		importmap(fc.getSelectedFile());
	    });
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    {
	cmdmap.put("exportmap", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if(args.length > 1)
			exportmap(new File(args[1]));
		    else
			exportmap();
		}
	    });
	cmdmap.put("importmap", new Console.Command() {
		public void run(Console cons, String[] args) {
		    if(args.length > 1)
			importmap(new File(args[1]));
		    else
			importmap();
		}
	    });
    }
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }
}
