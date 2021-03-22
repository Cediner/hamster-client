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

import hamster.KeyBind;
import hamster.SessionSettings;
import hamster.data.BeltData;
import hamster.io.SQLResCache;
import hamster.ui.*;
import hamster.ui.Timer.TimersWnd;
import hamster.ui.chr.SkillnCredoWnd;
import hamster.ui.core.indir.IndirSlotView;
import hamster.ui.opt.OptionsWnd;
import hamster.ui.script.ScriptManager;

import java.util.*;
import java.util.function.*;
import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.image.WritableRaster;

import static hamster.GlobalSettings.*;
import static hamster.KeyBind.*;

public class GameUI extends ConsoleHost implements Console.Directory {
    public static final Text.Foundry msgfoundry = new Text.Foundry(Text.dfont, 14);
    private static final int blpw = UI.scale(142);
    public final String chrid, genus;
    public final long plid;
    public Avaview portrait;
    public MapView map;
    public GobIcon.Settings iconconf;
    public Fightview fv;
    private Text lastmsg;
    private double msgtime;
    public Window equwnd, srchwnd, iconwnd;
    private Coord makewndc = Utils.getprefc("makewndc", new Coord(400, 200));
    public Inventory maininv;
    public CharWnd chrwdg;
    public MapWnd mapfile;
    public BuddyWnd buddies;
    public final Zergwnd zerg;
    public final Collection<Polity> polities = new ArrayList<>();
    public HelpWnd help;
    public OptionsWnd opts;
    public Collection<DraggedItem> hand = new LinkedList<>();
    public WItem vhand;
    public double prog = -1;
    private boolean afk = false;
    public BeltSlot[] belt = new BeltSlot[144];
    public final Map<Integer, String> polowners = new HashMap<>();
    public Bufflist buffs;

    //Crafting
    public Window makewnd;

    //Menu Searching
    public ActWnd paginasearch;

    // Delay adding of widgets to GameUI to be part of UI thread
    private final List<Widget> delayedAdd = new ArrayList<>();

    //movement pointers
    public final CustomPointer pointer;

    //Scent Tracking
    public final List<DowseWnd> dowsewnds = new ArrayList<>();

    //Character related windows
    public final SkillnCredoWnd scwnd;

    //Inventories
    public Window invwnd;
    public MiniInvView mminv;

    //Current village / Realm
    public String curvil = "???";
    public String currealm = "???";

    //Current Gob Target for Combat / Marking
    public long curtar = 0;

    //Calendar
    public final Cal cal;

    //MenuGrid
    public MenuGrid menu;

    //Chat UI
    public final ChatWnd chatwnd;
    public ChatUI chat;
    public ChatUI.Channel syslog, botlog;

    //Meters
    public Speedget speed;
    public IMeter hp, stam, energy;
    private final List<Widget> meters = new LinkedList<>();

    //Forage helper
    public final ForageHelperWnd foragehelper;

    //Windows for various Gob mods
    public final Window hidden, deleted, alerted, highlighted;

    //Hotbars
    public final BeltWnd hotbar1, hotbar2, hotbar3;

    //Questing Related widgets
    public final QuestWnd questwnd; // container
    public Widget qqview; // actual quest details

    //Map Markers
    public MapMarkerWnd mapmarkers;

    //Timers
    public final TimersWnd timers;

    //Equipment
    public Equipory equ;
    public MiniEquipView mmequ;
    public final IndirSlotView lrhandview;

    //Script Management
    public final ScriptManager scripts;

    //Session
    public final SessionSettings settings;

    private static final OwnerContext.ClassResolver<BeltSlot> beltctxr = new OwnerContext.ClassResolver<BeltSlot>()
	.add(Glob.class, slot -> slot.wdg().ui.sess.glob)
	.add(Session.class, slot -> slot.wdg().ui.sess);
    public class BeltSlot implements GSprite.Owner {
	public final int idx;
	public final Indir<Resource> res;
	public final Message sdt;

	public BeltSlot(int idx, Indir<Resource> res, Message sdt) {
	    this.idx = idx;
	    this.res = res;
	    this.sdt = sdt;
	}

	private GSprite spr = null;
	public GSprite spr() {
	    GSprite ret = this.spr;
	    if(ret == null)
		ret = this.spr = GSprite.create(this, res.get(), Message.nil);
	    return(ret);
	}

	public Resource getres() {return(res.get());}
	public Random mkrandoom() {return(new Random(System.identityHashCode(this)));}
	public <T> T context(Class<T> cl) {return(beltctxr.context(cl, this));}
	private GameUI wdg() {return(GameUI.this);}
    }

    @RName("gameui")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    String chrid = (String)args[0];
	    int plid = (Integer)args[1];
	    String genus = "";
	    if(args.length > 2)
		genus = (String)args[2];
	    return(new GameUI(ui.sess.username, chrid, plid, genus));
	}
    }

    public GameUI(final String usr, String chrid, long plid, String genus) {
	this.chrid = chrid;
	this.plid = plid;
	this.genus = genus;
	settings = new SessionSettings(usr, chrid);

	setcanfocus(true);
	setfocusctl(true);
	cal = new Cal();
	zerg = add(new Zergwnd(), Utils.getprefc("wndc-zerg", UI.scale(new Coord(187, 50))));
	zerg.hide();
	//Chat Wdgs
	chatwnd = new ChatWnd(chat = new ChatUI(600, 150));
	syslog = chat.add(new ChatUI.Log("System"));
	botlog = chat.add(new ChatUI.BotChat());
	//Quest Wdgs
	questwnd = new QuestWnd();
	//Setup hotbars
	final BeltData data = new BeltData(usr + "::" + chrid);
	hotbar1 = new BeltWnd("Hotbar 1", data, KB_STYLE, KB_VIS, KB_PAGE, KB_LOCK, Arrays.asList(KB_HK),5, 0);
	hotbar2 = new BeltWnd("Hotbar 2", data, KB_F_STYLE, KB_F_VIS, KB_F_PAGE, KB_F_LOCK, Arrays.asList(KB_HK_F), 5, 50);
	hotbar3 = new BeltWnd("Hotbar 3", data, KB_N_STYLE, KB_N_VIS, KB_N_PAGE, KB_N_LOCK, Arrays.asList(KB_HK_N), 4, 100);
	//Setup keybinds
	setKeybinds();
	//Custom Wdgs
	scripts = new ScriptManager();
	hidden = new HiddenManager();
	highlighted = new HighlightManager();
	deleted = new DeletedManager();
	alerted = new SoundManager();
	lrhandview = new IndirSlotView(new Coord(2, 1), "L-R hand view", new int[][]{{6, 7}});
	lrhandview.setVisible(settings.SHOWLRSLOTS.get());
	timers = new TimersWnd();
	timers.hide();
	foragehelper = new ForageHelperWnd();
	foragehelper.hide();
	scwnd = new SkillnCredoWnd();
	scwnd.hide();
	pointer = new CustomPointer("Queued Move");
    }

    protected void attached() {
	iconconf = loadiconconf();
	super.attached();
    }

    protected void added() {
	resize(parent.sz);
	ui.gui = this;
	ui.cons.out = new java.io.PrintWriter(new java.io.Writer() {
		final StringBuilder buf = new StringBuilder();
		
		public void write(char[] src, int off, int len) {
		    List<String> lines = new ArrayList<>();
		    synchronized(this) {
			buf.append(src, off, len);
			int p;
			while((p = buf.indexOf("\n")) >= 0) {
			    String ln = buf.substring(0, p).replace("\t", "        ");
			    lines.add(ln);
			    buf.delete(0, p + 1);
			}
		    }
		    for(String ln : lines) {
			syslog.append(ln, Color.WHITE);
		    }
		}
		
		public void close() {}
		public void flush() {}
	    });
	Debug.log = ui.cons.out;
	// Adding local widgets / custom stuff
	final Coord stdloc = UI.scale(200, 200);
	ui.root.sessionDisplay.unlink();
	add(ui.root.sessionDisplay);
	opts = add(new OptionsWnd(ui));
	opts.hide();
	add(questwnd, new Coord(0, sz.y - 200));
	add(chatwnd, new Coord(20, sz.y - 200));
	portrait = add(new Avaview(Avaview.dasz, plid, "plavacam"), UI.scale(new Coord(10, 10)));
	buffs = add(new Bufflist(), UI.scale(new Coord(95, 65)));
    	add(cal, new Coord(sz.x / 2 - cal.sz.x / 2, 0));
    	add(scripts).hide();
	add(hidden, stdloc);
	add(deleted, stdloc);
	add(alerted, stdloc);
	add(highlighted, stdloc);
	add(lrhandview, stdloc);
	add(timers, stdloc);
	add(foragehelper, stdloc);
	add(scwnd, stdloc);
	add(pointer);
    }

    public void dispose() {
	savewndpos();
	Debug.log = new java.io.PrintWriter(System.err);
	ui.cons.clearout();
	super.dispose();
    }

    public static class Hidewnd extends Window {
	Hidewnd(Coord sz, String cap, boolean lg) {
	    super(sz, cap, lg);
	}

	Hidewnd(Coord sz, String cap) {
	    super(sz, cap);
	}

	public void wdgmsg(Widget sender, String msg, Object... args) {
	    if((sender == this) && msg.equals("close")) {
		this.hide();
		return;
	    }
	    super.wdgmsg(sender, msg, args);
	}
    }

    static class Zergwnd extends Hidewnd {
	Tabs tabs = new Tabs(Coord.z, Coord.z, this);
	final TButton kin, pol, pol2;

	class TButton extends IButton {
	    Tabs.Tab tab = null;
	    final Tex inv;

	    TButton(String nm, boolean g) {
		super("gfx/hud/buttons/" + nm, "u", "d", null);
		if(g)
		    inv = Resource.loadtex("gfx/hud/buttons/" + nm + "g");
		else
		    inv = null;
	    }

	    public void draw(GOut g) {
		if((tab == null) && (inv != null))
		    g.image(inv, Coord.z);
		else
		    super.draw(g);
	    }

	    public void click() {
		if(tab != null) {
		    tabs.showtab(tab);
		    repack();
		}
	    }
	}

	Zergwnd() {
	    super(Coord.z, "Kith & Kin", true);
	    kin = add(new TButton("kin", false));
	    kin.tooltip = Text.render("Kin");
	    pol = add(new TButton("pol", true));
	    pol2 = add(new TButton("rlm", true));
	}

	private void repack() {
	    tabs.indpack();
	    kin.c = new Coord(0, tabs.curtab.contentsz().y + UI.scale(20));
	    pol.c = new Coord(kin.c.x + kin.sz.x + UI.scale(10), kin.c.y);
	    pol2.c = new Coord(pol.c.x + pol.sz.x + UI.scale(10), pol.c.y);
	    this.pack();
	}

	void ntab(Widget ch, TButton btn) {
	    Tabs.Tab tab = add(tabs.new Tab() {
		    public void cresize(Widget ch) {
			repack();
		    }
		}, tabs.c);
	    tab.add(ch, Coord.z);
	    btn.tab = tab;
	    repack();
	}

	void dtab(TButton btn) {
	    btn.tab.destroy();
	    btn.tab = null;
	    repack();
	}

	void addpol(Polity p) {
	    /* This isn't very nice. :( */
	    TButton btn = p.cap.equals("Village")?pol:pol2;
	    ntab(p, btn);
	    btn.tooltip = Text.render(p.cap);
	}
    }

    static class DraggedItem {
	final GItem item;
	final Coord dc;

	DraggedItem(GItem item, Coord dc) {
	    this.item = item; this.dc = dc;
	}
    }

    private void updhand() {
	if((hand.isEmpty() && (vhand != null)) || ((vhand != null) && !hand.contains(vhand.item))) {
	    ui.destroy(vhand);
	    vhand = null;
	    ui.sess.details.removeHeldItem();
	}
	if(!hand.isEmpty() && (vhand == null)) {
	    DraggedItem fi = hand.iterator().next();
	    vhand = add(new ItemDrag(fi.dc, fi.item));
	    ui.sess.details.attachHeldItem(vhand.item);
	}
    }

    private String mapfilename() {
	StringBuilder buf = new StringBuilder();
	buf.append(genus);
	String chrid = Utils.getpref("mapfile/" + this.chrid, "");
	if(!chrid.equals("")) {
	    if(buf.length() > 0) buf.append('/');
	    buf.append(chrid);
	}
	return(buf.toString());
    }

    public Coord optplacement(Widget child, Coord org) {
	Set<Window> closed = new HashSet<>();
	Set<Coord> open = new HashSet<>();
	open.add(org);
	Coord opt = null;
	double optscore = Double.NEGATIVE_INFINITY;
	Coord plc = null;
	{
	    Gob pl = map.player();
	    if(pl != null) {
		Coord3f raw = pl.placed.getc();
		if(raw != null)
		    plc = map.screenxf(raw).round2();
	    }
	}
	Area parea = Area.sized(Coord.z, sz);
	while(!open.isEmpty()) {
	    Coord cur = Utils.take(open);
	    double score = 0;
	    Area tarea = Area.sized(cur, child.sz);
	    if(parea.isects(tarea)) {
		double outside = 1.0 - (((double)parea.overlap(tarea).area()) / ((double)tarea.area()));
		if((outside > 0.75) && !cur.equals(org))
		    continue;
		score -= Math.pow(outside, 2) * 100;
	    } else {
		if(!cur.equals(org))
		    continue;
		score -= 100;
	    }
	    {
		boolean any = false;
		for(Widget wdg = this.child; wdg != null; wdg = wdg.next) {
		    if(!(wdg instanceof Window))
			continue;
		    Window wnd = (Window)wdg;
		    if(!wnd.visible)
			continue;
		    Area warea = wnd.parentarea(this);
		    if(warea.isects(tarea)) {
			any = true;
			score -= ((double)warea.overlap(tarea).area()) / ((double)tarea.area());
			if(!closed.contains(wnd)) {
			    open.add(new Coord(wnd.c.x - child.sz.x, cur.y));
			    open.add(new Coord(cur.x, wnd.c.y - child.sz.y));
			    open.add(new Coord(wnd.c.x + wnd.sz.x, cur.y));
			    open.add(new Coord(cur.x, wnd.c.y + wnd.sz.y));
			    closed.add(wnd);
			}
		    }
		}
		if(!any)
		    score += 10;
	    }
	    if(plc != null) {
		if(tarea.contains(plc))
		    score -= 100;
		else
		    score -= (1 - Math.pow(tarea.closest(plc).dist(plc) / sz.dist(Coord.z), 2)) * 1.5;
	    }
	    score -= (cur.dist(org) / sz.dist(Coord.z)) * 0.75;
	    if(score > optscore) {
		optscore = score;
		opt = cur;
	    }
	}
	return(opt);
    }

    private void savewndpos() {
	if(invwnd != null)
	    Utils.setprefc("wndc-inv", invwnd.c);
	if(equwnd != null)
	    Utils.setprefc("wndc-equ", equwnd.c);
	if(chrwdg != null)
	    Utils.setprefc("wndc-chr", chrwdg.c);
	if(zerg != null)
	    Utils.setprefc("wndc-zerg", zerg.c);
	if(mapfile != null) {
	    Utils.setprefc("wndc-map", mapfile.c);
	    Utils.setprefc("wndsz-map", mapfile.asz);
	}
    }

    private final BMap<String, Window> wndids = new HashBMap<>();

    public void addchild(Widget child, Object... args) {
	String place = ((String)args[0]).intern();
	//Setup hotbars
	switch (place) {
	    case "mapview" -> {
		child.resize(sz);
		map = add((MapView) child, Coord.z);
		map.lower();
		if (mapfile != null) {
		    ui.destroy(mapfile);
		    ui.destroy(mapmarkers);
		    mapfile = null;
		    mapmarkers = null;
		}
		ResCache mapstore = SQLResCache.mapdb;
		if (mapstore != null) {
		    MapFile file = MapFile.load(mapstore, mapfilename());
		    mapfile = new MapWnd(file, map, Utils.getprefc("wndsz-map", UI.scale(new Coord(700, 500))), "Map");
		    mapfile.show(Utils.getprefb("wndvis-map", false));
		    mapmarkers = new MapMarkerWnd(mapfile);
		    mapmarkers.hide();
		    add(mapfile, Utils.getprefc("wndc-map", new Coord(50, 50)));
		    add(mapmarkers, new Coord(50, 50));
		}
	    }
	    case "menu" -> {
		menu = (MenuGrid) add(child, new Coord(sz.x - child.sz.x, sz.y - child.sz.y));
		add(hotbar1, new Coord(20, 300)).setVisible(ui.gui.settings.SHOWHOTBAR1.get());
		add(hotbar2, new Coord(20, 400)).setVisible(ui.gui.settings.SHOWHOTBAR2.get());
		add(hotbar3, new Coord(20, 500)).setVisible(ui.gui.settings.SHOWHOTBAR3.get());

		paginasearch = add(new ActWnd("Menu Search"));
		paginasearch.hide();
		makewnd = add(new MakeWnd());
	    }
	    case "fight" -> fv = add((Fightview) child, sz.x - child.sz.x, 0);
	    case "fsess", "abt" -> add(child, Coord.z);
	    case "inv" -> {
		invwnd = new Hidewnd(Coord.z, "Inventory") {
		    public void cresize(Widget ch) {
			pack();
		    }
		};
		invwnd.add(maininv = (Inventory) child, Coord.z);
		invwnd.pack();
		if(!settings.SHOWINVONLOGIN.get())
		    invwnd.hide();
		mminv = new MiniInvView(maininv);
		add(mminv, new Coord(100, 100));
		add(invwnd, Utils.getprefc("wndc-inv", new Coord(100, 100)));
	    }
	    case "equ" -> {
		equwnd = new Hidewnd(Coord.z, "Equipment");
		equwnd.add(equ = (Equipory) child, Coord.z);
		equwnd.pack();
		equwnd.hide();
		mmequ = new MiniEquipView(equ);
		add(mmequ, new Coord(400, 10));
		add(equwnd, Utils.getprefc("wndc-equ", new Coord(400, 10)));
	    }
	    case "hand" -> {
		GItem g = add((GItem) child);
		Coord lc = (Coord) args[1];
		hand.add(new DraggedItem(g, lc));
		updhand();
	    }
	    case "chr" -> {
		chrwdg = add((CharWnd) child, Utils.getprefc("wndc-chr", new Coord(300, 50)));
		chrwdg.hide();
	    }
	    case "craft" -> {
		makewnd.add(child, new Coord(MakeWnd.WIDTH + 10, 0));
		makewnd.pack();
		makewnd.show();
	    }
	    case "buddy" -> zerg.ntab(buddies = (BuddyWnd) child, zerg.kin);
	    case "pol" -> {
		Polity p = (Polity) child;
		polities.add(p);
		zerg.addpol(p);
	    }
	    case "chat" -> chat.addchild(child);
	    case "party" -> add(child, UI.scale(10), UI.scale(95));
	    case "meter" -> {
		int x = (meters.size() % 3) * (IMeter.fsz.x + UI.scale(5));
		int y = (meters.size() / 3) * (IMeter.fsz.y + UI.scale(2));
		add(child, portrait.c.x + portrait.sz.x + UI.scale(10) + x, portrait.c.y + y);
		meters.add(child);
	    }
	    case "buff" -> buffs.addchild(child);
	    case "qq" -> {
		if (qqview != null)
		    qqview.reqdestroy();
		qqview = child;
		questwnd.add(child, Coord.z);
		questwnd.pack();
	    }
	    case "misc" -> {
		Coord c;
		int a = 1;
		if (args[a] instanceof Coord) {
		    c = (Coord) args[a++];
		} else if (args[a] instanceof Coord2d) {
		    c = ((Coord2d) args[a++]).mul(new Coord2d(this.sz.sub(child.sz))).round();
		    c = optplacement(child, c);
		} else if (args[a] instanceof String) {
		    c = relpos((String) args[a++], child, (args.length > a) ? ((Object[]) args[a++]) : new Object[]{}, 0);
		} else {
		    throw (new UI.UIException("Illegal gameui child", place, args));
		}
		while (a < args.length) {
		    Object opt = args[a++];
		    if (opt instanceof Object[]) {
			Object[] opta = (Object[]) opt;
			if ("id".equals(opta[0])) {
			    String wndid = (String) opta[1];
			    if (child instanceof Window) {
				c = Utils.getprefc(String.format("wndc-misc/%s", opta[1]), c);
				if (!wndids.containsKey(wndid)) {
				    c = fitwdg(child, c);
				    wndids.put(wndid, (Window) child);
				} else {
				    c = optplacement(child, c);
				}
			    }
			}
		    }
		}
		add(child, c);
	    }
	    default -> throw (new UI.UIException("Illegal gameui child", place, args));
	}
    }

    public void cdestroy(Widget w) {
	if(w instanceof Window) {
	    String wndid = wndids.reverse().get(w);
	    if(wndid != null) {
		wndids.remove(wndid);
		Utils.setprefc(String.format("wndc-misc/%s", wndid), w.c);
	    }
	}
	if(w instanceof GItem) {
	    for(Iterator<DraggedItem> i = hand.iterator(); i.hasNext();) {
		DraggedItem di = i.next();
		if(di.item == w) {
		    i.remove();
		    updhand();
		}
	    }
	} else if(w instanceof Polity && polities.contains(w)) {
	    polities.remove(w);
	    zerg.dtab(zerg.pol);
	} else if(w == chrwdg) {
	    chrwdg = null;
	}
	meters.remove(w);
    }
    
    private static final Resource.Anim progt = Resource.local().loadwait("gfx/hud/prog").layer(Resource.animc);
    private Tex curprog = null;
    private int curprogf, curprogb;
    private void drawprog(GOut g, double prog) {
	int fr = Utils.clip((int)Math.floor(prog * progt.f.length), 0, progt.f.length - 2);
	int bf = Utils.clip((int)(((prog * progt.f.length) - fr) * 255), 0, 255);
	if((curprog == null) || (curprogf != fr) || (curprogb != bf)) {
	    if(curprog != null)
		curprog.dispose();
	    WritableRaster buf = PUtils.imgraster(progt.f[fr][0].ssz);
	    PUtils.blit(buf, progt.f[fr][0].scaled().getRaster(), Coord.z);
	    PUtils.blendblit(buf, progt.f[fr + 1][0].scaled().getRaster(), Coord.z, bf);
	    curprog = new TexI(PUtils.rasterimg(buf)); curprogf = fr; curprogb = bf;
	}
	g.aimage(curprog, new Coord(sz.x / 2, (sz.y * 4) / 10), 0.5, 0.5);
    }

    public void draw(GOut g) {
	super.draw(g);
	if(prog >= 0)
	    drawprog(g, prog);
	int by = sz.y;
	if(cmdline != null) {
	    drawcmd(g, new Coord(blpw + UI.scale(10), by - UI.scale(20)));
	} else if(lastmsg != null) {
	    if((Utils.rtime() - msgtime) > 3.0) {
		lastmsg = null;
	    } else {
		g.chcolor(0, 0, 0, 192);
		g.frect(new Coord(blpw + UI.scale(8), by - UI.scale(22)), lastmsg.sz().add(UI.scale(4), UI.scale(4)));
		g.chcolor();
		g.image(lastmsg.tex(), new Coord(blpw + UI.scale(10), by - UI.scale(20)));
	    }
	}
    }
    
    private String iconconfname() {
	StringBuilder buf = new StringBuilder();
	buf.append("data/mm-icons");
	if(genus != null)
	    buf.append("/").append(genus);
	if(ui.sess != null)
	    buf.append("/").append(ui.sess.username);
	return(buf.toString());
    }

    private GobIcon.Settings loadiconconf() {
	if(ResCache.global == null)
	    return(new GobIcon.Settings());
	try {
	    try(StreamMessage fp = new StreamMessage(ResCache.global.fetch(iconconfname()))) {
		return(GobIcon.Settings.load(fp));
	    }
	} catch(java.io.FileNotFoundException e) {
	    return(new GobIcon.Settings());
	} catch(Exception e) {
	    new Warning(e, "failed to load icon-conf").issue();
	    return(new GobIcon.Settings());
	}
    }

    public void saveiconconf() {
	if(ResCache.global == null)
	    return;
	try {
	    try(StreamMessage fp = new StreamMessage(ResCache.global.store(iconconfname()))) {
		iconconf.save(fp);
	    }
	} catch(Exception e) {
	    new Warning(e, "failed to store icon-conf").issue();
	}
    }

    private Coord lastsavegrid = null;
    private int lastsaveseq = -1;
    private void mapfiletick() {
	MapView map = this.map;
	if((map == null) || (mapfile == null))
	    return;
	Gob pl = ui.sess.glob.oc.getgob(map.plgob);
	Coord gc;
	if(pl == null)
	    gc = map.cc.floor(MCache.tilesz).div(MCache.cmaps);
	else
	    gc = pl.rc.floor(MCache.tilesz).div(MCache.cmaps);
	try {
	    MCache.Grid grid = ui.sess.glob.map.getgrid(gc);
	    if((grid != null) && (!Utils.eq(gc, lastsavegrid) || (lastsaveseq != grid.seq))) {
	        mapfile.file.update(ui.sess.glob.map, gc);
		lastsavegrid = gc;
		lastsaveseq = grid.seq;
	    }
	} catch(Loading ignored) {
	}
    }

    private double lastwndsave = 0;
    public void tick(double dt) {
	super.tick(dt);
	double now = Utils.rtime();
	if(now - lastwndsave > 60) {
	    savewndpos();
	    lastwndsave = now;
	}
	double idle = now - ui.lastevent;
	if(!afk && (idle > 300)) {
	    afk = true;
	    wdgmsg("afk");
	} else if(afk && (idle <= 300)) {
	    afk = false;
	}
	mapfiletick();
	synchronized (delayedAdd) {
	    for(final var chd : delayedAdd) {
	        add(chd);
	    }
	    delayedAdd.clear();
	}
    }
    
    public void uimsg(String msg, Object... args) {
	switch (msg) {
	    case "err":
		String err = (String) args[0];
		error(err);
		break;
	    case "msg":
		String text = (String) args[0];
		msg(text);
		break;
	    case "prog":
		if (args.length > 0)
		    prog = ((Number) args[0]).doubleValue() / 100.0;
		else
		    prog = -1;
		break;
	    case "setbelt":
		int slot = (Integer) args[0];
		if (args.length < 2) {
		    belt[slot] = null;
		} else {
		    Indir<Resource> res = ui.sess.getres((Integer) args[1]);
		    Message sdt = Message.nil;
		    if (args.length > 2)
			sdt = new MessageBuf((byte[]) args[2]);
		    belt[slot] = new BeltSlot(slot, res, sdt);
		}
		if (slot <= 49)
		    hotbar1.update(slot);
		else if (slot <= 99)
		    hotbar2.update(slot);
		else if (slot <= 140)
		    hotbar3.update(slot);
		break;
	    case "polowner": //This is for Villages and Realms, need to look at how they differ, if at all.
		int id = (Integer) args[0];
		String o = (String) args[1];
		boolean n = ((Integer) args[2]) != 0;
		if (o != null)
		    o = o.intern();
		String cur = polowners.get(id);
		if (map != null) {
		    if ((o != null) && (cur == null)) {
			map.setpoltext(id, "Entering " + o);
			curvil = o;
		    } else if ((o == null) && (cur != null)) {
			map.setpoltext(id, "Leaving " + cur);
			curvil = "???";
		    }
		}
		polowners.put(id, o);
		break;
	    case "showhelp": {
		Indir<Resource> res = ui.sess.getres((Integer) args[0]);
		if (help == null)
		    help = adda(new HelpWnd(res), 0.5, 0.25);
		else
		    help.res = res;
		break;
	    }
	    case "map-mark": {
		long gobid = Utils.uint32((Integer) args[0]);
		long oid = (Long) args[1];
		Indir<Resource> res = ui.sess.getres((Integer) args[2]);
		String nm = (String) args[3];
		if (mapfile != null)
		    mapfile.markobj(gobid, oid, res, nm);
		break;
	    }
	    case "map-icons":
		GobIcon.Settings conf = this.iconconf;
		int tag = (Integer) args[0];
		if (args.length < 2) {
		    if (conf.tag != tag)
			wdgmsg("map-icons", conf.tag);
		} else if (args[1] instanceof String) {
		    Resource.Spec res = new Resource.Spec(null, (String) args[1], (Integer) args[2]);
		    GobIcon.Setting cset = new GobIcon.Setting(res);
		    boolean has = conf.settings.containsKey(res.name);
		    cset.show = cset.defshow = ((Integer) args[3]) != 0;
		    conf.receive(tag, new GobIcon.Setting[]{cset});
		    saveiconconf();
		    if (!has && conf.notify) {
			ui.sess.glob.loader.defer(() -> {
			    Resource lres = Resource.remote().load(res.name, res.ver).get();
			    Resource.Tooltip tip = lres.layer(Resource.tooltip);
			    if (tip != null)
				msg(String.format("%s added to list of seen icons.", tip.t));
			}, (Supplier<Object>) () -> null);
		    }
		} else if (args[1] instanceof Object[]) {
		    Object[] sub = (Object[]) args[1];
		    int a = 0;
		    Collection<GobIcon.Setting> csets = new ArrayList<>();
		    while (a < sub.length) {
			String resnm = (String) sub[a++];
			int resver = (Integer) sub[a++];
			int fl = (Integer) sub[a++];
			Resource.Spec res = new Resource.Spec(null, resnm, resver);
			GobIcon.Setting cset = new GobIcon.Setting(res);
			cset.show = cset.defshow = ((fl & 1) != 0);
			csets.add(cset);
		    }
		    conf.receive(tag, csets.toArray(new GobIcon.Setting[0]));
		    saveiconconf();
		}
		break;
	    default:
		super.uimsg(msg, args);
		break;
	}
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
	if((sender == chrwdg) && (msg.equals("close"))) {
	    chrwdg.hide();
	    return;
	} else if((sender == mapfile) && (msg.equals("close"))) {
	    mapfile.hide();
	    Utils.setprefb("wndvis-map", false);
	    return;
	} else if((sender == help) && (msg.equals("close"))) {
	    ui.destroy(help);
	    help = null;
	    return;
	} else if((sender == srchwnd) && (msg.equals("close"))) {
	    ui.destroy(srchwnd);
	    srchwnd = null;
	    return;
	} else if((sender == iconwnd) && (msg.equals("close"))) {
	    ui.destroy(iconwnd);
	    iconwnd = null;
	    return;
	}
	super.wdgmsg(sender, msg, args);
    }

    private static final int fitmarg = UI.scale(100);
    private Coord fitwdg(Widget wdg, Coord c) {
	Coord ret = new Coord(c);
	ret.x = Math.max(ret.x, Math.min(0, fitmarg - wdg.sz.x));
	ret.y = Math.max(ret.y, Math.min(0, fitmarg - wdg.sz.y));
	ret.x = Math.min(ret.x, sz.x - Math.min(fitmarg, wdg.sz.x));
	ret.y = Math.min(ret.y, sz.y - Math.min(fitmarg, wdg.sz.y));
	return(ret);
    }

    private void fitwdg(Widget wdg) {
	wdg.c = fitwdg(wdg, wdg.c);
    }

    public <T extends Widget> T dadd(T child) {
        synchronized (delayedAdd) {
	    delayedAdd.add(child);
	}
    	return child;
    }

    public void makeDowseWnd(final DowseWnd wnd) {
	synchronized (dowsewnds) {
	    dowsewnds.add(dadd(wnd));
	}
    }

    public void remDowseWnd(final DowseWnd wnd) {
	synchronized (dowsewnds) {
	    dowsewnds.removeIf(wdg -> wdg == wnd);
	}
    }

    public boolean wndstate(Window wnd) {
	if(wnd == null)
	    return(false);
	return(wnd.visible);
    }

    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
    private void setKeybinds() {
        binds.put(KB_TOGGLE_CMD, () -> { entercmd(); return true; });
        binds.put(KB_TOGGLE_CHAT, () -> { chatwnd.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_CHAR, () -> { chrwdg.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_EQU, () -> { equwnd.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_INV, () -> { invwnd.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_KIN, () -> { zerg.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_MINIMAP, () -> { mapfile.toggleVisibility(); return true; });
        binds.put(KB_TOGGLE_OPTS, () -> { opts.toggleVisibility(); return true; });
    	binds.put(KB_SCREENSHOT, () -> { Screenshooter.take(this, Config.screenurl); return true;});
    	binds.put(KB_FOCUS_MAP, () -> { setfocus(map); return true; });
    }

    public boolean globtype(char key, KeyEvent ev) {
        final String bind = KeyBind.generateSequence(ev, ui);
        for(final var kb : binds.keySet()) {
            if(kb.check(bind, binds.get(kb)))
                return true;
	}
	return(super.globtype(key, ev));
    }
    
    public boolean mousedown(Coord c, int button) {
	return(super.mousedown(c, button));
    }

    public void resize(Coord sz) {
	this.sz = sz;
	if(map != null)
	    map.resize(sz);
	super.resize(sz);
    }
    
    public void presize() {
	resize(parent.sz);
    }
    
    public void msg(String msg, Color color, Color logcol) {
	msgtime = Utils.rtime();
	lastmsg = msgfoundry.render(msg, color);
	syslog.append(msg, logcol);
    }

    public void msg(String msg, Color color) {
	msg(msg, color, color);
    }

    private static final Resource errsfx = Resource.local().loadwait("sfx/error");
    private double lasterrsfx = 0;
    public void error(String msg) {
	msg(msg, new Color(192, 0, 0), new Color(255, 0, 0));
	double now = Utils.rtime();
	if(now - lasterrsfx > 0.1) {
	    Audio.play(errsfx);
	    lasterrsfx = now;
	}
    }

    private static final Resource msgsfx = Resource.local().loadwait("sfx/msg");
    private double lastmsgsfx = 0;
    public void msg(String msg) {
	msg(msg, Color.WHITE, Color.WHITE);
	double now = Utils.rtime();
	if(now - lastmsgsfx > 0.1) {
	    Audio.play(msgsfx);
	    lastmsgsfx = now;
	}
    }
    
    public void act(String... args) {
	wdgmsg("act", (Object[])args);
    }

    public void act(int mods, Coord mc, Gob gob, String... args) {
	int n = args.length;
	Object[] al = new Object[n];
	System.arraycopy(args, 0, al, 0, n);
	if(mc != null) {
	    al = Utils.extend(al, al.length + 2);
	    al[n++] = mods;
	    al[n++] = mc;
	    if(gob != null) {
		al = Utils.extend(al, al.length + 2);
		al[n++] = (int)gob.id;
		al[n] = gob.rc;
	    }
	}
	wdgmsg("act", al);
    }

    private final Map<String, Console.Command> cmdmap = new TreeMap<>();
    {
	cmdmap.put("afk", (cons, args) -> {
	    afk = true;
	    wdgmsg("afk");
	});
	cmdmap.put("act", (cons, args) -> {
	    Object[] ad = new Object[args.length - 1];
	    System.arraycopy(args, 1, ad, 0, ad.length);
	    wdgmsg("act", ad);
	});
	cmdmap.put("chrmap", new Console.Command() {
		public void run(Console cons, String[] args) {
		    Utils.setpref("mapfile/" + chrid, args[1]);
		}
	    });
	cmdmap.put("tool", (cons, args) -> {
	    try {
		Object[] wargs = new Object[args.length - 2];
		if (wargs.length >= 0) System.arraycopy(args, 2, wargs, 0, wargs.length);
		add(gettype(args[1]).create(ui, wargs), 200, 200);
	    } catch(RuntimeException e) {
		e.printStackTrace(Debug.log);
	    }
	});
    }
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }
}
