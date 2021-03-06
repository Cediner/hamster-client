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

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.font.TextAttribute;
import java.awt.image.BufferedImage;

import com.google.common.flogger.FluentLogger;
import hamster.GlobalSettings;
import hamster.KeyBind;
import hamster.data.ScriptIconData;
import hamster.ui.core.MovableWidget;
import hamster.ui.script.ScriptManager;
import hamster.util.ObservableCollection;
import hamster.util.msg.MailBox;
import hamster.util.msg.MessageBus;
import haven.Resource.AButton;

import java.io.File;
import java.util.*;
import java.util.function.Consumer;

public class MenuGrid extends MovableWidget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public final static Tex bg = Resource.loadtex("gfx/hud/invsq");
    public final static Coord bgsz = bg.sz().add(-UI.scale(1), -UI.scale(1));
    public final static RichText.Foundry ttfnd = new RichText.Foundry(TextAttribute.FAMILY, "SansSerif", TextAttribute.SIZE, UI.scale(10f));
    private Coord gsz = new Coord(5, 5);
    public final ObservableCollection<Pagina> paginae = new ObservableCollection<>(new HashSet<>());
    public Pagina cur;
    private Pagina dragging;
    private Collection<PagButton> curbtns = Collections.emptyList();
    private PagButton pressed, layout[][] = new PagButton[gsz.x][gsz.y];
    private UI.Grab grab;
    private int curoff = 0;
    private boolean recons = true;
    public final Map<String, CustomPagina> custompag = new HashMap<>();
    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
    private final List<String> duse = new ArrayList<>();

    /*
     * MessageBus / MailBox System Message
     */
    public static final MessageBus<MenuGridMail> MessageBus = new MessageBus<>();
    private MailBox<MenuGridMail> mailbox;
    public static abstract class MenuGridMail extends hamster.util.msg.Message {
	public abstract void apply(final MenuGrid menu);
    }

    public static class UpdateLayout extends MenuGridMail {
	@Override
	public void apply(MenuGrid menu) {
	    menu.updlayoutsize();
	}
    }


    @RName("scm")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new MenuGrid());
	}
    }

    public static class Interaction {
	public final int btn, modflags;
	public final Coord2d mc;
	public final ClickData click;

	public Interaction(int btn, int modflags, Coord2d mc, ClickData click) {
	    this.btn = btn;
	    this.modflags = modflags;
	    this.mc = mc;
	    this.click = click;
	}

	public Interaction(int btn, int modflags) {
	    this(btn, modflags, null, null);
	}

	public Interaction() {
	    this(1, 0);
	}
    }
    public static class PagButton implements ItemInfo.Owner {
	public final Pagina pag;
	public final Resource res;
	public final KeyBind kb;

	public PagButton(Pagina pag) {
	    this.pag = pag;
	    this.res = pag.res();
	    this.kb = binding();
	}

	public BufferedImage img() {return(res.layer(Resource.imgc).scaled());}
	public String name() {
	    if(!GlobalSettings.DEBUG.get())
	    	return pag.name();
	    else
	        return String.format("%s (%s)", pag.name(), pag.res().name);
	}
	public String hotkey() {
	    return pag.hotkey();
	}
	public KeyBind binding() {
	    return KeyBind.getDynamicKB(res.name, "MenuGrid", hotkey());
	}
	@Deprecated public void use() {
	    pag.use();
	}
	public void use(Interaction iact) {
	    if (pag instanceof CustomPagina)
	        use();
	    else {
		Object[] args = Utils.extend(new Object[0], res.layer(Resource.action).ad);
		args = Utils.extend(args, Integer.valueOf(pag.scm.ui.modflags()));
		if (iact.mc != null) {
		    args = Utils.extend(args, iact.mc.floor(OCache.posres));
		    if (iact.click != null)
			args = Utils.extend(args, iact.click.clickargs());
		}
		pag.scm.wdgmsg("act", args);
	    }
	}

	public String sortkey() {
	    AButton ai = pag.act();
	    if(ai.ad.length == 0)
		return("\0" + name());
	    return(name());
	}

	private List<ItemInfo> info = null;
	public List<ItemInfo> info() {
	    if(info == null)
		info = ItemInfo.buildinfo(this, pag.rawinfo);
	    return(info);
	}
	private static final OwnerContext.ClassResolver<PagButton> ctxr = new OwnerContext.ClassResolver<PagButton>()
	    .add(Glob.class, p -> p.pag.scm.ui.sess.glob)
	    .add(Session.class, p -> p.pag.scm.ui.sess);
	public <T> T context(Class<T> cl) {return(ctxr.context(cl, this));}

	public BufferedImage rendertt(boolean withpg) {
	    Resource.Pagina pg = res.layer(Resource.pagina);
	    String tt = name();
	    final String key = kb.bind.get();
	    int pos = -1;
	    char vkey = key.length() > 0 ? key.charAt(0) : '\0';
	    if((vkey != 0) && (key.contains("-")))
		pos = tt.toUpperCase().indexOf(Character.toUpperCase(vkey));
	    if(pos >= 0)
		tt = tt.substring(0, pos) + "$b{$col[255,128,0]{" + tt.charAt(pos) + "}}" + tt.substring(pos + 1);
	    else if(!key.equals(""))
		tt += " [$b{$col[255,128,0]{" + key + "}}]";
	    BufferedImage ret = ttfnd.render(tt, UI.scale(300)).img;
	    if(withpg) {
		List<ItemInfo> info = info();
		info.removeIf(el -> el instanceof ItemInfo.Name);
		if(!info.isEmpty())
		    ret = ItemInfo.catimgs(0, ret, ItemInfo.longtip(info));
		if(pg != null)
		    ret = ItemInfo.catimgs(0, ret, ttfnd.render("\n" + pg.text, UI.scale(200)).img);
		if(GlobalSettings.DEBUG.get()) {
		    for(final var inf : info) {
			ret = ItemInfo.catimgs(0, ret, Text.render("\n" + inf).img);
		    }
		}
	    }
	    return(ret);
	}

	@Resource.PublishedCode(name = "pagina")
	public interface Factory {
	    public PagButton make(Pagina info);
	}
    }

    public final PagButton next = new PagButton(new Pagina(this, Resource.local().loadwait("gfx/hud/sc-next").indir())) {
	    {pag.button = this;}

	    public void use(Interaction iact) {
		if((curoff + (gsz.x*gsz.y)-2) >= curbtns.size())
		    curoff = 0;
		else
		    curoff += (gsz.x * gsz.y) - 2;
		updlayout();
	    }

	    public String name() {return("More...");}

	    public KeyBind binding() {return(KeyBind.KB_SCM_NEXT);}
	};

    public final PagButton bk = new PagButton(new Pagina(this, Resource.local().loadwait("gfx/hud/sc-back").indir())) {
	    {pag.button = this;}

	    public void use(Interaction iact) {
		pag.scm.change(paginafor(pag.scm.cur.act().parent));
		curoff = 0;
	    }

	    public String name() {return("Back");}

	    public KeyBind binding() {return(KeyBind.KB_SCM_BACK);}
	};

    public static class Pagina {
	public final MenuGrid scm;
	public final Indir<Resource> res;
	public State st;
	public double meter, gettime, dtime, fstart;
	public Indir<Tex> img;
	public int newp;
	public Object[] rawinfo = {};
	private final Consumer<Pagina> onUse;

	public static enum State {
	    ENABLED, DISABLED {
		public Indir<Tex> img(Pagina pag) {
		    return(Utils.cache(() -> new TexI(PUtils.monochromize(pag.button().img(), Color.LIGHT_GRAY))));
		}
	    };

	    public Indir<Tex> img(Pagina pag) {
		return(Utils.cache(() -> new TexI(pag.button().img())));
	    }
	}

	public Pagina(MenuGrid scm, Indir<Resource> res) {
	    this.scm = scm;
	    this.res = res;
	    state(State.ENABLED);
	    this.onUse = (me) -> scm.wdgmsg("act", (Object[]) res().layer(Resource.action).ad);
	}

	public Pagina(MenuGrid scm, Indir<Resource> res, final Consumer<Pagina> onUse) {
	    this.scm = scm;
	    this.res = res;
	    state(State.ENABLED);
	    this.onUse = onUse;
	}

	public String name() {
	    return(res().layer(Resource.action).name);
	}

	public String hotkey() {
	    char hk = res().layer(Resource.action).hk;
	    if(hk == 0)
		return "";
	    return(KeyMatch.forchar(Character.toUpperCase(hk), KeyMatch.MODS & ~KeyMatch.S, 0).name());
	}

	public Resource res() {
	    return(res.get());
	}

	public Resource.AButton act() {
	    return(res().layer(Resource.action));
	}

	public void use() {
	    onUse.accept(this);
	}

	private PagButton button = null;
	public PagButton button() {
	    if(button == null) {
		Resource res = res();
		PagButton.Factory f = res.getcode(PagButton.Factory.class, false);
		if(f == null)
		    button = new PagButton(this);
		else
		    button = f.make(this);
	    }
	    return(button);
	}

	public void state(State st) {
	    this.st = st;
	    this.img = st.img(this);
	}
    }

    public static class CustomPagina extends Pagina {
	public final String key;

	private CustomPagina(MenuGrid scm, String key, Indir<Resource> res, final Consumer<Pagina> onUse) {
	    super(scm, res, onUse);
	    this.key = key;
	}
    }

    public static class ScriptPagina extends CustomPagina {
        private final AButton act;
        private final String name;

        public ScriptPagina(final MenuGrid scm, final String script,
			    final Indir<Resource> res, final Consumer<Pagina> onUse) {
            super(scm, String.format("Script::%s", script), res, onUse);
            this.name = script;
	    final Resource tmp = new Resource(Resource.local(),  String.format("script::%s", script), 1);
	    this.act = tmp.new AButton(Resource.local().load("custom/paginae/default/scripts"), script);
	}

	public AButton act() {
            return act;
	}

	@Override
	public String name() {
	    return name;
	}

	@Override
	public String hotkey() {
	    return "";
	}
    }

    public Map<Indir<Resource>, Pagina> pmap = new WeakHashMap<Indir<Resource>, Pagina>();
    public Pagina paginafor(Indir<Resource> res) {
	if(res == null)
	    return(null);
	synchronized(pmap) {
	    Pagina p = pmap.get(res);
	    if(p == null)
		pmap.put(res, p = new Pagina(this, res));
	    return(p);
	}
    }

    private boolean cons(Pagina p, Collection<PagButton> buf) {
	Pagina[] cp = new Pagina[0];
	Collection<Pagina> open, close = new HashSet<Pagina>();
	synchronized(paginae) {
	    open = new LinkedList<Pagina>();
	    for(Pagina pag : paginae) {
		if(pag.newp == 2) {
		    pag.newp = 0;
		    pag.fstart = 0;
		}
		open.add(pag);
	    }
	    for(Pagina pag : pmap.values()) {
		if(pag.newp == 2) {
		    pag.newp = 0;
		    pag.fstart = 0;
		}
	    }
	}
	boolean ret = true;
	while(!open.isEmpty()) {
	    Iterator<Pagina> iter = open.iterator();
	    Pagina pag = iter.next();
	    iter.remove();
	    try {
		AButton ad = pag.act();
		if(ad == null)
		    throw(new RuntimeException("Pagina in " + pag.res + " lacks action"));
		Pagina parent = paginafor(ad.parent);
		if((pag.newp != 0) && (parent != null) && (parent.newp == 0)) {
		    parent.newp = 2;
		    parent.fstart = (parent.fstart == 0)?pag.fstart:Math.min(parent.fstart, pag.fstart);
		}
		if(parent == p)
		    buf.add(pag.button());
		else if((parent != null) && !close.contains(parent) && !open.contains(parent))
		    open.add(parent);
		close.add(pag);
	    } catch(Loading e) {
		ret = false;
	    }
	}
	return(ret);
    }

    public MenuGrid() {
	super(bgsz.mul(new Coord(5,5)).add(UI.scale(1), UI.scale(1)), "Menugrid");
	//Custom Management Menu
	paginae.add(paginafor(Resource.local().load("custom/paginae/default/management")));
	addCustom(new CustomPagina(this, "management::landmanager",
		Resource.local().load("custom/paginae/default/wnd/selector"),
		(pag) -> ui.gui.add(new MapMod(true))));
	addCustom(new CustomPagina(this, "management::scripts",
		Resource.local().load("custom/paginae/default/wnd/scripts"),
		(pag) -> ui.gui.scripts.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::alerted",
		Resource.local().load("custom/paginae/default/wnd/alerted"),
		(pag) -> ui.gui.alerted.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::deleted",
		Resource.local().load("custom/paginae/default/wnd/deleted"),
		(pag) -> ui.gui.deleted.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::hidden",
		Resource.local().load("custom/paginae/default/wnd/hidden"),
		(pag) -> ui.gui.hidden.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::highlightmanager",
		Resource.local().load("custom/paginae/default/wnd/highlight"),
		(pag) -> ui.gui.highlighted.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::timers",
		Resource.local().load("custom/paginae/default/wnd/timers"),
		(pag) -> ui.gui.timers.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::foragehelper",
		Resource.local().load("custom/paginae/default/wnd/foragehelper"),
		(pag) -> ui.gui.foragehelper.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::skillsncredo",
		Resource.local().load("custom/paginae/default/wnd/skillsncredo"),
		(pag) -> ui.gui.scwnd.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::crafting",
		Resource.local().load("custom/paginae/default/wnd/crafting"),
		(pag) -> ui.gui.makewnd.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::search",
		Resource.local().load("custom/paginae/default/wnd/search"),
		(pag) -> ui.gui.paginasearch.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::studyreport",
		Resource.local().load("custom/paginae/default/wnd/study"),
		(pag) -> GameUI.MessageBus.send(new GameUI.ToggleVisibility(GameUI.Wdg.StudyWindow))));
	addCustom(new CustomPagina(this, "management::foodsearch",
		Resource.local().load("custom/paginae/default/wnd/food"),
		(pag) -> ui.gui.foodwnd.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::shorten",
		Resource.local().load("custom/paginae/default/wnd/shorten"),
		(pag) -> ui.gui.shorten.toggleVisibility()));
	//Hafen Window toggles
	addCustom(new CustomPagina(this, "management::inv",
		Resource.local().load("custom/paginae/default/wnd/inv"),
		(pag) -> ui.gui.invwnd.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::char",
		Resource.local().load("custom/paginae/default/wnd/char"),
		(pag) -> ui.gui.chrwdg.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::equ",
		Resource.local().load("custom/paginae/default/wnd/equ"),
		(pag) -> ui.gui.equwnd.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::kithnkin",
		Resource.local().load("custom/paginae/default/wnd/kithnkin"),
		(pag) -> ui.gui.zerg.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::lmap",
		Resource.local().load("custom/paginae/default/wnd/lmap"),
		(pag) -> GameUI.MessageBus.send(new GameUI.ToggleVisibility(GameUI.Wdg.MiniMap))));
	addCustom(new CustomPagina(this, "management::opts",
		Resource.local().load("custom/paginae/default/wnd/opts"),
		(pag) -> ui.gui.opts.toggleVisibility()));
	addCustom(new CustomPagina(this, "management::chat",
		Resource.local().load("custom/paginae/default/wnd/chat"),
		(pag) -> GameUI.MessageBus.send(new GameUI.ToggleVisibility(GameUI.Wdg.ChatWindow))));
	//Scripts
	final File dir = new File("data/scripts/");
	if (dir.exists()) {
	    final File[] files = dir.listFiles((fdir, name) -> (name.endsWith(".lisp") || name.endsWith(".lua"))
		    && !name.startsWith("_config"));
	    if (files != null) {
		for (final File f : files) {
		    if (f.getName().endsWith(".lisp")) {
		        final var name = f.getName().substring(0, f.getName().lastIndexOf(".lisp"));
			final var res = ScriptIconData.getIcon(name);
		        addCustom(new ScriptPagina(this, name, res,
				(pag) -> ui.sess.details.context.launchLispScript(name, ui.sess.details)));
		    } else if (f.getName().endsWith(".lua")) {
			final var name = f.getName().substring(0, f.getName().lastIndexOf(".lua"));
			final var res = ScriptIconData.getIcon(name);
			addCustom(new ScriptPagina(this, name, res,
				(pag) -> ui.sess.details.context.launchLuaScript(name, ui.sess.details)));
		    }
		}
	    }
	}

	//Keybinds
	binds.put(KeyBind.KB_SCM_ROOT, () -> {
	    if(this.cur != null) {
		this.cur = null;
		curoff = 0;
		updlayout();
		return true;
	    } else {
	        return false;
	    }
	});
	binds.put(KeyBind.KB_SCM_BACK, () -> {
	    if(this.cur != null) {
	        use(bk, new Interaction(), false);
	        return true;
	    } else {
	        return false;
	    }
	});
	binds.put(KeyBind.KB_SCM_NEXT, () -> {
	    if((layout[gsz.x - 2][gsz.y - 1] == next)) {
		use(next, new Interaction(), false);
		return true;
	    } else {
		return false;
	    }
	});
    }

    private void addCustom(final CustomPagina pag) {
	paginae.add(pag);
	custompag.put(pag.key, pag);
    }

    @Override
    protected void added() {
	super.added();
	mailbox = new MailBox<>(ui.office);
	MessageBus.subscribe(mailbox);
	updlayoutsize();
    }

    @Override
    public void dispose() {
	MessageBus.unsubscribe(mailbox);
	super.dispose();
    }

    public void updlayoutsize() {
        gsz = new Coord(GlobalSettings.MENUGRIDSIZEX.get(), GlobalSettings.MENUGRIDSIZEY.get());
	layout = new PagButton[gsz.x][gsz.y];
	updlayout();
	resize(bgsz.mul(gsz).add(UI.scale(1), UI.scale(1)));
    }

    private void updlayout() {
	synchronized(paginae) {
	    List<PagButton> cur = new ArrayList<>();
	    recons = !cons(this.cur, cur);
	    Collections.sort(cur, Comparator.comparing(PagButton::sortkey));
	    this.curbtns = cur;
	    int i = curoff;
	    for(int y = 0; y < gsz.y; y++) {
		for(int x = 0; x < gsz.x; x++) {
		    PagButton btn = null;
		    if((this.cur != null) && (x == gsz.x - 1) && (y == gsz.y - 1)) {
			btn = bk;
		    } else if((cur.size() > ((gsz.x * gsz.y) - 1)) && (x == gsz.x - 2) && (y == gsz.y - 1)) {
			btn = next;
		    } else if(i < cur.size()) {
			btn = cur.get(i++);
		    }
		    layout[x][y] = btn;
		}
	    }
	}
    }

    private static Map<PagButton, Tex> glowmasks = new WeakHashMap<>();
    private Tex glowmask(PagButton pag) {
	Tex ret = glowmasks.get(pag);
	if(ret == null) {
	    ret = new TexI(PUtils.glowmask(PUtils.glowmask(pag.img().getRaster()), 4, new Color(32, 255, 32)));
	    glowmasks.put(pag, ret);
	}
	return(ret);
    }
    public void draw(GOut g) {
	double now = Utils.rtime();
	for(int y = 0; y < gsz.y; y++) {
	    for(int x = 0; x < gsz.x; x++) {
		Coord p = bgsz.mul(new Coord(x, y));
		g.image(bg, p);
		PagButton btn = layout[x][y];
		if(btn != null) {
		    Pagina info = btn.pag;
		    Tex btex;
		    try {
			btex = info.img.get();
			g.image(btex, p.add(UI.scale(1), UI.scale(1)), btex.sz());
		    } catch(NullPointerException e) {
			System.err.println(btn);
			System.err.println(info.scm == this);
			throw(e);
		    }
		    if(info.meter > 0) {
			double m = info.meter;
			if(info.dtime > 0)
			    m += (1 - m) * (now - info.gettime) / info.dtime;
			m = Utils.clip(m, 0, 1);
			g.chcolor(255, 255, 255, 128);
			g.fellipse(p.add(bgsz.div(2)), bgsz.div(2), Math.PI / 2, ((Math.PI / 2) + (Math.PI * 2 * m)));
			g.chcolor();
		    }
		    if(info.newp != 0) {
			if(info.fstart == 0) {
			    info.fstart = now;
			} else {
			    double ph = (now - info.fstart) - (((x + (y * gsz.x)) * 0.15) % 1.0);
			    Tex glow = glowmask(btn);
			    if(ph < 1.25) {
				g.chcolor(255, 255, 255, (int)(255 * ((Math.cos(ph * Math.PI * 2) * -0.5) + 0.5)));
			    } else {
				g.chcolor(255, 255, 255, 128);
			    }
			    g.image(glow, p.sub(4, 4));
			    g.chcolor();
			}
		    }
		    if(btn == pressed) {
			g.chcolor(new Color(0, 0, 0, 128));
			g.frect(p.add(UI.scale(1), UI.scale(1)), btex.sz());
			g.chcolor();
		    }
		}
	    }
	}
	super.draw(g);
	if(dragging != null) {
	    Tex dt = dragging.img.get();
	    ui.drawafter(new UI.AfterDraw() {
		    public void draw(GOut g) {
			g.image(dt, ui.mc.add(dt.sz().div(2).inv()));
		    }
		});
	}
    }

    private PagButton curttp = null;
    private boolean curttl = false;
    private Tex curtt = null;
    private double hoverstart;
    public Object tooltip(Coord c, Widget prev) {
	PagButton pag = bhit(c);
	double now = Utils.rtime();
	if(pag != null) {
	    if(prev != this)
		hoverstart = now;
	    boolean ttl = (now - hoverstart) > 0.5;
	    if((pag != curttp) || (ttl != curttl)) {
		try {
		    BufferedImage ti = pag.rendertt(ttl);
		    curtt = (ti == null) ? null : new TexI(ti);
		} catch(Loading l) {
		    return(null);
		}
		curttp = pag;
		curttl = ttl;
	    }
	    return(curtt);
	} else {
	    hoverstart = now;
	    return(null);
	}
    }

    private PagButton bhit(Coord c) {
	Coord bc = c.div(bgsz);
	if((bc.x >= 0) && (bc.y >= 0) && (bc.x < gsz.x) && (bc.y < gsz.y))
	    return(layout[bc.x][bc.y]);
	else
	    return(null);
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
	return btn == 3 && c.isect(Coord.z, sz);
    }

    public boolean mousedown(Coord c, int button) {
	PagButton h = bhit(c);
	if((button == 1) && (h != null)) {
	    pressed = h;
	    grab = ui.grabmouse(this);
	} else {
	    super.mousedown(c, button);
	}
	return true;
    }

    public void mousemove(Coord c) {
	if((dragging == null) && (pressed != null)) {
	    PagButton h = bhit(c);
	    if(h != pressed)
		dragging = pressed.pag;
	} else {
	    super.mousemove(c);
	}
    }

    public void duse(final String resn) {
        this.duse.add(resn);
    }

    public void use(final String resn) {
        try {
	    final Resource res = Resource.remote().loadwait(resn);
	    wdgmsg("act", (Object[]) res.layer(Resource.action).ad);
	} catch (Exception e) {
            logger.atSevere().withCause(e).log("Failed to use %s", resn);
	}
    }

    public void change(Pagina dst) {
	this.cur = dst;
	curoff = 0;
	updlayout();
    }

    public void use(PagButton r, Interaction iact, boolean reset) {
	Collection<PagButton> sub = new ArrayList<>();
	cons(r.pag, sub);
	if(sub.size() > 0) {
	    change(r.pag);
	} else {
	    r.pag.newp = 0;
	    r.use(iact);
	    if(reset)
		change(null);
	}
	//updlayout();
    }

    public void tick(double dt) {
        if(duse.size() > 0) {
            for(final var sk : duse)
                use(sk);
            duse.clear();
	}
	if(recons)
	    updlayout();
	if(mailbox != null)
	    mailbox.processMail(mail -> mail.apply(this));
    }

    public boolean mouseup(Coord c, int button) {
	PagButton h = bhit(c);
	if((button == 1) && (grab != null)) {
	    if(dragging != null) {
	        if(!(dragging instanceof CustomPagina))
	            ui.dropthing(ui.root, ui.mc, dragging.res());
	        else
	            ui.dropthing(ui.root, ui.mc, dragging);
		pressed = null;
		dragging = null;
	    } else if(pressed != null) {
		if(pressed == h)
		    use(h, new Interaction(), false);
		pressed = null;
	    }
	    grab.remove();
	    grab = null;
	} else {
	    super.mouseup(c, button);
	}
	return(true);
    }

    public void uimsg(String msg, Object... args) {
	if(msg == "goto") {
	    if(args[0] == null)
		change(null);
	    else
		change(paginafor(ui.sess.getres((Integer)args[0])));
	} else if(msg == "fill") {
	    synchronized(paginae) {
		int a = 0;
		while(a < args.length) {
		    int fl = (Integer)args[a++];
		    Pagina pag = paginafor(ui.sess.getres((Integer)args[a++]));
		    if((fl & 1) != 0) {
			pag.state(Pagina.State.ENABLED);
			pag.meter = 0;
			if((fl & 2) != 0)
			    pag.state(Pagina.State.DISABLED);
			if((fl & 4) != 0) {
			    pag.meter = ((Number)args[a++]).doubleValue() / 1000.0;
			    pag.gettime = Utils.rtime();
			    pag.dtime = ((Number)args[a++]).doubleValue() / 1000.0;
			}
			if((fl & 8) != 0)
			    pag.newp = 1;
			if((fl & 16) != 0)
			    pag.rawinfo = (Object[])args[a++];
			else
			    pag.rawinfo = new Object[0];
			paginae.add(pag);
		    } else {
			paginae.remove(pag);
		    }
		}
		updlayout();
	    }
	} else {
	    super.uimsg(msg, args);
	}
    }

    public boolean globtype(char k, KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
	PagButton pag = null;
	for(PagButton btn : curbtns) {
	    if(btn.kb.match(bind)) {
		if((pag == null)) {
		    pag = btn;
		}
	    }
	}
	if(pag != null) {
	    use(pag, new Interaction(), (KeyMatch.mods(ev) & KeyMatch.S) == 0);
	    return(true);
	}
	return(false);
    }
}
