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
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.layout.LinearGrouping;

import java.awt.event.KeyEvent;
import java.util.*;
import java.awt.Font;
import java.awt.Color;
import java.awt.image.BufferedImage;

import static haven.Inventory.invsq;

public class Makewindow extends LinearGrouping {
    public static final Coord qmodsz = UI.scale(20, 20);
    private static final Map<Indir<Resource>, Tex> qmicons = new WeakHashMap<>();
    private static Tex qmicon(Indir<Resource> qm) {
	return(qmicons.computeIfAbsent(qm, res -> new TexI(PUtils.convolve(res.get().layer(Resource.imgc).img,
		qmodsz, CharWnd.iconfilter))));
    }

    public String rcpnm;
    public List<Spec> inputs = Collections.emptyList();
    public List<Spec> outputs = Collections.emptyList();
    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();

    // UI
    private final LinearGrouping items;
    private final LinearGrouping inputlst;
    private final IndirLabel softcap;
    private final LinearGrouping modlst;
    private final LinearGrouping toollst;
    private final LinearGrouping outputlst;

    @RName("make")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new Makewindow((String)args[0]));
	}
    }

    private static final OwnerContext.ClassResolver<Makewindow> ctxr = new OwnerContext.ClassResolver<Makewindow>()
	.add(Glob.class, wdg -> wdg.ui.sess.glob)
	.add(Session.class, wdg -> wdg.ui.sess);
    public class Spec extends Widget implements GSprite.Owner, ItemInfo.SpriteOwner {
	public Indir<Resource> res;
	public MessageBuf sdt;
	public Tex num;
	private GSprite spr;
	private final Object[] rawinfo;
	private List<ItemInfo> info;
	private TexI tip = null;

	public Spec(Indir<Resource> res, Message sdt, int num, Object[] info) {
	    super(Inventory.sqsz);
	    this.res = res;
	    this.sdt = new MessageBuf(sdt);
	    if(num >= 0)
		this.num = new TexI(Utils.outline2(Text.render(Integer.toString(num), Color.WHITE).img, Utils.contrast(Color.WHITE)));
	    else
		this.num = null;
	    this.rawinfo = info;
	}

	public GSprite sprite() {
	    if(spr == null)
		spr = GSprite.create(this, res.get(), sdt.clone());
	    return(spr);
	}

	public void draw(GOut g) {
	    try {
		sprite().draw(g);
	    } catch(Loading ignored) {}
	    if(num != null)
		g.aimage(num, Inventory.sqsz, 1.0, 1.0);
	}

	private int opt = 0;
	public boolean opt() {
	    if(opt == 0) {
		try {
		    opt = (ItemInfo.find(Optional.class, info()) != null) ? 1 : 2;
		} catch(Loading l) {
		    return(false);
		}
	    }
	    return(opt == 1);
	}

	public BufferedImage longtip() {
	    List<ItemInfo> info = info();
	    BufferedImage img;
	    if(info.isEmpty()) {
		Resource.Tooltip tt = res.get().layer(Resource.tooltip);
		if(tt == null)
		    return(null);
		img = Text.render(tt.t).img;
	    } else {
		img = ItemInfo.longtip(info);
	    }
	    Resource.Pagina pg = res.get().layer(Resource.pagina);
	    if(pg != null)
		img = ItemInfo.catimgs(0, img, RichText.render("\n" + pg.text, 200).img);
	    return(img);
	}

	@Override
	public Object tooltip(Coord c, Widget prev) {
	    return tip != null ? tip : (tip = new TexI(longtip()));
	}

	private Random rnd = null;
	public Random mkrandoom() {
	    if(rnd == null)
		rnd = new Random();
	    return(rnd);
	}
	public Resource getres() {return(res.get());}
	public <T> T context(Class<T> cl) {return(ctxr.context(cl, Makewindow.this));}
	@Deprecated
	public Glob glob() {return(ui.sess.glob);}

	public List<ItemInfo> info() {
	    if(info == null)
		info = ItemInfo.buildinfo(this, rawinfo);
	    return(info);
	}
	public Resource resource() {return(res.get());}
    }

    private static class ResWdg extends Widget {
	final Indir<Resource> res;

	public ResWdg(final Indir<Resource> res) {
	    super(qmodsz);
	    this.res = res;
	}

	@Override
	public void draw(GOut g) {
	    super.draw(g);
	    try {
		final Tex qm = qmicon(res);
		g.image(qm, Coord.z);
	    } catch (Loading ignore) {}
	}

	@Override
	public Object tooltip(Coord c, Widget prev) {
	    try {
		final Coord tsz = qmicon(res).sz();
		if(c.isect(Coord.z, tsz))
		    return res.get().layer(Resource.tooltip).t;
	    } catch (Loading ignore) { }
	    return null;
	}
    }

    private interface Stat {
        int value();
    }

    private static class SAttrStat implements Stat {
        private final CharWnd.SAttr attr;
        public SAttrStat(final CharWnd.SAttr attr) { this.attr = attr; }
        public int value() { return attr.attr.comp; }
    }

    private static class AttrStat implements Stat {
	private final CharWnd.Attr attr;
	public AttrStat(final CharWnd.Attr attr) { this.attr = attr; }
	public int value() { return attr.attr.comp; }
    }

    private static class QualityWdg extends ResWdg {
	private Stat stat;

	public QualityWdg(final Indir<Resource> res) {
	    super(res);
	    stat = null;
	    final Coord qsz = FastText.size("0000");
	    resize(sz.add(0, qsz.y).max(qsz.x, 0));
	}

	public int stat() {
	    return stat != null ? stat.value() : 0;
	}

	@Override
	public void tick(double dt) {
	    super.tick(dt);
	    try {
		if(stat == null && ui.gui.chrwdg != null) {
		    final String name = res.get().basename();
		    for (CharWnd.SAttr attr : ui.gui.chrwdg.skill) {
			if (name.equals(attr.attr.nm)) {
			    stat = new SAttrStat(attr);
			    break;
			}
		    }
		    if(stat == null) {
			for (CharWnd.Attr attr : ui.gui.chrwdg.base) {
			    if (name.equals(attr.attr.nm)) {
				stat = new AttrStat(attr);
				break;
			    }
			}
		    }
		}
	    } catch (Loading ignore) {}
	}

	@Override
	public void draw(GOut g) {
	    super.draw(g);
	    int y = 0;
	    try {
		final Tex qm = qmicon(res);
		g.image(qm, new Coord(sz.x / 2 - qm.sz().x/2, y));
		y  += qm.sz().y + UI.scale(5);
	    } catch (Loading ignore) {}
	    final Coord qsz = FastText.size(""+stat());
	    FastText.printf(g, new Coord(sz.x/2 - qsz.x/2, y), "%d", stat());
	}
    }

    public void tick(double dt) {
        super.tick(dt);
	for(Spec s : inputs) {
	    if(s.spr != null)
		s.spr.tick(dt);
	}
	for(Spec s : outputs) {
	    if(s.spr != null)
		s.spr.tick(dt);
	}

	if(items.sz.x < softcap.sz.x) {
	    items.pack();
	    pack();
	    parent.pack();
	}
    }

    public Makewindow(String rcpnm) {
        super(rcpnm, Text.std, UI.scale(0, 5), false, Direction.VERTICAL);
	this.rcpnm = rcpnm;
        final Coord spacer = UI.scale(5, 0);
        items = add(new LinearGrouping(5, false));
	LinearGrouping btns = add(new LinearGrouping(spacer, false, Direction.HORIZONTAL));

        inputlst = items.add(new LinearGrouping("Inputs:", Text.std, spacer,false, LinearGrouping.Direction.HORIZONTAL));
        inputlst.pack();
        modlst = items.add(new LinearGrouping("Quality:", Text.std,spacer, false, LinearGrouping.Direction.HORIZONTAL));
	modlst.pack();
	softcap = items.add(new IndirLabel(this::softcap));
        toollst = items.add(new LinearGrouping("Tools:", Text.std, spacer, false, LinearGrouping.Direction.HORIZONTAL));
	toollst.hide();
        toollst.pack();
        outputlst = items.add(new LinearGrouping("Result:", Text.std, spacer, false, LinearGrouping.Direction.HORIZONTAL));
	outputlst.pack();
	items.pack();

	btns.add(new Button(UI.scale(85), "Craft").action(() -> wdgmsg("make", 0)));
	btns.add(new Button(UI.scale(85), "Craft All").action(() -> wdgmsg("make", 1)));
	btns.pack();

	binds.put(KeyBind.KB_MAKE_ONE, () -> {wdgmsg("make", 0); return true;});
	binds.put(KeyBind.KB_MAKE_ALL, () -> {wdgmsg("make", 1); return true;});
	pack();
    }

    private String softcap() {
	int mods = 0;
	int stats = 1;
        for(Widget wdg = modlst.child; wdg != null; wdg = wdg.next) {
	    if(wdg instanceof QualityWdg) {
	        mods++;
	        stats *= ((QualityWdg) wdg).stat();
	    }
	}
        if(mods == 0)
            return "Softcap: 0";
        else
            return String.format("Softcap: %.2f", Math.pow(stats, 1.0/mods));
    }

    @Override
    public boolean globtype(char key, KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
	return(super.globtype(key, ev));
    }

    public void uimsg(String msg, Object... args) {
	switch (msg) {
	    case "inpop" -> {
		List<Spec> inputs = new LinkedList<>();
		for (int i = 0; i < args.length; ) {
		    int resid = (Integer) args[i++];
		    Message sdt = (args[i] instanceof byte[]) ? new MessageBuf((byte[]) args[i++]) : MessageBuf.nil;
		    int num = (Integer) args[i++];
		    Object[] info = {};
		    if ((i < args.length) && (args[i] instanceof Object[]))
			info = (Object[]) args[i++];
		    final Spec spec = new Spec(ui.sess.getres(resid), sdt, num, info);
		    inputs.add(spec);
		    inputlst.add(spec);
		}
		this.inputs = inputs;
		inputlst.pack();
		items.pack();
		pack();
		parent.pack();
	    }
	    case "opop" -> {
		List<Spec> outputs = new LinkedList<>();
		for (int i = 0; i < args.length; ) {
		    int resid = (Integer) args[i++];
		    Message sdt = (args[i] instanceof byte[]) ? new MessageBuf((byte[]) args[i++]) : MessageBuf.nil;
		    int num = (Integer) args[i++];
		    Object[] info = {};
		    if ((i < args.length) && (args[i] instanceof Object[]))
			info = (Object[]) args[i++];
		    final Spec spec = new Spec(ui.sess.getres(resid), sdt, num, info);
		    outputs.add(spec);
		    outputlst.add(spec);
		}
		this.outputs = outputs;
		outputlst.pack();
		items.pack();
		pack();
		parent.pack();
	    }
	    case "qmod" -> {
	        for(final var arg : args) {
		    modlst.add(new QualityWdg(ui.sess.getres((Integer)arg)));
		}
		modlst.pack();
		items.pack();
		pack();
		parent.pack();
	    }
	    case "tool" -> {
	        toollst.add(new ResWdg(ui.sess.getres((Integer) args[0])));
	        toollst.show();
	        toollst.pack();
		items.pack();
		pack();
		parent.pack();
	    }
	    default -> super.uimsg(msg, args);
	}
    }

    public static class Optional extends ItemInfo.Tip {
	public static final Text text = RichText.render("$i{Optional}", 0);
	public Optional(Owner owner) {
	    super(owner);
	}

	public BufferedImage tipimg() {
	    return(text.img);
	}

	public Tip shortvar() {return(this);}
    }

    public static class MakePrep extends ItemInfo implements GItem.ColorInfo {
	private final static Color olcol = new Color(0, 255, 0, 64);
	public MakePrep(Owner owner) {
	    super(owner);
	}

	public Color olcol() {
	    return(olcol);
	}
    }
}
