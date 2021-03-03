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

import hamster.GlobalSettings;
import hamster.data.WindowData;
import hamster.ui.core.MovableWidget;
import hamster.ui.core.Theme;
import hamster.ui.core.indir.IndirThemeRes;
import hamster.ui.core.indir.IndirThemeTex;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Objects;

import static haven.PUtils.*;
import static haven.Resource.cdec;

public class Window extends MovableWidget implements DTarget {
    @Resource.LayerName("windowconfig")
    public static class WindowConfig extends Resource.Layer {
	final Coord tlc;
	final Coord brc;
	final Coord capc;
	final Coord btnc;

	public WindowConfig(Resource res, Message buf) {
	    res.super();
	    tlc = cdec(buf);
	    brc = cdec(buf);
	    capc = cdec(buf);
	    btnc = cdec(buf);
	}

	public void init() { }
    }

    private static final IndirThemeRes res = Theme.themeres("window");
    //bg, left bg, right bg
    public static final IndirThemeTex bg = res.tex(0);
    public static final IndirThemeTex bgl = res.tex(1);
    public static final IndirThemeTex bgr = res.tex(2);
    //caption left, mid, right
    public static final IndirThemeTex cl = res.tex(3);
    public static final IndirThemeTex cm = res.tex(4);
    public static final IndirThemeTex cr = res.tex(5);
    // bottom left, right
    public static final IndirThemeTex bl = res.tex(6);
    public static final IndirThemeTex br = res.tex(7);
    //left, right, bottom
    public static final IndirThemeTex lm = res.tex(8);
    public static final IndirThemeTex rm = res.tex(9);
    public static final IndirThemeTex bm = res.tex(10);

    //Large margin vs small margin
    public static final Coord dlmrgn = UI.scale(23, 14), dsmrgn = UI.scale(3, 3);
    //caption foundry
    public static final BufferedImage ctex = Resource.loadimg("gfx/hud/fonttex");
    public static final Text.Furnace cf = new Text.Imager(new PUtils.TexFurn(new Text.Foundry(Text.fraktur, 15).aa(true), ctex)) {
	protected BufferedImage proc(Text text) {
	    // return(rasterimg(blurmask2(text.img.getRaster(), 1, 1, Color.BLACK)));
	    return(rasterimg(blurmask2(text.img.getRaster(), UI.rscale(0.75), UI.rscale(1.0), Color.BLACK)));
	}
    };
    //Basic frame box
    public static final IBox wbox = new IBox("frame") {
	final Coord co = UI.scale(3, 3), bo = UI.scale(2, 2);

	public Coord btloff() {
	    return (super.btloff().sub(bo));
	}

	public Coord ctloff() {
	    return (super.ctloff().sub(co));
	}

	public Coord bisz() {
	    return (super.bisz().sub(bo.mul(2)));
	}

	public Coord cisz() {
	    return (super.cisz().sub(co.mul(2)));
	}
    };

    //margin based off large or not
    public final Coord mrgn;
    //close button
    public final IButton cbtn, lbtn;
    public final ArrayList<IButton> btns = new ArrayList<>();

    public boolean dt = false;
    //Caption
    public Text cap;
    //Window size, usable space top left, usable space size
    public Coord wsz, atl, asz;
    //close position, close size
    public Coord ctl, csz;

    // Removing the window deco when hidden if allowed
    private boolean hidable = false, hidden;
    private IButton hbtn;
    // Removing the window content up to the top bar if allowed
    private boolean folded = false;
    private IButton fbtn;


    @RName("wnd")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    Coord sz = UI.scale((Coord)args[0]);
	    String cap = (args.length > 1) ? (String)args[1] : null;
	    boolean lg = args.length > 2 && ((Integer) args[2] != 0);
	    return(new Window(sz, cap, lg, Coord.z, Coord.z));
	}
    }


    /* Added Constructors */
    public Window(Coord sz, String cap, final String moveKey, boolean lg, Coord tlo, Coord rbo) {
	super(moveKey);
	this.mrgn = lg ? dlmrgn : dsmrgn;
	cbtn = add(new IButton("buttons/close", null, this::close));
	lbtn = add(new IButton("buttons/lock", null, this::toggleLock));
	chcap(cap);
	resize2(sz);
	setfocustab(true);
    }

    public Window(Coord sz, String cap, String moveKey) {
	this(sz, cap, moveKey, false, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap, String moveKey, boolean lg) {
	this(sz, cap, moveKey, lg, Coord.z, Coord.z);
    }

    /* Default Base Client Constructors */
    public Window(Coord sz, String cap, boolean lg, Coord tlo, Coord rbo) {
	this.mrgn = lg ? dlmrgn : dsmrgn;
	cbtn = add(new IButton("buttons/close", null, this::close));
	lbtn = null;
	chcap(cap);
	resize2(sz);
	setfocustab(true);
    }

    public Window(Coord sz, String cap, boolean lg) {
	this(sz, cap, lg, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap) {
	this(sz, cap, false);
    }

    protected void added() {
        super.added();
	parent.setfocus(this);
    	if(lbtn != null && locked()) {
    	    lbtn.swap(IButton.Type.UP, IButton.Type.HOVER);
	}
    }

    public void makeHidable() {
	hbtn = addBtn("buttons/hide", null, this::toggleHide);
	if (cap != null) {
	    hidable = WindowData.shouldHide(cap.text);
	    hidden = false;
	    if (hidable) {
		hbtn.swap(IButton.Type.DOWN, IButton.Type.UP);
	    }
	}
    }

    public void makeFoldable() {
        fbtn = addBtn("buttons/fold", null, this::toggleFold);
	if (cap != null) {
	    folded = WindowData.shouldFold(cap.text);
	    if (folded) {
		fbtn.swap(IButton.Type.DOWN, IButton.Type.UP);
	    }
	}
    }

    public void toggleHide() {
	hidable = !hidable;
	hidden = false;
	hbtn.swap(IButton.Type.DOWN, IButton.Type.UP);
	if (cap != null) {
	    WindowData.save(cap.text, hidable, folded);
	}
    }

    public void toggleFold() {
        folded = !folded;
	fbtn.swap(IButton.Type.DOWN, IButton.Type.UP);
	if (cap != null) {
	    WindowData.save(cap.text, folded, hidable);
	}
    }

    public IButton addBtn(final String res, final String tt, final Runnable action) {
	final IButton btn = add(new IButton(res, tt, action));
	btns.add(btn);
	return btn;
    }

    public IButton addBtn(final String res, final String tt, final Runnable action, final Runnable laction) {
	final IButton btn = add(new IButton(res, tt, action, laction));
	btns.add(btn);
	return btn;
    }

    @Override
    public void toggleLock() {
	lbtn.swap(IButton.Type.UP, IButton.Type.HOVER);
	super.toggleLock();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
	final TexI cl = Window.cl.tex(), cr = Window.cr.tex(), cm = Window.cm.tex();
	Coord cpc = c.sub(cl.sz().x, 0);
	Coord cprc = c.sub(sz.x - cr.sz().x, 0);
	//content size
	return c.isect(ctl, csz) ||
		//or left caption
		(c.isect(Coord.z, cl.sz()) && cl.back.getRaster().getSample(c.x, c.y, 3) >= 128) ||
		//or right caption
		(c.isect(new Coord(sz.x - cr.sz().x, 0), cr.sz()) &&
			cr.back.getRaster().getSample(cprc.x % cr.back.getWidth(), cprc.y, 3) >= 128) ||
		//or mid caption
		(c.isect(new Coord(cl.sz().x, 0), new Coord(sz.x - cr.sz().x - cl.sz().x, cm.sz().y)) &&
			(cm.back.getRaster().getSample(cpc.x % cm.back.getWidth(), cpc.y, 3) >= 128));
    }

    public void close() {
	wdgmsg("close");
    }

    public void chcap(String cap) {
	if(cap == null)
	    this.cap = null;
	else
	    this.cap = cf.render(cap);
    }

    public void cdraw(GOut g) {
    }

    protected void drawframe(GOut g) {
	g.chcolor(GlobalSettings.WNDCOL.get());
	final Tex bgl = Window.bgl.tex(), bgr = Window.bgr.tex(), bg = Window.bg.tex();
	final Tex cl = Window.cl.tex(), bl = Window.bl.tex(), br = Window.br.tex(), cr = Window.cr.tex();
	final Tex lm = Window.lm.tex(), rm = Window.rm.tex(), bm = Window.bm.tex(), cm = Window.cm.tex();
	final WindowConfig cfg = Window.res.layer(WindowConfig.class);

	//draw background
	g.rimagev(bgl, ctl, csz.y);
	g.rimagev(bgr, ctl.add(csz.x - bgr.sz().x, 0), csz.y);
	g.rimage(bg, ctl, csz);


	//corners
	g.image(cl, Coord.z);
	g.image(bl, new Coord(0, sz.y - bl.sz().y));
	g.image(br, sz.sub(br.sz()));
	g.image(cr, new Coord(sz.x - cr.sz().x, 0));

	//horizontal and vertical tiling of the long parts
	g.rimagev(lm, new Coord(0, cl.sz().y), sz.y - bl.sz().y - cl.sz().y);
	g.rimagev(rm, new Coord(sz.x - rm.sz().x, cr.sz().y), sz.y - br.sz().y - cr.sz().y);
	g.rimageh(bm, new Coord(bl.sz().x, sz.y - bm.sz().y), sz.x - br.sz().x - bl.sz().x);
	g.rimageh(cm, new Coord(cl.sz().x, 0), sz.x - cl.sz().x - cr.sz().x);
	g.chcolor();

	//caption if applies
	if (cap != null) {
	    g.image(cap.tex(), UI.scale(cfg.capc));
	}
    }

    public void draw(GOut g) {
	if (!hidden) {
	    drawframe(g);
	}
	cdraw(g.reclip(atl, asz));
	super.draw(g);
    }

    public Coord contentsz() {
	Coord max = new Coord(0, 0);
	for(Widget wdg = child; wdg != null; wdg = wdg.next) {
	    if(wdg == cbtn)
		continue;
	    if(!wdg.visible)
		continue;
	    Coord br = wdg.c.add(wdg.sz);
	    if(br.x > max.x)
		max.x = br.x;
	    if(br.y > max.y)
		max.y = br.y;
	}
	return(max);
    }

    private void placecbtn() {
	final WindowConfig cfg = Window.res.layer(WindowConfig.class);
	cbtn.c = new Coord(sz.x - cbtn.sz.x - atl.x - UI.scale(cfg.btnc.x), -atl.y + UI.scale(cfg.btnc.y));
	final Coord c;
	if (lbtn != null) {
	    lbtn.c = cbtn.c.sub(lbtn.sz.x + UI.scale(5), 0);
	    c = new Coord(lbtn.c.x - (lbtn.sz.x + UI.scale(5)), lbtn.c.y);
	} else {
	    c = new Coord(cbtn.c.x - (cbtn.sz.x + UI.scale(5)), cbtn.c.y);
	}
	for (final IButton btn : btns) {
	    btn.c = c.copy();
	    c.x -= btn.sz.x + UI.scale(5);
	}
    }

    private void resize2(Coord sz) {
	final WindowConfig cfg = Window.res.layer(WindowConfig.class);
	asz = sz; //usable size for content
	csz = asz.add(mrgn.mul(2)); //add margin around usable size
	wsz = csz.add(UI.scale(cfg.tlc)).add(UI.scale(cfg.brc)); //usable size + margin + frame size
	//tlo, rbo = top left offset, bottom right offset usually 0 always...
	//Basically same job as tlc, brc
	this.sz = wsz;
	//top left coordinate of inner content area
	ctl = UI.scale(cfg.tlc);
	//Top left coordinate of where usable space starts after accounting for margin
	atl = ctl.add(mrgn);
	//Where the close button goes
	cbtn.c = new Coord(sz.x - UI.scale(cfg.btnc.x) - cbtn.sz.x, UI.scale(cfg.btnc.y));
	for (Widget ch = child; ch != null; ch = ch.next)
	    ch.presize();
	placecbtn();
    }

    public void resize(Coord sz) {
	resize2(sz);
    }

    @Deprecated
    public void decohide(boolean h) {
    }

    @Deprecated
    public boolean decohide() {
	return(hidable);
    }

    public void uimsg(String msg, Object... args) {
	if(msg == "dt") {
	    dt = (Integer)args[0] != 0;
	} else if(msg == "cap") {
	    String cap = (String)args[0];
	    chcap(cap.equals("") ? null : cap);
	} else if(msg == "dhide") {
	    decohide((Integer)args[0] != 0);
	} else {
	    super.uimsg(msg, args);
	}
    }

    public Coord xlate(Coord c, boolean in) {
	if(in)
	    return(c.add(atl));
	else
	    return(c.sub(atl));
    }

    public void mousemove(Coord c) {
	if (hidable) {
	    if (c.isect(Coord.z, sz) || moving()) {
		hidden = false;
		cbtn.visible = true;
		if (lbtn != null)
		    lbtn.visible = true;
		btns.forEach(btn -> btn.visible = true);
	    } else {
		hidden = true;
		cbtn.visible = false;
		if (lbtn != null)
		    lbtn.visible = false;
		btns.forEach(btn -> btn.visible = false);
	    }
	}
	super.mousemove(c);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
	if(sender == cbtn) {
	    close();
	} else {
	    super.wdgmsg(sender, msg, args);
	}
    }

    public boolean keydown(java.awt.event.KeyEvent ev) {
	if(super.keydown(ev))
	    return(true);
	if(key_esc.match(ev)) {
	    close();
	    return(true);
	}
	return(false);
    }

    public boolean drop(Coord cc, Coord ul) {
	if(dt) {
	    wdgmsg("drop", cc);
	    return(true);
	}
	return(false);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
	return(false);
    }

    public Object tooltip(Coord c, Widget prev) {
	if(!checkhit(c))
	    return(super.tooltip(c, prev));
	Object ret = super.tooltip(c, prev);
	return Objects.requireNonNullElse(ret, "");
    }

    public static void main(String[] args) {
	Window wnd = new Window(new Coord(300, 200), "Inventory", true);
	new haven.rs.DrawBuffer(haven.rs.Context.getdefault().env(), new Coord(512, 512))
	    .draw(g -> {
		    wnd.draw(g);
		    g.getimage(img -> Debug.dumpimage(img, args[0]));
	    });
    }
}
