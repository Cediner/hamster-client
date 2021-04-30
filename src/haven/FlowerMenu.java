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
import hamster.KeyBind;
import hamster.data.MenuExclusionData;

import java.awt.Color;
import java.util.Map;
import java.util.function.Consumer;

import static java.lang.Math.PI;

public class FlowerMenu extends Widget {
    public static final Color pink = new Color(255, 0, 128);
    public static final Color ptc = Color.YELLOW;
    public static final Text.Foundry ptf = new Text.Foundry(Text.dfont, 12);
    public static final IBox pbox = Window.wbox;
    public static final Tex pbg = Window.bg.tex();
    public static final int ph = UI.scale(30);
    public Petal[] opts;
    private UI.Grab mg, kg;
    public static final Coord customBoxPadding = UI.scale(4, 4);
    private final Consumer<String> callback;
    private final boolean srvbased;

    @RName("sm")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    String[] opts = new String[args.length];
	    for(int i = 0; i < args.length; i++)
		opts[i] = (String)args[i];
	    return(new FlowerMenu(opts));
	}
    }

    public class Petal extends Widget {
	public String name;
	public final Consumer<String> callback;
	public double ta, tr;
	public int num;
	protected Text text;
	protected double a = 1;

	public Petal(final String name, final Consumer<String> callback) {
	    super(Coord.z);
	    this.name = name;
	    this.callback = callback;
	    text = ptf.render(name, ptc);
	    resize(text.sz().x + UI.scale(25), text.sz().y + UI.scale(5));
	}

	public Petal(String name) {
	    this(name, null);
	}

	public void move(Coord c) {
	    this.c = c.sub(sz.div(2));
	}

	public void move(double a, double r) {
	    move(Coord.sc(a, r));
	}

	public void draw(GOut g) {
	    g.chcolor(new Color(255, 255, 255, (int)(255 * a)));
	    g.image(pbg, UI.scale(3, 3), UI.scale(3, 3), sz.add(UI.scale(-6, -6)), UI.scale(pbg.sz()));
	    pbox.draw(g, Coord.z, sz);
	    g.image(text.tex(), sz.div(2).sub(text.sz().div(2)));
	}

	public boolean mousedown(Coord c, int button) {
	    choose(this);
	    return(true);
	}

	public Area ta(Coord tc) {
	    return(Area.sized(tc.sub(sz.div(2)), sz));
	}

	public Area ta(double a, double r) {
	    return(ta(Coord.sc(a, r)));
	}
    }

    public class CustomPetal extends Petal {
	boolean h = false;

	private CustomPetal(String name, final Consumer<String> callback) {
	    super(name, callback);
	    sz = new Coord(text.sz().x + UI.scale(35), UI.scale(30));
	}

	private CustomPetal(String name) {
	    this(name, null);
	}

	@Override
	public void draw(GOut g) {
	    g.chcolor(new Color(255, 255, 255, (int) (255 * a)));
	    Coord bgc = new Coord();
	    for (bgc.y = 0; bgc.y < sz.y; bgc.y += pbg.sz().y) {
		for (bgc.x = 0; bgc.x < sz.x; bgc.x += pbg.sz().x)
		    g.image(pbg, bgc, Coord.z, sz);
	    }
	    g.chcolor();
	    //
	    if (h) {
		g.chcolor(0, 0, 0, (int) (128 * a));
		g.frect(Coord.z, sz);
		g.chcolor(new Color(255, 255, 255, (int) (255 * a)));
	    }
	    FastText.aprint(g, new Coord(0, sz.y/2), -1, 0.5, Integer.toString((num + 1) % 10));
	    g.image(text.tex(), sz.sub(UI.scale(8, 0)).sub(text.sz()).div(2).add(UI.scale(8, 0)));
	    g.chcolor();
	}

	@Override
	public void move(double a, double r) {
	}

	@Override
	public void mousemove(Coord c) {
	    h = c.isect(Coord.z, sz.sub(UI.scale(1,1)));
	}
    }

    private static double nxf(double a) {
	return(-1.8633 * a * a + 2.8633 * a);
    }

    public class Chosen extends NormAnim {
	Petal chosen;
		
	Chosen(Petal c) {
	    super(0.75);
	    chosen = c;
	}
		
	public void ntick(double s) {
	    double ival = 0.8;
	    double off = ((1.0 - ival) / (opts.length - 1));
	    for(int i = 0; i < opts.length; i++) {
		Petal p = opts[i];
		if(p == chosen) {
		    if(s > 0.6) {
			p.a = 1 - ((s - 0.6) / 0.4);
		    } else if(s < 0.3) {
			double a = nxf(s / 0.3);
			p.move(p.ta, p.tr * (1 - a));
		    }
		} else {
		    if(s > 0.3) {
			p.a = 0;
		    } else {
			double a = s / 0.3;
			a = Utils.clip((a - (off * i)) * (1.0 / ival), 0, 1);
			p.a = 1 - a;
		    }
		}
	    }
	    if(s == 1.0)
		ui.destroy(FlowerMenu.this);
	}
    }

    public class Cancel extends NormAnim {
	Cancel() {super(0.25);}

	public void ntick(double s) {
	    double ival = 0.8;
	    double off = (opts.length == 1) ? 0.0 : ((1.0 - ival) / (opts.length - 1));
	    for(int i = 0; i < opts.length; i++) {
		Petal p = opts[i];
		double a = Utils.clip((s - (off * i)) * (1.0 / ival), 0, 1);
		double b = 1.0 - nxf(1.0 - a);
		p.move(p.ta + (b * PI), p.tr * (1 - b));
		p.a = 1 - a;
	    }
	    if(s == 1.0)
		ui.destroy(FlowerMenu.this);
	}
    }

    private void organize(Petal[] opts) {
	int width = UI.scale(80);
	for (Petal petal : opts)
	    width = Math.max(width, petal.sz.x);
	Coord c = new Coord(customBoxPadding);
	for (Petal petal : opts) {
	    petal.c = new Coord(c);
	    petal.resize(width, petal.sz.y);
	    c.y += petal.sz.y - UI.scale(1);
	}
	pack();
	// clip to parent
	int x = Utils.clip(this.c.x, 0, parent.sz.x - sz.x);
	int y = Utils.clip(this.c.y, 0, parent.sz.y - sz.y);
	this.c = new Coord(x, y);
    }

    public FlowerMenu(final Map<String, Consumer<String>> optmap) {
        super(Coord.z);
        this.callback = null;
        opts = new Petal[optmap.size()];
        int i = 0;
        for(final var opt : optmap.keySet()) {
            opts[i] = add(new CustomPetal(opt, optmap.get(opt)));
            opts[i].num = i;
	}
	srvbased = false;
    }

    public FlowerMenu(final Consumer<String> callback, final String... options) {
	super(Coord.z);
	this.callback = callback;
	opts = new Petal[options.length];
	for (int i = 0; i < options.length; i++) {
	    add(opts[i] = new CustomPetal(options[i]));
	    opts[i].num = i;
	}
	srvbased = callback == null;
    }

    public FlowerMenu(String... options) {
        this(null, options);
    }

    protected void added() {
	if(c.equals(-1, -1))
	    c = parent.ui.lcc;
	mg = ui.grabmouse(this);
	kg = ui.grabkeys(this);
	organize(opts);
    }

    public boolean mousedown(Coord c, int button) {
	if(!anims.isEmpty())
	    return(true);
	if(GlobalSettings.KEEPFLOPEN.get())
	    return super.mousedown(c, button);
	if(!super.mousedown(c, button))
	    choose(null);
	return(true);
    }

    public void uimsg(String msg, Object... args) {
	if(msg.equals("cancel")) {
	    new Cancel();
	    mg.remove();
	    kg.remove();
	} else if(msg.equals("act")) {
	    new Chosen(opts[(Integer)args[0]]);
	    mg.remove();
	    kg.remove();
	}
    }

    public void draw(GOut g) {
	super.draw(g, false);
	Window.wbox.draw(g, Coord.z, sz);
    }

    public boolean keydown(java.awt.event.KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	char key = ev.getKeyChar();
	if((key >= '0') && (key <= '9')) {
	    int opt = (key == '0')?10:(key - '1');
	    if(opt < opts.length) {
		choose(opts[opt]);
		kg.remove();
	    }
	    return(true);
	} else if(kb_esc.match(bind)) {
	    choose(null);
	    kg.remove();
	    return(true);
	}
	return(false);
    }

    public void choose(Petal option) {
	if (callback == null) {
	    if (option == null) {
	        if(srvbased)
	            wdgmsg("cl", -1);
	        else
	            ui.destroy(this);
	    } else if(option.callback  != null) {
	        option.callback.accept(option.name);
	        ui.destroy(this);
	    } else {
		wdgmsg("cl", option.num, ui.modflags());
	    }
	} else {
	    if (option != null)
		callback.accept(option.name);
	    ui.destroy(this);
	}
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	if(ui.gui != null && ui.gui.fmAutoSelectOpt != null) {
	    for(final var opt : opts) {
		if(opt.name.equals(ui.gui.fmAutoSelectOpt)) {
		    wdgmsg("cl", opt.num, 0);
		    hide();
		    ui.gui.fmAutoSelectOpt = null;
		    return;
		}
	    }
	    ui.gui.fmAutoSelectOpt = null;
	}
    }

    /*****************************************************************************************
     *  FlowerMenu Scripting API
     *****************************************************************************************/
    @Override
    public void binded() {
	ui.sess.details.attachFlowermenu(this);
	int jack = ui.modflags();

	if(ui.gui.fmAutoSelectOpt != null) {
	    for(final var opt : opts) {
	        if(opt.name.equals(ui.gui.fmAutoSelectOpt)) {
	            wdgmsg("cl", opt.num, 0);
	            hide();
		    ui.gui.fmAutoSelectOpt = null;
	            return;
		}
	    }
	    ui.gui.fmAutoSelectOpt = null;
	}

	if(!ui.gui.fmOverrideSettings) {
	    if (GlobalSettings.AUTOONEOPTFMENU.get() && opts.length == 1 && !MenuExclusionData.isExcluded(opts[0].name)) {
		wdgmsg("cl", 0, 0);
		hide();
	    } else if (GlobalSettings.QUICKFLMENU.get() && jack < opts.length && opts.length <= 3) {
		if (opts[0].name.equals("Empty")) {
		    if (opts.length == 1) return; //don't jackui a single empty
		    //switch options for containers
		    jack = jack == 1 ? 0 : 1;
		}
		if (!MenuExclusionData.isExcluded(opts[jack].name)) {
		    wdgmsg("cl", jack, 0);
		    hide();
		}
	    } else if (GlobalSettings.QUICKFLMENU.get() && jack < opts.length && opts[1].name.equals("Sip")) {
		if (!MenuExclusionData.isExcluded(opts[jack].name)) {
		    wdgmsg("cl", jack, 0);
		    hide();
		}
	    }
	} else {
	    ui.gui.fmOverrideSettings = false;
	}
    }

    @Override
    protected void removed() {
	ui.sess.details.removeFlowermenu();
    }

    @SuppressWarnings("unused")
    public Petal[] getOpts() {
        return opts;
    }

    @SuppressWarnings("unused")
    public void choose(final Petal option, final int flags) {
	if (callback == null) {
	    if (option == null) {
		wdgmsg("cl", -1);
	    } else {
		wdgmsg("cl", option.num, flags);
	    }

	} else {
	    if (option != null)
		callback.accept(option.name);
	    ui.destroy(this);
	}
    }
}
