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
import hamster.IndirSetting;
import hamster.ui.core.MovableWidget;

import java.awt.Color;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IMeter extends MovableWidget {
    enum Type {
	HP, STAM, ENERGY, OTHER
    }

    private static final Pattern hppat = Pattern.compile("Health: ([0-9]+)/([0-9]+)/([0-9]+)");
    private static final Pattern stampat = Pattern.compile("Stamina: ([0-9]+)");
    private static final Pattern energypat = Pattern.compile("Energy: ([0-9]+)");
    private static final Color bgc = new Color(72, 72, 72, 192);
    public static final Coord off = UI.scale(22, 7);
    public static final Coord fsz = UI.scale(101, 24);
    public static final Coord msz = UI.scale(75, 10);
    static Coord bsz = UI.scale(200, 25);
    static Coord boff = UI.scale(2, 2);
    static Coord bmsz = bsz.sub(boff);
    public final Indir<Resource> bg;
    public List<Meter> meters;
    private Tex tt;
    private final Type t;
    
    @RName("im")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    Indir<Resource> bg = ui.sess.getres((Integer)args[0]);
	    final Session.CachedRes.Ref res = (Session.CachedRes.Ref) bg;
	    List<Meter> meters = decmeters(args, 1);
	    final Type t = switch (res.name()) {
		case "gfx/hud/meter/hp" -> Type.HP;
		case "gfx/hud/meter/stam" -> Type.STAM;
		case "gfx/hud/meter/nrj" -> Type.ENERGY;
		default -> Type.OTHER;
	    };
	    return(new IMeter(bg, meters, "meter-" + args[0], t));
	}
    }
    
    public IMeter(Indir<Resource> bg, List<Meter> meters, final String name, final Type t) {
	super(fsz, name);
	this.bg = bg;
	this.meters = meters;
	this.t = t;
    }

    @Override
    protected void added() {
	super.added();
	switch (t) {
	    case HP -> {
		ui.gui.hp = this;
	        checkVisiblity(GlobalSettings.SHOWHEALTH);
	    }
	    case STAM -> {
		ui.gui.stam = this;
	        checkVisiblity(GlobalSettings.SHOWSTAM);
	    }
	    case ENERGY -> {
		ui.gui.energy = this;
	        checkVisiblity(GlobalSettings.SHOWENERGY);
	    }
	}
    }

    private void checkVisiblity(final IndirSetting<Boolean> setting) {
	if (visible && !setting.get()) {
	    hide();
	} else if (!visible && setting.get()) {
	    show();
	}
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
	return c.isect(Coord.z, sz);
    }
    
    public static class Meter {
	public final Color c;
	public final double a;
	
	public Meter(Color c, double a) {
	    this.c = c;
	    this.a = a;
	}
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	if (GlobalSettings.BIGSIMPLEMETERS.get()) {
	    if (!sz.equals(bsz))
		resize(bsz);
	} else if (!sz.equals(fsz)) {
	    resize(fsz);
	}
    }

    public void drawbig(GOut g) {
	g.chcolor(0, 0, 0, 255);
	g.frect(boff, bmsz);
	g.chcolor();
	for (Meter m : meters) {
	    int w = bmsz.x;
	    w = (int) Math.ceil(w * m.a);
	    g.chcolor(m.c);
	    g.frect(boff, new Coord(w, bmsz.y));
	}
	g.chcolor();
	if (tt != null) {
	    g.chcolor(bgc);
	    g.frect(sz.div(2).sub(tt.sz().div(2)), tt.sz());
	    g.chcolor();
	    g.aimage(tt, sz.div(2), 0.5f, 0.5f);
	}
	Window.wbox.draw(g, Coord.z, sz);
    }

    public void draw(GOut g) {
	if (!GlobalSettings.BIGSIMPLEMETERS.get()) {
	    try {
		Tex bg = this.bg.get().layer(Resource.imgc).tex();
		g.chcolor(0, 0, 0, 255);
		g.frect(off, msz);
		g.chcolor();
		for (Meter m : meters) {
		    int w = msz.x;
		    w = (int) Math.ceil(w * m.a);
		    g.chcolor(m.c);
		    g.frect(off, new Coord(w, msz.y));
		}
		g.chcolor();
		g.image(bg, Coord.z);
		if (tt != null)
		    g.aimage(tt, sz.div(2), 0.5f, 0.5f);
	    } catch (Loading ignored) { }
	} else {
	    drawbig(g);
	}
    }
    
    private static List<Meter> decmeters(Object[] args, int s) {
	ArrayList<Meter> buf = new ArrayList<>();
	for(int a = s; a < args.length; a += 2)
	    buf.add(new Meter((Color)args[a], ((Number)args[a + 1]).doubleValue() * 0.01));
	buf.trimToSize();
	return(buf);
    }

    public void uimsg(String msg, Object... args) {
	if(msg.equals("set")) {
	    this.meters = decmeters(args, 0);
	} else {
	    super.uimsg(msg, args);
	    if(msg.equals("tip")) {
		final String tt = (String) args[0];
		this.tt = Text.renderstroked(tt.substring(tt.indexOf(':') + 1)).tex();
		switch (t) {
		    case HP -> {
			final Matcher matcher = hppat.matcher(tt);
			if (matcher.find()) {
			    ui.sess.details.shp = Integer.parseInt(matcher.group(1));
			    ui.sess.details.hhp = Integer.parseInt(matcher.group(2));
			    ui.sess.details.mhp = Integer.parseInt(matcher.group(3));
			    setVisible(GlobalSettings.SHOWHEALTH.get());
			}
		    }
		    case STAM -> {
			final Matcher matcher = stampat.matcher(tt);
			if (matcher.find()) {
			    ui.sess.details.stam = Integer.parseInt(matcher.group(1));
			    setVisible(GlobalSettings.SHOWSTAM.get());
			}
		    }
		    case ENERGY -> {
			final Matcher matcher = energypat.matcher(tt);
			if (matcher.find()) {
			    ui.sess.details.energy = Integer.parseInt(matcher.group(1));
			    setVisible(GlobalSettings.SHOWENERGY.get());
			}
		    }
		}
	    }
	}
    }
}
