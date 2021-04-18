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
import hamster.ui.core.Theme;
import hamster.ui.core.indir.IndirThemeRes;
import hamster.ui.core.indir.IndirThemeTex;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.function.Consumer;

public class TextEntry extends SIWidget {
    public static final Color defcol = new Color(255, 205, 109), dirtycol = new Color(255, 232, 209);
    public static final Text.Foundry fnd = new Text.Foundry(Text.serif, 12).aa(true);
    private static final IndirThemeRes res = Theme.themeres("textedit");
    public static final IndirThemeTex lcap = res.img(0);
    public static final IndirThemeTex rcap = res.img(2);
    public static final IndirThemeTex mext = res.img(1);
    public static final Tex caret = Resource.loadtex("gfx/hud/text/caret");
    public static final Coord toff = new Coord(lcap.imgs().getWidth() - 1, 1);
    public static final Coord coff = UI.scale(new Coord(-3, 0));
    public static final int wmarg = lcap.imgs().getWidth() + rcap.imgs().getWidth() + 1;
    public boolean dshow = false;
    public LineEdit buf;
    public int sx;
    public boolean pw = false;
    public boolean numeric = false;
    public String text;
    private boolean dirty = false;
    private double focusstart;
    private boolean readonly = false;
    private Text.Line tcache = null;
    private final Consumer<String> onChange;
    private final Consumer<String> onActivate;

    @RName("text")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    if(args[0] instanceof Coord)
		return(new TextEntry(UI.scale((Coord)args[0]), (String)args[1]));
	    else
		return(new TextEntry(UI.scale((Integer)args[0]), (String)args[1]));
	}
    }

    public void settext(String text) {
	buf.setline(text);
	redraw();
    }

    public void rsettext(String text) {
	buf = new LineEdit(this.text = text) {
		protected void done(String line) {
		    activate(line);
		}
		
		protected void changed() {
		    redraw();
		    TextEntry.this.text = line;
		    TextEntry.this.changed();
		}
	    };
	redraw();
    }

    public void setpw(final boolean val) {
	this.pw = val;
	commit();
    }

    public void setReadOnly(final boolean readonly) {
	this.readonly = readonly;
    }

    public void commit() {
	dirty = false;
	redraw();
    }

    public void uimsg(String name, Object... args) {
	switch (name) {
	    case "settext" -> settext((String) args[0]);
	    case "get" -> wdgmsg("text", buf.line);
	    case "pw" -> pw = ((Integer) args[0]) != 0;
	    case "dshow" -> dshow = ((Integer) args[0]) != 0;
	    case "cmt" -> commit();
	    default -> super.uimsg(name, args);
	}
    }

    public int numvalue() {
	if (numeric) {
	    if (text.length() == 0)
		return 0;
	    else if (text.equals("-"))
		return 0;
	    else {
		try {
		    return Integer.parseInt(text);
		} catch (RuntimeException re) {
		    return 0;
		}
	    }
	} else {
	    return 0;
	}
    }

    protected String dtext() {
	if(pw) {
	    return("\u2022".repeat(buf.line.length()));
	} else {
	    return(buf.line);
	}
    }

    public void draw(BufferedImage img) {
	Graphics g = img.getGraphics();
	String dtext = dtext();
	tcache = fnd.render(dtext, (dshow && dirty) ? dirtycol : defcol);

	g.drawImage(lcap.imgs(), 0, 0, null);
	g.drawImage(mext.imgs(), lcap.imgs().getWidth(), 0,
		sz.x - lcap.imgs().getWidth() - rcap.imgs().getWidth(), mext.imgs().getHeight(), null);
	g.drawImage(rcap.imgs(), sz.x - rcap.imgs().getWidth(), 0,  null);

	g.drawImage(tcache.img, toff.x - sx, toff.y, null);

	g.dispose();
    }

    public void draw(GOut g) {
	g.chcolor(GlobalSettings.TXBCOL.get());
	super.draw(g);
	g.chcolor();
	if (hasfocus) {
	    int cx = tcache.advance(buf.point);
	    int lx = cx - sx + 1;
	    if (cx < sx) {
		sx = cx;
		redraw();
	    }
	    if (cx > sx + (sz.x - wmarg)) {
		sx = cx - (sz.x - wmarg);
		redraw();
	    }
	    if (((Utils.rtime() - focusstart) % 1.0) < 0.5)
		g.image(caret, toff.add(coff).add(lx, 0));
	}
    }

    public TextEntry(final int w, final String deftext, final Consumer<String> onChange, final Consumer<String> onActivate) {
	super(new Coord(w, UI.scale(mext.imgs().getHeight())));
	this.onChange = onChange;
	this.onActivate = onActivate;
	rsettext(deftext);
	setcanfocus(true);
    }

    public TextEntry(int w, String deftext) {
        this(w, deftext, null, null);
    }

    @Deprecated
    public TextEntry(Coord sz, String deftext) {
	this(sz.x, deftext, null, null);
    }

    protected void changed() {
	dirty = true;
	if (onChange != null)
	    onChange.accept(text);
    }

    public void activate(String text) {
	if(canactivate)
	    wdgmsg("activate", text);
	if (onActivate != null)
	    onActivate.accept(text);
    }

    public boolean gkeytype(KeyEvent ev) {
	activate(buf.line);
	return(true);
    }

    public boolean keydown(KeyEvent e) {
	return !readonly && (buf.key(e));
    }

    public boolean mousedown(Coord c, int button) {
	parent.setfocus(this);
	if(tcache != null) {
	    buf.point = tcache.charat(c.x + sx);
	}
	return(true);
    }

    public void gotfocus() {
	focusstart = Utils.rtime();
    }

    public void resize(int w) {
	resize(w, sz.y);
	redraw();
    }
}
