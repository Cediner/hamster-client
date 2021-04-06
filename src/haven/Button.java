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

import hamster.ui.core.Theme;
import hamster.ui.core.indir.IndirThemeRes;
import hamster.ui.core.indir.IndirThemeTex;

import java.awt.Graphics;
import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;

public class Button extends SIWidget {
    private static final IndirThemeRes res = Theme.themeres("buttons/textbtn");
    public static final IndirThemeTex ul = res.img(0);
    public static final IndirThemeTex um = res.img(1);
    public static final IndirThemeTex ur = res.img(2);
    public static final IndirThemeTex dl = res.img(3);
    public static final IndirThemeTex dm = res.img(4);
    public static final IndirThemeTex dr = res.img(5);
    public static final int hs = ul.imgs().getHeight(), hl = um.imgs().getHeight();
    public static final Resource click = Loading.waitfor(Resource.local().load("sfx/hud/btn"));
    public static final Resource.Audio lbtdown = Loading.waitfor(Resource.local().load("sfx/hud/lbtn")).layer(Resource.audio, "down");
    public static final Resource.Audio lbtup   = Loading.waitfor(Resource.local().load("sfx/hud/lbtn")).layer(Resource.audio, "up");
    public static final int margin = UI.scale(10);
    public boolean lg;
    public Text text;
    public BufferedImage cont;
    public Runnable action = null;
    static Text.Foundry tf = new Text.Foundry(Text.serif.deriveFont(Font.BOLD, UI.scale(12f))).aa(true);
    static Text.Furnace nf = new PUtils.BlurFurn(new PUtils.TexFurn(tf, Window.ctex), 1, 1, new Color(80, 40, 0));
    protected boolean a = false;
    private UI.Grab d = null;
	
    @RName("btn")
    public static class $Btn implements Factory {
	public Widget create(UI ui, Object[] args) {
	    if(args.length > 2)
		return(new Button(UI.scale((Integer)args[0]), (String)args[1], ((Integer)args[2]) != 0));
	    else
		return(new Button(UI.scale((Integer)args[0]), (String)args[1]));
	}
    }
    @RName("ltbtn")
    public static class $LTBtn implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(wrapped(UI.scale((Integer)args[0]), (String)args[1]));
	}
    }
	
    public static Button wrapped(int w, String text) {
	Button ret = new Button(w, tf.renderwrap(text, w - margin));
	return(ret);
    }

    private static boolean largep(int w) {
	return (w >= (ul.imgs().getWidth() + um.imgs().getWidth() + ur.imgs().getWidth()));
    }

    private Button(int w, boolean lg) {
	super(new Coord(w, lg?hl:hs));
	this.lg = lg;
    }

    public Button(int w, String text, boolean lg, Runnable action) {
	this(w, lg);
	this.text = nf.render(text);
	this.cont = this.text.img;
	this.action = action;
    }

    public Button(int w, String text, boolean lg) {
	this(w, text, lg, null);
	this.action = () -> wdgmsg("activate");
    }

    public Button(int w, String text, Runnable action) {
	this(w, text, largep(w), action);
    }

    public Button(int w, String text) {
	this(w, text, largep(w));
    }

    public Button(int w, Text text) {
	this(w, largep(w));
	this.text = text;
	this.cont = text.img;
    }
	
    public Button(int w, BufferedImage cont) {
	this(w, largep(w));
	this.cont = cont;
    }


    public Button(final String text, Runnable action) {
        this(0, false);
	change(text);
	sz = new Coord(cont.getWidth() + ul.imgs().getWidth() + um.imgs().getWidth() + ur.imgs().getWidth(), ul.imgs().getHeight());
	this.action = action;
    }

    public Button(final String text) {
	this(0, false);
	change(text);
	sz = new Coord(cont.getWidth() + ul.imgs().getWidth() + um.imgs().getWidth() + ur.imgs().getWidth(), ul.imgs().getHeight());
	this.action = () -> wdgmsg("activate");
    }
	
    public Button action(Runnable action) {
	this.action = action;
	return(this);
    }

    public void draw(BufferedImage img) {
	Graphics g = img.getGraphics();

	if (a) {
	    //down
	    g.drawImage(dl.imgs(), 0, 0, dl.imgs().getWidth(), sz.y, null);
	    g.drawImage(dm.imgs(), dl.imgs().getWidth(), 0, sz.x - dr.imgs().getWidth() - dl.imgs().getWidth(), sz.y, null);
	    g.drawImage(dr.imgs(), sz.x - dr.imgs().getWidth(), 0, dr.imgs().getWidth(), sz.y, null);
	} else {
	    //up
	    g.drawImage(ul.imgs(), 0, 0, ul.imgs().getWidth(), sz.y, null);
	    g.drawImage(um.imgs(), ul.imgs().getWidth(), 0, sz.x - ur.imgs().getWidth() - ul.imgs().getWidth(), sz.y, null);
	    g.drawImage(ur.imgs(), sz.x - ur.imgs().getWidth(), 0, ur.imgs().getWidth(), sz.y, null);
	}

	Coord tc = sz.sub(Utils.imgsz(cont)).div(2);
	if(a)
	    tc = tc.add(UI.scale(1), UI.scale(1));
	g.drawImage(cont, tc.x, tc.y, null);

	g.dispose();
    }
	
    public void change(String text, Color col) {
	this.text = tf.render(text, col);
	this.cont = this.text.img;
	redraw();
    }
    
    public void change(String text) {
	this.text = nf.render(text);
	this.cont = this.text.img;
	redraw();
    }

    public void click() {
	if(action != null)
	    action.run();
    }

    public boolean gkeytype(java.awt.event.KeyEvent ev) {
	click();
	return(true);
    }
    
    public void uimsg(String msg, Object... args) {
	if(msg == "ch") {
	    if(args.length > 1)
		change((String)args[0], (Color)args[1]);
	    else
		change((String)args[0]);
	} else {
	    super.uimsg(msg, args);
	}
    }
    
    public void mousemove(Coord c) {
	if(d != null) {
	    boolean a = c.isect(Coord.z, sz);
	    if(a != this.a) {
		this.a = a;
		redraw();
	    }
	}
    }

    protected void depress() {
	Audio.play(click);
    }

    protected void unpress() {
	Audio.play(click);
    }

    public boolean mousedown(Coord c, int button) {
	if(button != 1)
	    return(false);
	a = true;
	d = ui.grabmouse(this);
	depress();
	redraw();
	return(true);
    }
	
    public boolean mouseup(Coord c, int button) {
	if((d != null) && button == 1) {
	    d.remove();
	    d = null;
	    a = false;
	    redraw();
	    if(c.isect(new Coord(0, 0), sz)) {
		unpress();
		click();
	    }
	    return(true);
	} else if(d != null) {
	    //XXX: Seems like if you click down, then click another button and let up it'll
	    // bug this out and only send mouseup for the last button up and not each
	    d.remove();
	    d = null;
	    a = false;
	    redraw();
	    unpress();
	}
	return(false);
    }
}
