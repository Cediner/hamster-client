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
import java.awt.image.BufferedImage;

public class IButton extends SIWidget {
    public enum Type {
	UP, DOWN, HOVER
    }

    protected interface ImgData {
	BufferedImage up();
	BufferedImage down();
	BufferedImage hover();

	void swap(final Type left, final Type right);
    }

    protected static class StaticImgData implements ImgData {
	BufferedImage up, down, hover;

	private StaticImgData(final BufferedImage up, final BufferedImage down, final BufferedImage hover) {
	    this.up = up;
	    this.down = down;
	    this.hover = hover;
	}

	@Override
	public BufferedImage up() {
	    return up;
	}

	@Override
	public BufferedImage down() {
	    return down;
	}

	@Override
	public BufferedImage hover() {
	    return hover;
	}

	private BufferedImage get(final Type ty) {
	    return switch (ty) {
		case UP -> up;
		case DOWN -> down;
		case HOVER -> hover;
	    };
	}

	private void set(final Type ty, final BufferedImage img) {
	    switch (ty) {
		case UP -> up = img;
		case DOWN -> down = img;
		case HOVER -> hover = img;
	    }
	}

	@Override
	public void swap(Type left, Type right) {
	    final BufferedImage limg = get(left);
	    final BufferedImage rimg = get(right);
	    set(left, rimg);
	    set(right, limg);
	}
    }

    protected static class IndirImgData implements ImgData {
	IndirThemeTex up, down, hover;

	private IndirImgData(final IndirThemeTex up, final IndirThemeTex down, final IndirThemeTex hover) {
	    this.up = up;
	    this.down = down;
	    this.hover = hover;
	}

	@Override
	public BufferedImage up() {
	    return up.imgs();
	}

	@Override
	public BufferedImage down() {
	    return down.imgs();
	}

	@Override
	public BufferedImage hover() {
	    return hover.imgs();
	}

	private IndirThemeTex get(final Type ty) {
	    return switch (ty) {
		case UP -> up;
		case DOWN -> down;
		case HOVER -> hover;
	    };
	}

	private void set(final Type ty, final IndirThemeTex img) {
	    switch (ty) {
		case UP -> up = img;
		case DOWN -> down = img;
		case HOVER -> hover = img;
	    }
	}

	@Override
	public void swap(Type left, Type right) {
	    final IndirThemeTex limg = get(left);
	    final IndirThemeTex rimg = get(right);
	    set(left, rimg);
	    set(right, limg);
	}
    }

    protected final ImgData imgs;
    boolean h = false;
    boolean a = false;
    UI.Grab d = null;

    private Runnable action;
    private final Runnable laction;
    private long pressStart = 0L;

    @RName("ibtn")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new IButton(Resource.loadsimg((String)args[0]), Resource.loadsimg((String)args[1])));
	}
    }

    /* Theme based constructors */
    public IButton(final String themeres, final String tooltip, final Runnable action, final Runnable laction) {
	super(Coord.z);
	if (tooltip != null)
	    settip(tooltip);
	final IndirThemeRes res = Theme.themeres(themeres);
	imgs = new IndirImgData(res.img(0), res.img(1), res.img(2));
	this.action = action;
	this.laction = laction;
	resize(Utils.imgsz(imgs.up()));
    }

    public IButton(final String themeres, final String tooltip, final Runnable action) {
	this(themeres, tooltip, action, action);
    }


    public IButton(final String themeres, final Runnable action) {
	this(themeres, null, action, action);
    }

    public IButton(final String themeres, final String tooltip) {
	super(Coord.z);
	if (tooltip != null)
	    settip(tooltip);
	final IndirThemeRes res = Theme.themeres(themeres);
	imgs = new IndirImgData(res.img(0), res.img(1), res.img(2));
	action = () -> wdgmsg("activate");
	laction = () -> wdgmsg("activate");
	resize(Utils.imgsz(imgs.up()));
    }

    public IButton(BufferedImage up, BufferedImage down, BufferedImage hover, final Runnable action, final Runnable longAction) {
	super(Utils.imgsz(up));
	imgs = new StaticImgData(up, down, hover);
	this.action = action;
	this.laction = longAction;
    }

    /* Default constructors from base client */
    public IButton(BufferedImage up, BufferedImage down, BufferedImage hover, Runnable action) {
	super(Utils.imgsz(up));
	imgs = new StaticImgData(up, down, hover);
	this.action = action;
	this.laction = action;
    }

    public IButton(BufferedImage up, BufferedImage down, BufferedImage hover) {
	super(Utils.imgsz(up));
	imgs = new StaticImgData(up, down, hover);
	this.action = this.laction = () -> wdgmsg("activate");
    }

    public IButton(BufferedImage up, BufferedImage down) {
	this(up, down, up);
    }

    public IButton(String base, String up, String down, String hover, final Runnable action) {
	this(Resource.loadsimg(base + up),
		Resource.loadsimg(base + down),
		Resource.loadsimg(base + (hover == null ? up : hover)),
		action, null);
    }

    public IButton(String base, String up, String down, String hover) {
	this(base, up, down, hover, null);
	this.action = () -> wdgmsg("activate");
    }

    public IButton action(Runnable action) {
	this.action = action;
	return(this);
    }

    public void draw(BufferedImage buf) {
	Graphics g = buf.getGraphics();
	BufferedImage img;
	if(a)
	    img = imgs.down();
	else if(h)
	    img = imgs.hover();
	else
	    img = imgs.up();
	g.drawImage(img, 0, 0, null);
	g.dispose();
    }

    public boolean checkhit(Coord c) {
	if(!c.isect(Coord.z, sz))
	    return(false);
	if(imgs.up().getRaster().getNumBands() < 4)
	    return(true);
	return(imgs.up().getRaster().getSample(c.x, c.y, 3) >= 128);
    }

    public void swap(final Type left, final Type right) {
	imgs.swap(left, right);
    }

    public void click() {
	if(action != null)
	    action.run();
    }

    public void longclick() {
	if(laction != null)
	    laction.run();
    }

    public boolean gkeytype(java.awt.event.KeyEvent ev) {
	click();
	return(true);
    }
    
    protected void depress() {
    }

    protected void unpress() {
    }

    public boolean mousedown(Coord c, int button) {
	if(button != 1)
	    return(false);
	if(!checkhit(c))
	    return(false);
	a = true;
	d = ui.grabmouse(this);
	depress();
	redraw();
	pressStart = System.currentTimeMillis();
	return(true);
    }

    public boolean mouseup(Coord c, int button) {
	if((d != null) && button == 1) {
	    d.remove();
	    d = null;
	    mousemove(c);
	    if(checkhit(c)) {
		unpress();
		if (System.currentTimeMillis() - pressStart < 3000) {
		    click();
		} else {
		    longclick();
		}
	    }
	    return(true);
	}
	return(false);
    }

    public void mousemove(Coord c) {
	boolean h = checkhit(c);
	boolean a = false;
	if(d != null) {
	    a = h;
	    h = true;
	}
	if((h != this.h) || (a != this.a)) {
	    this.h = h;
	    this.a = a;
	    redraw();
	}
    }

    public Object tooltip(Coord c, Widget prev) {
	if(!checkhit(c))
	    return(null);
	return(super.tooltip(c, prev));
    }
}
