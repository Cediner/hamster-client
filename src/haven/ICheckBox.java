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

import java.awt.image.BufferedImage;

public class ICheckBox extends ACheckBox {
    public enum Type {
	UP, DOWN, HOVERUP, HOVERDOWN
    }

    protected interface ImgData {
	Tex up();
	Tex down();
	Tex hoverup();
	Tex hoverdown();
    }

    protected static class StaticImgData implements ImgData {
	final Tex up, down, hoverup, hoverdown;

	private StaticImgData(final Tex up, final Tex down,
			      final Tex hoverup, final Tex hoverdown) {
	    this.up = up;
	    this.down = down;
	    this.hoverup = hoverup;
	    this.hoverdown = hoverdown;
	}

	@Override
	public Tex up() {
	    return up;
	}

	@Override
	public Tex down() {
	    return down;
	}

	@Override
	public Tex hoverup() {
	    return hoverup;
	}

	@Override
	public Tex hoverdown() {
	    return hoverdown;
	}
    }

    protected static class IndirImgData implements ImgData {
	final IndirThemeTex up, down, hoverup, hoverdown;

	private IndirImgData(final IndirThemeTex up, final IndirThemeTex down,
			     final IndirThemeTex hoverup, final IndirThemeTex hoverdown) {
	    this.up = up;
	    this.down = down;
	    this.hoverup = hoverup;
	    this.hoverdown = hoverdown;
	}

	@Override
	public Tex up() {
	    return up.tex();
	}

	@Override
	public Tex down() {
	    return down.tex();
	}

	@Override
	public Tex hoverup() {
	    return hoverup.tex();
	}

	@Override
	public Tex hoverdown() {
	    return hoverdown.tex();
	}
    }

    private final ImgData imgs;
    private final BufferedImage img;
    public boolean h;

    @RName("ichk")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    Tex up = Loading.waitfor(ui.sess.getres((Integer)args[0])).layer(Resource.imgc).tex();
	    Tex down = Loading.waitfor(ui.sess.getres((Integer)args[1])).layer(Resource.imgc).tex();
	    Tex hoverup = (args.length > 2) ? Loading.waitfor(ui.sess.getres((Integer)args[1])).layer(Resource.imgc).tex() : up;
	    Tex hoverdown = (args.length > 3) ? Loading.waitfor(ui.sess.getres((Integer)args[1])).layer(Resource.imgc).tex() : down;
	    ICheckBox ret = new ICheckBox(up, down, hoverup, hoverdown);
	    ret.canactivate = true;
	    return(ret);
	}
    }

    /* Themed Constructors */
    public ICheckBox(final String themeres, final String tooltip) {
	super(Coord.z);
	if (tooltip != null)
	    settip(tooltip);
	final IndirThemeRes res = Theme.themeres(themeres);
	imgs = new IndirImgData(res.img(0), res.img(1), res.img(2), res.img(1));
    	if(imgs.up() instanceof TexI)
    	    this.img = ((TexI)imgs.up()).back;
    	else
    	    this.img = null;
	resize(imgs.up().sz());
    }

    /* Default Client Constructors */
    public ICheckBox(Tex up, Tex down, Tex hoverup, Tex hoverdown) {
	super(up.sz());
	this.imgs = new StaticImgData(up, down, hoverup, hoverdown);
	if(up instanceof TexI)
	    this.img = ((TexI)up).back;
	else
	    this.img = null;
    }

    public ICheckBox(Tex up, Tex down, Tex hover) {
	this(up, down, hover, down);
    }

    public ICheckBox(Tex up, Tex down) {
	this(up, down, up);
    }

    public ICheckBox(String base, String up, String down, String hoverup, String hoverdown) {
	this(Resource.loadtex(base + up), Resource.loadtex(base + down), Resource.loadtex(base + hoverup), Resource.loadtex(base + hoverdown));
    }
    public ICheckBox(String base, String up, String down, String hover) {
	this(Resource.loadtex(base + up), Resource.loadtex(base + down), Resource.loadtex(base + hover));
    }
    public ICheckBox(String base, String up, String down) {
	this(Resource.loadtex(base + up), Resource.loadtex(base + down));
    }

    public void draw(GOut g) {
	if(!state())
	    g.image(h ? imgs.hoverup() : imgs.up(), Coord.z);
	else
	    g.image(h ? imgs.hoverdown() : imgs.down(), Coord.z);
        super.draw(g);
    }

    public boolean checkhit(Coord c) {
	if(!c.isect(Coord.z, sz))
	    return(false);
	if((img == null) || img.getRaster().getNumBands() < 4)
	    return(true);
	return(img.getRaster().getSample(c.x, c.y, 3) >= 128);
    }

    public boolean mousedown(Coord c, int button) {
	if((button == 1) && checkhit(c)) {
	    click();
	    return(true);
	}
	return(super.mousedown(c, button));
    }

    public void mousemove(Coord c) {
	this.h = checkhit(c);
    }

    public Object tooltip(Coord c, Widget prev) {
	if(!checkhit(c))
	    return(null);
	return(super.tooltip(c, prev));
    }
}
