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
import hamster.ui.core.indir.IndirThemeTex;

public class IBox {
    public interface ImgData {
	Tex ctl();
	Tex ctr();
	Tex cbl();
	Tex cbr();
	Tex bl();
	Tex br();
	Tex bt();
	Tex bb();
    }

    private static class StaticImgData implements ImgData {
	public final Tex ctl, ctr, cbl, cbr;
	public final Tex bl, br, bt, bb;

	private StaticImgData(Tex ctl, Tex ctr, Tex cbl, Tex cbr, Tex bl, Tex br, Tex bt, Tex bb) {
	    this.ctl = ctl;
	    this.ctr = ctr;
	    this.cbl = cbl;
	    this.cbr = cbr;
	    this.bl = bl;
	    this.br = br;
	    this.bt = bt;
	    this.bb = bb;
	}

	public Tex ctl() {
	    return ctl;
	}
	public Tex ctr() {
	    return ctr;
	}
	public Tex cbl() {
	    return cbl;
	}
	public Tex cbr() {
	    return cbr;
	}
	public Tex bl() {
	    return bl;
	}
	public Tex br() {
	    return br;
	}
	public Tex bt() {
	    return bt;
	}
	public Tex bb() {
	    return bb;
	}
    }

    private static class IndirImgData implements ImgData {
	public final IndirThemeTex ctl, ctr, cbl, cbr;
	public final IndirThemeTex bl, br, bt, bb;

	public IndirImgData(IndirThemeTex ctl, IndirThemeTex ctr, IndirThemeTex cbl, IndirThemeTex cbr,
			    IndirThemeTex bl, IndirThemeTex br, IndirThemeTex bt, IndirThemeTex bb) {
	    this.ctl = ctl;
	    this.ctr = ctr;
	    this.cbl = cbl;
	    this.cbr = cbr;
	    this.bl = bl;
	    this.br = br;
	    this.bt = bt;
	    this.bb = bb;
	}

	public Tex ctl() {
	    return ctl.tex();
	}
	public Tex ctr() {
	    return ctr.tex();
	}
	public Tex cbl() {
	    return cbl.tex();
	}
	public Tex cbr() {
	    return cbr.tex();
	}
	public Tex bl() {
	    return bl.tex();
	}
	public Tex br() {
	    return br.tex();
	}
	public Tex bt() {
	    return bt.tex();
	}
	public Tex bb() {
	    return bb.tex();
	}
    }

    public final ImgData imgs;

    /* Themed IBox */
    public IBox(final String res) {
	this.imgs = new IndirImgData(Theme.themetex(res, 0), Theme.themetex(res, 1), Theme.themetex(res, 2),
		Theme.themetex(res, 3), Theme.themetex(res, 4), Theme.themetex(res, 5),
		Theme.themetex(res, 6), Theme.themetex(res, 7));
    }

    /* Non-themed IBox */
    public IBox(Tex ctl, Tex ctr, Tex cbl, Tex cbr, Tex bl, Tex br, Tex bt, Tex bb) {
	this.imgs = new StaticImgData(ctl, ctr, cbl, cbr, bl, br, bt, bb);
    }

    public IBox(String base, String ctl, String ctr, String cbl, String cbr, String bl, String br, String bt, String bb) {
	this(Resource.loadtex(base + "/" + ctl),
	     Resource.loadtex(base + "/" + ctr),
	     Resource.loadtex(base + "/" + cbl),
	     Resource.loadtex(base + "/" + cbr),
	     Resource.loadtex(base + "/" + bl),
	     Resource.loadtex(base + "/" + br),
	     Resource.loadtex(base + "/" + bt),
	     Resource.loadtex(base + "/" + bb));
    }

    public Coord btloff() {
	return(new Coord(imgs.bl().sz().x, imgs.bt().sz().y));
    }

    public Coord ctloff() {
	return(imgs.ctl().sz());
    }

    public Coord bbroff() {
	return(new Coord(imgs.br().sz().x, imgs.bb().sz().y));
    }

    public Coord cbroff() {
	return(imgs.cbr().sz());
    }

    public Coord bisz() {
	return(new Coord(imgs.bl().sz().x + imgs.br().sz().x, imgs.bt().sz().y + imgs.bb().sz().y));
    }

    public Coord cisz() {
	return(imgs.ctl().sz().add(imgs.cbr().sz()));
    }

    @Deprecated
    public Coord tloff() {
	return(btloff());
    }

    @Deprecated
    public Coord bsz() {
	return(cisz());
    }

    public Tex bb() { return imgs.bb(); }

    public void draw(GOut g, Coord tl, Coord sz) {
	final Tex ctl = imgs.ctl(), ctr = imgs.ctr(), cbl = imgs.cbl(), cbr = imgs.cbr();
	final Tex bl = imgs.bl(), br = imgs.br(), bt = imgs.bt(), bb = imgs.bb();
	g.image(bt, tl.add(new Coord(ctl.sz().x, 0)), new Coord(sz.x - ctr.sz().x - ctl.sz().x, bt.sz().y));
	g.image(bb, tl.add(new Coord(cbl.sz().x, sz.y - bb.sz().y)), new Coord(sz.x - cbr.sz().x - cbl.sz().x, bb.sz().y));
	g.image(bl, tl.add(new Coord(0, ctl.sz().y)), new Coord(bl.sz().x, sz.y - cbl.sz().y - ctl.sz().y));
	g.image(br, tl.add(new Coord(sz.x - br.sz().x, ctr.sz().y)), new Coord(br.sz().x, sz.y - cbr.sz().y - ctr.sz().y));
	g.image(ctl, tl);
	g.image(ctr, tl.add(sz.x - ctr.sz().x, 0));
	g.image(cbl, tl.add(0, sz.y - cbl.sz().y));
	g.image(cbr, new Coord(sz.x - cbr.sz().x + tl.x, sz.y - cbr.sz().y + tl.y));
    }
}
