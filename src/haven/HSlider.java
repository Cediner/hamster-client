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


public class HSlider extends Widget {
    private static final IndirThemeRes res = Theme.themeres("slider/horizontal");
    private static final IndirThemeTex left = res.tex(0);
    private static final IndirThemeTex middle = res.tex(1);
    private static final IndirThemeTex right = res.tex(2);
    private static final IndirThemeTex slider = res.tex(3);
    public int val, min, max;
    private UI.Grab drag = null;

    public HSlider(int w, int min, int max, int val) {
	super(new Coord(w, slider.tex().sz().y));
	this.val = val;
	this.min = min;
	this.max = max;
    }

    public void draw(GOut g) {
	g.chcolor(GlobalSettings.SLIDERCOL.get());
	//y offset incase sflarp.sz.y > schain.sz.y
	int cy = (slider.tex().sz().y / 2) - (left.tex().sz().y / 2);
	//Top
	g.image(left.tex(), new Coord(0, cy));
	//middle
	g.rimageh(middle.tex(), new Coord(left.tex().sz().x, cy), sz.x - (left.tex().sz().x + right.tex().sz().x));
	//bottom
	g.image(right.tex(), new Coord(sz.x - right.tex().sz().x, cy));
	//slider
	int fx = ((sz.x - slider.tex().sz().x) * (val - min)) / (max - min);
	g.image(slider.tex(), new Coord(fx, 0));
	g.chcolor();
    }
    
    public boolean mousedown(Coord c, int button) {
	if (button != 1 || !c.isect(Coord.z, sz))
	    return(false);
	drag = ui.grabmouse(this);
	mousemove(c);
	return(true);
    }
    
    public void mousemove(Coord c) {
	if (drag != null) {
	    double a = (double) (c.x - (slider.tex().sz().x / 2)) / (double) (sz.x - slider.tex().sz().x);
	    if (a < 0)
		a = 0;
	    if (a > 1)
		a = 1;
	    val = ((int) Math.round(a * (max - min)) + min);
	    changed();
	}
    }
    
    public boolean mouseup(Coord c, int button) {
	if(button != 1)
	    return(false);
	if(drag == null)
	    return(false);
	drag.remove();
	drag = null;
	return(true);
    }

    public void changed() {}
    
    public void resize(int w) {
	super.resize(new Coord(w, slider.tex().sz().y));
    }
}
