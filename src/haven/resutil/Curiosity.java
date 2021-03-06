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

package haven.resutil;

import haven.*;
import java.awt.image.BufferedImage;
import java.util.concurrent.TimeUnit;

public class Curiosity extends ItemInfo.Tip {
    public final int exp, mw, enc, time;

    public Curiosity(Owner owner, int exp, int mw, int enc, int time) {
	super(owner);
	this.exp = exp;
	this.mw = mw;
	this.enc = enc;
	this.time = time;
    }

    public double lpperhour() {
	return exp / getRealTime() * TimeUnit.HOURS.toSeconds(1);
    }

    public double getRealTime() { return (time / Glob.gametimefac); }

    static String[] units = {"s", "m", "h", "d"};
    static int[] div = {60, 60, 24};
    static String timefmt(int time) {
	int[] vals = new int[units.length];
	vals[0] = time;
	for(int i = 0; i < div.length; i++) {
	    vals[i + 1] = vals[i] / div[i];
	    vals[i] = vals[i] % div[i];
	}
	StringBuilder buf = new StringBuilder();
	for(int i = units.length - 1; i >= 0; i--) {
	    if(vals[i] > 0) {
		if(buf.length() > 0)
		    buf.append(' ');
		buf.append(vals[i]);
		buf.append(units[i]);
	    }
	}
	return(buf.toString());
    }

    public static String timeleft(int time) {
	int[] vals = new int[units.length];
	int start = 0;
	vals[0] = time;
	for(int i = 0; i < div.length; i++) {
	    vals[i + 1] = vals[i] / div[i];
	    vals[i] = vals[i] % div[i];
	    if(vals[i + 1] > 0)
		start = i + 1;
	    else if(vals[i] > 0)
	        start = i;
	}

	StringBuilder buf = new StringBuilder();
	for(int i = start; i >= Math.max(start-1, 1); i--) {
	    if(vals[i] > 0) {
		buf.append(vals[i]);
		buf.append(units[i]);
	    }
	}

	if(buf.isEmpty()) {
	    buf.append(vals[0]);
	    buf.append("s");
	}

	return(buf.toString());
    }

    public BufferedImage tipimg() {
	StringBuilder buf = new StringBuilder();
	if(exp > 0)
	    buf.append(String.format("Learning points: $col[192,192,255]{%s}\n", Utils.thformat(exp)));
	if(time > 0)
	    buf.append(String.format("Study time: $col[192,255,192]{%s}\n", timefmt((int)(getRealTime()))));
	if(mw > 0)
	    buf.append(String.format("Mental weight: $col[255,192,255]{%d}\n", mw));
	if (exp > 0 && time > 0) {
	    final double lph = lpperhour();
	    buf.append(String.format("LP/hour: $col[255,192,255]{%.2f}\n", lph));
	    if (mw > 0) {
		buf.append(String.format("LP/hour/weight: $col[255,192,255]{%.2f}\n", lph / mw));
	    }
	}
	if(enc > 0)
	    buf.append(String.format("Experience cost: $col[255,255,192]{%d}\n", enc));
	return(RichText.render(buf.toString(), 0).img);
    }
}
