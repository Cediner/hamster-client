package hamster.data;

import java.util.Arrays;

public enum DirMap {
    N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW;

    private static final DirMap[] anglemap = new DirMap[360];
    static {
        for(var a = 0; a < 360; ++a) {
            final DirMap angle;
	    if(a >= 12 && a <= 33) {
		angle = NNE;
	    } else if(a >= 34 && a <= 56) {
		angle = NE;
	    } else if(a >= 57 && a <= 78) {
		angle = ENE;
	    } else if(a >= 79 && a <= 101) {
		angle = E;
	    } else if(a >= 102 && a <= 123) {
		angle = ESE;
	    } else if(a >= 124 && a <= 146) {
		angle = SE;
	    } else if(a >= 147 && a <= 168) {
		angle = SSE;
	    } else if(a >= 169 && a <= 191) {
		angle = S;
	    } else if(a >= 192 && a <= 213) {
	        angle = SSW;
	    } else if(a >= 214 && a <= 236) {
	        angle = SW;
	    } else if(a >= 237 && a <= 258) {
	        angle = WSW;
	    } else if(a >= 259 && a <= 281) {
	        angle = W;
	    } else if(a >= 282 && a <= 303) {
	        angle = WNW;
	    } else if(a >= 304 && a <= 326) {
	        angle = NNW;
	    } else if(a >= 327 && a <= 348) {
	        angle = NNW;
	    } else {
	        angle = N;
	    }
	    anglemap[a] = angle;
	}
    }

    public static String anglename(final double angle) {
        var a = (int)Math.toDegrees(angle) + 90;
        a = a < 0 ? 360 + a : a;
        return anglemap[a % 360].name();
    }
}
