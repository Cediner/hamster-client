package hamster.gob.sprites;

import haven.Gob;
import haven.Resource;
import haven.Sprite;
import haven.render.Pipe;

public class ShakeOl extends Sprite implements Gob.SetupMod {
    public static final double T = 0.35D;
    public final double a;
    double t = 0.0D;

    public ShakeOl(Sprite.Owner paramOwner, double paramDouble, final Resource res) {
	super(paramOwner, res);
	this.a = paramDouble;
    }

    public Pipe.Op gobstate() {
	return new Shake(Math.min(this.t / 0.35D, 1.0D), this.a);
    }

    public boolean tick(double paramDouble) {
	return ((this.t += paramDouble) > 0.35D);
    }
}
