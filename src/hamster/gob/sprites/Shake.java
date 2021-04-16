package hamster.gob.sprites;

import haven.render.sl.Type;
import haven.render.sl.Cons;
import haven.render.Homo3D;
import haven.render.sl.ShaderMacro;
import haven.render.sl.Uniform;
import haven.RUtils;

public class Shake extends RUtils.GeomAdHoc
{
    public static final Uniform dv;
    public final double t;
    public final double a;
    public static final ShaderMacro sh;

    public Shake(final double t, final double a) {
	super(Shake.sh);
	this.t = t;
	this.a = a;
    }

    float[] val() {
	final double n = Math.sin(this.t * 3.141592653589793 * 2.0 * 4.0) * (1.0 - this.t) * 0.019999999552965164;
	return new float[] { (float)(Math.cos(this.a) * n), (float)(Math.sin(this.a) * n) };
    }

    static {
	dv = new Uniform(Type.VEC2, pipe -> ((Shake)pipe.get(RUtils.adhocg)).val(), RUtils.adhocg);
	sh = (programContext -> {
	    final Homo3D homo3D = Homo3D.get(programContext);
	    Homo3D.get(programContext).mapv.mod(expression -> Cons.add(expression,
		    Cons.vec4(Cons.mul(Shake.dv.ref(),
			    Cons.max(Cons.pick(homo3D.objv.depref(), "z"),
				    Cons.l(0.0))), Cons.l(0.0), Cons.l(0.0))), 0);
	});
    }
}