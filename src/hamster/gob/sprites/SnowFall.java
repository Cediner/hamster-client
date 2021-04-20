package hamster.gob.sprites;

import hamster.GlobalSettings;
import haven.*;
import haven.render.*;
import haven.res.lib.bollar.BollData;

import java.awt.*;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

/**
 * Each Grid Cut will have a FlavObj with SnowFall. It'll be spawned directly in the center of a Cut
 * and the snow from it will fall within that Cut only.
 *
 * For the time being this will be a toggle to turn on if wanted
 */
public class SnowFall extends Sprite {
    private static final Random rnd = new Random();
    private static final Pipe.Op basemat = new Light.PhongLight(false,
	    new Color(255, 255, 255),
	    new Color(255, 255, 255),
	    new Color(255, 255, 255),
	    new Color(255, 255, 255),
	    1.0F);

    public class Flake {
	Coord3f position;
	Coord3f velocity;
	Coord3f normal;
	boolean done = false;

	Flake(Coord3f pos) {
	    this.position = new Coord3f(pos.x, pos.y, pos.z);
	    this.velocity = new Coord3f(0.0F, 0.0F, 0.0F);
	    this.normal = new Coord3f(0, 0, 0).norm();
	}

	boolean tick(float dt) {
	    float str = GlobalSettings.SNOWFALLSPEED.get();
	    float guststr = GlobalSettings.SNOWGUSTSPEED.get();
	    this.velocity.z = this.velocity.z + -.98f * dt * str;
	    this.velocity.x += dt * (float) rnd.nextGaussian() * 0.3F * guststr;
	    this.velocity.y += dt * (float) rnd.nextGaussian() * 0.3F * guststr;
	    this.position.x += dt * this.velocity.x;
	    this.position.y += dt * this.velocity.y;
	    this.position.z += dt * this.velocity.z;
	    final Gob g = (Gob)owner;
	    if(g.glob.map.getcz(g.rc.add(position.x, position.y)) >= position.z) {
	        this.done = true;
	    }
	    return !done;
	}
    }

    private static final Coord tsize = MCache.cutsz;
    private final Coord3f offset = MCache.tilesz.mul(-tsize.x / 2d, -tsize.y / 2d).withHeight(0);
    private final Coord3f sz = MCache.tilesz.mul(tsize.x, tsize.y).withHeight(0);
    private List<Flake> flakes = new ArrayList<>();
    private final PointSize size;
    private float de = 0.0F;
    private final WeakReference<MCache.Grid> grid;

    final BollData data = new BollData(new VertexArray.Layout(new VertexArray.Layout.Input(Homo3D.vertex, new VectorFormat(3, NumberFormat.FLOAT32), 0,  0, 24),
	    new VertexArray.Layout.Input(Homo3D.normal, new VectorFormat(3, NumberFormat.FLOAT32), 0, 12, 24)));

    public SnowFall(final Gob owner, final MCache.Grid grid) {
        super(owner, null);
	this.grid = new WeakReference<>(grid);
        size = GlobalSettings.LARGESNOWFLAKE.get() ? new PointSize(4f) : new PointSize(2f);
    }

    @Override
    public boolean tick(double ddt) {
	float dt = (float) ddt;
	final MCache.Grid grid = this.grid.get();
        if(GlobalSettings.SHOWSNOW.get() && grid != null && grid.type() == MCache.GridType.OUTDOOR) {
	    final Gob g = (Gob) owner;
	    float str = GlobalSettings.SNOWDENSITY.get();
	    de += dt * str;
	    if (de > 1.0F) {
		de -= 1.0F;
		final Coord3f pos = offset.add(rnd.nextFloat() * sz.x, rnd.nextFloat() * sz.y, (float) MCache.tilesz.x * 50);
		pos.z += !GlobalSettings.FLATWORLD.get() ? g.glob.map.getcz(g.rc.add(pos.x, pos.y)) : 0;
		flakes.add(new Flake(pos));
	    }
	}
	//Remove dead dust
        flakes = flakes.parallelStream().filter(flake -> flake.tick(dt)).collect(Collectors.toList());
	return false;
    }

    @Override
    public void gtick(Render g) {
	data.update(g, flakes.size(), this::fill);
    }

    private FillBuffer fill(final DataBuffer dst, final Environment env) {
	FillBuffer ret = env.fillbuf(dst);
	ByteBuffer buf = ret.push();
	for(Flake flake : flakes) {
	    buf.putFloat(flake.position.x).putFloat(flake.position.y).putFloat(flake.position.z);
	    buf.putFloat(flake.normal.x).putFloat(flake.normal.y).putFloat(flake.normal.z);
	}
	return(ret);
    }

    @Override
    public void dispose() {
	data.dispose();
    }

    @Override
    public void added(RenderTree.Slot slot) {
	slot.ostate(Pipe.Op.compose(basemat, size));
	slot.add(data);
    }
}
