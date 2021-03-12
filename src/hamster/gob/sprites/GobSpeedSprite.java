package hamster.gob.sprites;

import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;

public class GobSpeedSprite extends Sprite {
    public static final int id = -24447;
    private Tex speed;
    private double lspeed;

    public GobSpeedSprite(final Gob g) {
        super(g, null);
    }

    public void draw(GOut g, Pipe state) {
        if (speed != null) {
            Coord c = Homo3D.obj2view(new Coord3f(0, 0, 16), state).round2();
            g.aimage(speed, c, 0.5, 2.0);
        }
    }

    @Override
    public boolean tick(double dt) {
        final Gob g = (Gob) owner;
        final double spd = g.getv();
        if (spd != lspeed) {
            speed = Text.renderstroked(String.format("%.2f", spd)).tex();
            lspeed = spd;
        }
        return super.tick(dt);
    }
}
