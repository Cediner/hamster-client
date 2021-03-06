package hamster.gob.sprites;

import haven.*;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;
import java.util.regex.Pattern;

public class Mark extends SkelSprite implements Gob.SetupMod {
    public static final String CHAT_FMT = "$Mark{%d,%d}";
    public static final String CHAT_TILE_FMT = "$MarkTile{%d,%f,%f}";
    public static final Pattern CHAT_FMT_PAT = Pattern.compile("\\$Mark\\{([0-9]+),([0-9]+)}");
    public static final Pattern CHAT_TILE_FMT_PAT = Pattern.compile("\\$MarkTile\\{(-?[0-9]+),([0-9]+\\.[0-9]+),([0-9]+\\.[0-9]+)}");
    private static final Resource tgtfx = Resource.local().loadwait("custom/fx/partytgt");
    public static final int id = -24441;
    private final float[] emi = {1.0f, 0.0f, 1.0f, 0.0f};
    private final float[] clr = Utils.c2fa(new Color(255, 0, 255, 0));
    private int life;
    private boolean haslife;
    private long time;

    public Mark(final int life) {
        super(null, tgtfx, Message.nil);
        this.life = life;
        haslife = life != -1;
    }

    public void setLife(final int life) {
        if (haslife)
            this.life = life;
        if (life == -1)
            haslife = false;
    }

    public void revoke() {
        haslife = true;
        life = 0;
    }

    @Override
    public boolean tick(double dt) {
        super.tick(dt);
        time += dt;
        if (haslife) {
            life -= dt;
            return life <= 0;
        } else {
            return false;
        }
    }

    @Override
    public Pipe.Op gobstate() {
        emi[3] = clr[3] = (float) (1 + Math.sin(2 * Math.PI * time / 3000f));
        return new BaseColor(new Color(clr[0], clr[1], clr[2]));
    }

    @Override
    public String toString() {
        return String.format("Mark[life: %s]", life);
    }
}
