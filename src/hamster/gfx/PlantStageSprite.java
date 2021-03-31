package hamster.gfx;

import hamster.GlobalSettings;
import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class PlantStageSprite extends Sprite {
    public static final Text.Foundry cfnd = new Text.Foundry(new Font("Serif", Font.BOLD, 18)).aa(true);
    public static final Text.Foundry fnd = new Text.Foundry(new Font("Serif", Font.BOLD, 12)).aa(true);
    public static final int id = -24445;
    private static final Color stagecolor = new Color(255, 227, 168);
    private static final Tex stgmaxtex = cfnd.renderstroked("\u25CF", new Color(254, 100, 100), Color.BLACK).tex();
    private static final Tex stghrvtex = cfnd.renderstroked("\u25CF", new Color(201, 180, 0), Color.BLACK).tex();
    private static final Tex stgflwtex = cfnd.renderstroked("\u25CF", new Color(120, 120, 120), Color.BLACK).tex();
    private static final Map<Integer, Tex> stgtex = new HashMap<>();

    private static Tex mkStgTex(final int stage) {
        final Tex stg = fnd.renderstroked("" + stage, stagecolor, Color.BLACK).tex();
        stgtex.put(stage, stg);
        return stg;
    }

    private static Tex getStgTex(final int stage) {
        return stgtex.getOrDefault(stage, mkStgTex(stage));
    }

    public int stg, maxstg;
    private Tex tex;
    private final boolean multistg;

    public PlantStageSprite(final Gob g, int stg, int stgmax, boolean multistg) {
        super(g, null);
        this.maxstg = stgmax;
        this.multistg = multistg;
        update(stg, stgmax);
    }

    public void draw(GOut g, Pipe state) {
        final Gob gob = (Gob) owner;
        final UI ui = gob.glob.ui.get();
        if (ui != null && ui.gui != null && GlobalSettings.SHOWCROPSTAGE.get()) {
            Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 16), state).round2();
            g.aimage(tex, sc, 0.5, 2.0);
        }
    }

    public void update(int stg, int stgmax) {
        this.stg = stg;
        if (stgmax == -1) // fallow
            tex = stgflwtex;
        else if (multistg && stg == stgmax - 1)
            tex = stghrvtex;
        else if (stg == stgmax)
            tex = stgmaxtex;
        else
            tex = getStgTex(stg);
    }

    @Override
    public boolean tick(double dt) {
        final Gob g = (Gob) owner;
        final int nstg = !g.fallowplant() ? g.sdt() : -1;
        if (stg != nstg) {
            update(stg, maxstg);
        }
        return super.tick(dt);
    }
}
