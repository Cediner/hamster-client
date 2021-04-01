package hamster.gob.sprites;

import hamster.GlobalSettings;
import hamster.gob.Tag;
import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;

import java.awt.*;
import java.awt.image.BufferedImage;

public class DamageText extends Sprite implements PView.Render2D  {
    public static final int id = -14115;
    public static final Text.Foundry fnd = new Text.Foundry(Text.sans, 10);
    private static final Color armorcol = new Color(136, 255, 136);
    private static final Color hhpcol = new Color(255, 204, 0);
    private static final Color shpcol = new Color(255, 0, 0);

    private int shp;
    private int hhp;
    private int armor;
    private Tex tex;
    private boolean alive = true;

    public DamageText(Sprite.Owner owner, Resource res) {
	super(owner, res);
	shp = 0;
	hhp = 0;
	armor = 0;
	remake();
    }

    public void draw(GOut g, Pipe state) {
        if(tex != null) {
	    Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 15), state, Area.sized(g.sz())).round2();
	    if (sc.isect(Coord.z, g.sz())) {
		g.aimage(tex, sc, 0.5, 3);
	    }
	}
    }

    private void remake() {
	final BufferedImage img = Utils.hconcat(fnd.render(shp + " ", shpcol).img,
		fnd.render(hhp + " ", hhpcol).img, fnd.render(armor + "", armorcol).img);
	tex = new TexI(Utils.outline2(img, Color.BLACK));
    }

    public void incshp(final int shp) {
	this.shp += shp;
	remake();
    }

    public void inchhp(final int hhp) {
	this.hhp += hhp;
	remake();
    }

    public void incarmor(final int armor) {
	this.armor += armor;
	remake();
    }

    public void rem() {
        alive = false;
    }

    @Override
    public boolean tick(double ddt) {
	super.tick(ddt);
	final Gob g = (Gob) owner;
	if ((g.hasTag(Tag.HUMAN) && !GlobalSettings.SHOWPLAYERDMG.get()) ||
		g.hasTag(Tag.ANIMAL) && !GlobalSettings.SHOWANIMALDMG.get()) {
	    return true;
	} else {
	    return !alive;
	}
    }
}