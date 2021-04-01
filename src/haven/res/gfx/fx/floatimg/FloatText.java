package haven.res.gfx.fx.floatimg;

import hamster.GlobalSettings;
import hamster.gob.Tag;
import hamster.gob.sprites.DamageText;
import haven.*;

import java.awt.Color;

public class FloatText extends FloatSprite {
    private static final Color
	    shp = new Color(255, 0, 0),
	    hhp = new Color(255, 204, 0),
	    armor = new Color(136, 255, 136);
    public static final Text.Foundry fnd = new Text.Foundry(Text.sans, 10);

    public FloatText(Sprite.Owner owner, Resource res, String text, Color color) {
	super(owner, res,
		new TexI(Utils.outline2(fnd.render(text, color).img, Utils.contrast(color))),
		2);

	if(owner instanceof Gob) {
	    final Gob g = (Gob) owner;
	    if((g.hasTag(Tag.HUMAN) && GlobalSettings.SHOWPLAYERDMG.get()) ||
		    g.hasTag(Tag.ANIMAL) && GlobalSettings.SHOWANIMALDMG.get()) {
		if (color.equals(shp))
		    getDmgOl(g).incshp(Integer.parseInt(text));
		else if (color.equals(hhp))
		    getDmgOl(g).inchhp(Integer.parseInt(text));
		else if (color.equals(armor))
		    getDmgOl(g).incarmor(Integer.parseInt(text));
	    }
	}
    }

    private DamageText getDmgOl(final Gob gob) {
	Gob.Overlay ol = gob.findol(DamageText.id);
	if (ol == null) {
	    //Make a new damagetext
	    ol = gob.daddol(DamageText.id, new DamageText(owner, res));
	}
	return (DamageText) ol.spr;
    }
}
