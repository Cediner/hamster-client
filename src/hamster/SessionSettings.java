package hamster;

import haven.Coord;
import haven.render.BaseColor;

import java.awt.*;

/**
 * Session based settings based off account:character name
 */
public class SessionSettings {
    ////Gob
    public final IndirSetting<Integer> BADKIN;
    public final IndirSetting<Boolean> COLORFULFARMES;
    public final IndirSetting<Boolean> COLORFULTUBS;
    public final IndirSetting<Boolean> COLORFULCUPBOARDS;
    public final IndirSetting<Boolean> COLORFULCHEESERACKS;
    public final IndirSetting<Boolean> SHOWCROPSTAGE;
    public final IndirSetting<Boolean> SIMPLECROPS;
    public final IndirSetting<BaseColor> GOBHIDDENCOL;
    public final IndirSetting<BaseColor> GOBHITBOXCOL;
    public final IndirSetting<Boolean> SHOWGOBHALO;
    public final IndirSetting<Boolean> SHOWGOBHALOONHEARTH;
    public final IndirSetting<Boolean> SHOWGOBHP;
    public final IndirSetting<Integer> PATHWIDTH;
    public final IndirSetting<Boolean> SHOWGOBPATH;
    public final IndirSetting<Boolean> SHOWANIMALPATH;
    public final IndirSetting<Boolean> SHOWANIMALRADIUS;
    public final IndirSetting<BaseColor> GOBPATHCOL;
    public final IndirSetting<BaseColor> ANIMALPATHCOL;
    public final IndirSetting<BaseColor> VEHPATHCOL;

    //Combat UI
    public final IndirSetting<Boolean> COLORIZEAGGRO;

    public SessionSettings(final String account, final String character) {
        final Settings local = new Settings(String.format("%s:%s", account, character));

        //Gob
        COLORIZEAGGRO = new IndirSetting<>(local, "gob.colorize-aggro", true);
        BADKIN = new IndirSetting<>(local, "gob.bad-kin-color", 2);
        COLORFULFARMES = new IndirSetting<>(local, "gob.colorful-frames", true);
        COLORFULTUBS = new IndirSetting<>(local, "gob.colorful-tubs", true);
        COLORFULCUPBOARDS = new IndirSetting<>(local, "gob.colorful-cupboards", true);
        COLORFULCHEESERACKS = new IndirSetting<>(local, "gob.colorful-cheese-racks", true);
        SHOWCROPSTAGE = new IndirSetting<>(local, "gob.show-crop-stage", false);
        SIMPLECROPS = new IndirSetting<>(local, "gob.simple-crops", false);
        GOBHIDDENCOL = new IndirSetting<>(local, "gob.hidden-col", new BaseColor(Color.WHITE));
        GOBHITBOXCOL = new IndirSetting<>(local, "gob.hitbox-col", new BaseColor(Color.WHITE));
        SHOWGOBHALO = new IndirSetting<>(local, "gob.show-gob-halo", false);
        SHOWGOBHALOONHEARTH = new IndirSetting<>(local, "gob.show-gob-halo-on-hearth", true);
        SHOWGOBHP = new IndirSetting<>(local, "gob.show-gob-hp", true);
        PATHWIDTH = new IndirSetting<>(local, "gob.path-width", 4);
        SHOWGOBPATH = new IndirSetting<>(local, "gob.show-gob-path", false);
        SHOWANIMALPATH = new IndirSetting<>(local, "gob.show-animal-path", false);
        SHOWANIMALRADIUS = new IndirSetting<>(local, "gob.show-animal-radius", false);
        GOBPATHCOL = new IndirSetting<>(local, "gob.gob-path-color", new BaseColor(Color.GREEN));
        ANIMALPATHCOL = new IndirSetting<>(local, "gob.animal-path-color", new BaseColor(Color.RED));
        VEHPATHCOL = new IndirSetting<>(local, "gob.vehicle-path-color", new BaseColor(Color.ORANGE));
    }
}
