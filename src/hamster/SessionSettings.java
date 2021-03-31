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

    ////Camera
    public final IndirSetting<String> CAMERA;
    public final IndirSetting<Integer> CAMERAPROJFAR;
    public final IndirSetting<Boolean> FREECAMREXAXIS;
    public final IndirSetting<Boolean> FREECAMREYAXIS;
    public final IndirSetting<Boolean> FREECAMLOCKELAV;

    ////UI
    public final IndirSetting<Boolean> SHOWPLAVA;
    public final IndirSetting<Boolean> SHOWSPEED;
    public final IndirSetting<Boolean> SHOWHEALTH;
    public final IndirSetting<Boolean> SHOWENERGY;
    public final IndirSetting<Boolean> SHOWSTAM;
    public final IndirSetting<Boolean> SHOWCAL;
    public final IndirSetting<Boolean> SHOWHOTBAR1;
    public final IndirSetting<Boolean> SHOWHOTBAR2;
    public final IndirSetting<Boolean> SHOWHOTBAR3;
    public final IndirSetting<Boolean> SHOWMINIINV;
    public final IndirSetting<Boolean> SHOWMINIEQU;
    public final IndirSetting<Boolean> SHOWSTUDY;
    public final IndirSetting<Boolean> SHOWINVONLOGIN;
    public final IndirSetting<Boolean> SHOWBELTONLOGIN;
    public final IndirSetting<Boolean> SHOWSESSIONS;
    public final IndirSetting<Boolean> SHOWCHAT;
    public final IndirSetting<Boolean> SHOWMINIMAP;
    public final IndirSetting<Boolean> SHOWLRSLOTS;
    public final IndirSetting<Boolean> SHOWEXPWND;
    //Menugrid
    public final IndirSetting<Integer> MENUGRIDSIZEX;
    public final IndirSetting<Integer> MENUGRIDSIZEY;
    //Meter UI
    public final IndirSetting<Boolean> BIGSIMPLEMETERS;
    //Inv UI
    public final IndirSetting<Boolean> SHOWITEMQ;
    public final IndirSetting<Boolean> SHOWITEMWEAR;
    public final IndirSetting<Boolean> SHOWITEMCONT;
    public final IndirSetting<Boolean> ALWAYSITEMLONGTIPS;
    public final IndirSetting<Boolean> AUTOEQUIP;
    public final IndirSetting<Boolean> WATERDROPITEMCTRL;
    //Flowermenu UI
    public final IndirSetting<Boolean> QUICKFLMENU;
    public final IndirSetting<Boolean> KEEPFLOPEN;
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

        //Camera
        CAMERA = new IndirSetting<>(local, "camera.camera-type", "sortho");
        CAMERAPROJFAR = new IndirSetting<>(local, "camera.camera-proj-far", 5000);
        FREECAMREXAXIS = new IndirSetting<>(local, "camera.free.reverse-x-axis", false);
        FREECAMREYAXIS = new IndirSetting<>(local, "camera.free.reverse-y-axis", false);
        FREECAMLOCKELAV = new IndirSetting<>(local, "camera.free.lock-elevation", false);

        //UI
        SHOWPLAVA = new IndirSetting<>(local, "ui.show-player-avatar", true);
        SHOWSPEED = new IndirSetting<>(local, "ui.show-player-speed", true);
        SHOWHEALTH = new IndirSetting<>(local, "ui.show-player-health", true);
        SHOWENERGY = new IndirSetting<>(local, "ui.show-player-energy", true);
        SHOWSTAM = new IndirSetting<>(local, "ui.show-player-stam", true);
        SHOWCAL = new IndirSetting<>(local, "ui.show-calendar", true);
        SHOWHOTBAR1 = new IndirSetting<>(local, "ui.show-hotbar1", true);
        SHOWHOTBAR2 = new IndirSetting<>(local, "ui.show-hotbar2", true);
        SHOWHOTBAR3 = new IndirSetting<>(local, "ui.show-hotbar3", true);
        SHOWMINIINV = new IndirSetting<>(local, "ui.show-mini-inv", true);
        SHOWMINIEQU = new IndirSetting<>(local, "ui.show-mini-equ", true);
        SHOWSTUDY = new IndirSetting<>(local, "ui.show-study", true);
        SHOWSESSIONS = new IndirSetting<>(local, "ui.show-session-display", true);
        SHOWCHAT = new IndirSetting<>(local, "ui.show-chat", true);
        SHOWLRSLOTS = new IndirSetting<>(local, "ui.show-lr-hand-slots", true);
        SHOWMINIMAP = new IndirSetting<>(local, "ui.show-minimap", true);
        SHOWINVONLOGIN = new IndirSetting<>(local, "ui.show-inv-on-login", true);
        SHOWBELTONLOGIN = new IndirSetting<>(local, "ui.show-belt-on-login", true);
        MENUGRIDSIZEX = new IndirSetting<>(local, "ui.mg.size-x", 4);
        MENUGRIDSIZEY = new IndirSetting<>(local, "ui.mg.size-y", 4);
        BIGSIMPLEMETERS = new IndirSetting<>(local, "ui.big-simple-imeters", false);
        SHOWITEMQ = new IndirSetting<>(local, "ui.inv.show-item-quality", true);
        SHOWITEMWEAR = new IndirSetting<>(local, "ui.inv.show-item-wear", true);
        SHOWITEMCONT = new IndirSetting<>(local, "ui.inv.show-item-cont", true);
        ALWAYSITEMLONGTIPS = new IndirSetting<>(local, "ui.inv.always-show-longtip", true);
        AUTOEQUIP = new IndirSetting<>(local, "ui.inv.auto-equip", true);
        WATERDROPITEMCTRL = new IndirSetting<>(local, "ui.dont-drop-item-over-water", false);
        QUICKFLMENU = new IndirSetting<>(local, "ui.flowermenu.quick-menu", false);
        KEEPFLOPEN = new IndirSetting<>(local, "ui.flowermenu.never-close-on-click", false);
        SHOWEXPWND = new IndirSetting<>(local, "ui.show-experience-window", true);
    }
}
