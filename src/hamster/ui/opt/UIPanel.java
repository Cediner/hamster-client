package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.data.MenuExclusionData;
import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirHSlider;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

public class UIPanel extends Scrollport {
    public UIPanel(final UI ui) {
        super(OptionsWnd.PANEL_SIZE);
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping overall = new LinearGrouping(spacer, false, LinearGrouping.Direction.VERTICAL);
        final Grouping lang = new LinearGrouping(TranslationLookup.get("opt_ui_lang"), spacer, false);
        final Grouping visibility = new GridGrouping(TranslationLookup.get("opt_ui_vis"), spacer, spacer.x, UI.scale(250), false);
        final Grouping minimap = new GridGrouping(TranslationLookup.get("opt_ui_mm"), spacer, spacer.x, UI.scale(200), false);
        final Grouping menu = new LinearGrouping(TranslationLookup.get("opt_ui_mg"), spacer, false);
        final Grouping meter = new LinearGrouping(TranslationLookup.get("opt_ui_meter"), spacer, false);
        final Grouping inv = new LinearGrouping(TranslationLookup.get("opt_ui_inv"), spacer, false);
        final Grouping fmenu = new LinearGrouping(TranslationLookup.get("opt_ui_fmenu"), spacer, false);

        { // Language
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>(TranslationLookup.get("opt_ui_lang_sel"), UI.scale(500), GlobalSettings.LANG);
            {
                for(final var lan : TranslationLookup.langs()) {
                    rgrp.add(lan.display, lan.name);
                }
            }
            lang.add(rgrp);
            lang.pack();
            overall.add(lang);
        }
        { //visibility
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_ava"), GlobalSettings.SHOWPLAVA,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.PlayerAvatar, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_spd"), GlobalSettings.SHOWSPEED,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.PlayerSpeed, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_hp"), GlobalSettings.SHOWHEALTH,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.PlayerHealth, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_energy"), GlobalSettings.SHOWENERGY,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.PlayerEnergy, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_stam"), GlobalSettings.SHOWSTAM,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.PlayerStamina, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_chat"), GlobalSettings.SHOWCHAT,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.ChatWindow, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_cal"), GlobalSettings.SHOWCAL,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.Calendar, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_sess"), GlobalSettings.SHOWSESSIONS,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.SessionDisplay, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_hb1"), GlobalSettings.SHOWHOTBAR1,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.Hotbar1, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_hb2"), GlobalSettings.SHOWHOTBAR2,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.Hotbar2, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_hb3"), GlobalSettings.SHOWHOTBAR3,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.Hotbar3, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_minv"), GlobalSettings.SHOWMINIINV,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.MiniInv, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_mequ"), GlobalSettings.SHOWMINIEQU,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.MiniEqu, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_lrhand"), GlobalSettings.SHOWLRSLOTS,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.LRHandSlots, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_study"), GlobalSettings.SHOWSTUDY,
                    (val) -> GameUI.MessageBus.send(new GameUI.SetVisiblity(GameUI.Wdg.StudyWindow, val))));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_exp"), GlobalSettings.SHOWEXPWND));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_inv_login"), GlobalSettings.SHOWINVONLOGIN));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_belt_login"), GlobalSettings.SHOWBELTONLOGIN));
            visibility.add(new IndirCheckBox(TranslationLookup.get("opt_ui_vis_equip_stats"), GlobalSettings.SHOWEQUIPSTATS));
            visibility.pack();
            overall.add(visibility);
        }
        { //minimap
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_gob"), GlobalSettings.SHOWMMGOBS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_names"), GlobalSettings.SHOWMMMARKERNAMES));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_gob_names"), GlobalSettings.SHOWMMGOBNAMES));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_markers"), GlobalSettings.SHOWMMMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_small_markers"), GlobalSettings.SMALLMMMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_placed"), GlobalSettings.SHOWPMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_natural"), GlobalSettings.SHOWNMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_custom"), GlobalSettings.SHOWCMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_linked"), GlobalSettings.SHOWLMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_kingdom"), GlobalSettings.SHOWKMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_kingdom_radius"), GlobalSettings.SHOWKMARKERRAD));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_village"), GlobalSettings.SHOWVMARKERS));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_village_radius"), GlobalSettings.SHOWVMARKERRAD));
            minimap.add(new IndirCheckBox(TranslationLookup.get("opt_ui_mm_village_name"), GlobalSettings.SHOWVMARKERTIPS));
            minimap.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_ui_mm_queued_path_col"), GlobalSettings.MMPATHCOL));
            minimap.pack();
            overall.add(minimap);
        }
        { //Inventory
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_q"), GlobalSettings.SHOWITEMQ));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_wear"), GlobalSettings.SHOWITEMWEAR));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_cont"), GlobalSettings.SHOWITEMCONT));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_meter"), GlobalSettings.SHOWMETERPER));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_curio_tm"), GlobalSettings.SHOWTIMELEFTCURIO));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_longtip"), GlobalSettings.ALWAYSITEMLONGTIPS));
            inv.add(new IndirCheckBox(TranslationLookup.get("opt_ui_inv_drop_water_mb"), GlobalSettings.WATERDROPITEMCTRL));
            inv.pack();
            overall.add(inv);
        }
        { // Menu Grid
            menu.add(new IndirLabel(() -> String.format(TranslationLookup.get("opt_ui_mg_cols"), GlobalSettings.MENUGRIDSIZEX.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, GlobalSettings.MENUGRIDSIZEX, (val) -> MenuGrid.MessageBus.send(new MenuGrid.UpdateLayout())));
            menu.add(new IndirLabel(() -> String.format(TranslationLookup.get("opt_ui_mg_rows"), GlobalSettings.MENUGRIDSIZEY.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, GlobalSettings.MENUGRIDSIZEY, (val) -> MenuGrid.MessageBus.send(new MenuGrid.UpdateLayout())));
            menu.pack();
            overall.add(menu);
        }
        { //Flowermenu
            fmenu.add(new IndirCheckBox(TranslationLookup.get("opt_ui_fmenu_quick"), GlobalSettings.QUICKFLMENU));
            fmenu.add(new IndirCheckBox(TranslationLookup.get("opt_ui_fmenu_auto_one"), GlobalSettings.AUTOONEOPTFMENU));
            fmenu.add(new IndirCheckBox(TranslationLookup.get("opt_ui_fmenu_keep_open"), GlobalSettings.KEEPFLOPEN));
            {
              final GridGrouping grp = new GridGrouping("Menu Exclusion Options", UI.scale(2), UI.scale(200), true);
              final var lst = grp.add(new Listbox<String>(UI.scale(200), 10, UI.scale(20)) {
                  @Override
                  protected String listitem(int i) {
                      return MenuExclusionData.get(i);
                  }

                  @Override
                  protected int listitems() {
                      return MenuExclusionData.size();
                  }

                  @Override
                  protected void drawitem(GOut g, String item, int i) {
                      FastText.aprint(g, g.sz().div(2), 0.5, 0.5, MenuExclusionData.get(i));
                  }
              });
              final var txt = grp.add(new TextEntry(UI.scale(200), ""));
              grp.add(new Button( UI.scale(200),"Add", () -> {
                  MenuExclusionData.add(txt.text);
                  txt.settext("");
              }));
              grp.add(new Button(UI.scale(200),"Remove", () -> {
                  if(lst.sel != null) {
                      MenuExclusionData.rem(lst.sel);
                  }
              }));
              grp.pack();
              fmenu.add(grp);
            }
            fmenu.pack();
            overall.add(fmenu);
        }
        { // Meter
            meter.add(new IndirCheckBox(TranslationLookup.get("opt_ui_meter_alt_display"), GlobalSettings.BIGSIMPLEMETERS));
            meter.pack();
            overall.add(meter);
        }

        overall.pack();
        add(overall);
        pack();
    }
}
