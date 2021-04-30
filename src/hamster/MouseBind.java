package hamster;

import hamster.data.TranslationLookup;
import haven.UI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MouseBind {
    private static final Settings datastore = new Settings("mousebinds", new HashMap<>());
    public static final Map<String, List<MouseBind>> bindgrps = new HashMap<>();
    //MapView related
    public static final MouseBind //TODO MV_PATHFIND_GOB_IACT
        MV_LOCK_PLACING_OBJ, MV_SHOW_SPEC_MENU, MV_QUEUE_MOVE, MV_PATHFIND_MOVE;
    //Item related
    public static final MouseBind
        ITM_TRANSFER, ITM_TRANSFER_ALL_ALIKE, ITM_DROP, ITM_DROP_ALL_ALIKE,
        ITM_TAKE, ITM_TOGGLE_LOCK, ITM_AUTO_EQUIP, ITM_AUTO_EQUIP_LH,
        ITM_AUTO_EQUIP_RH;
    //Held Item Related
    public static final MouseBind
        HITM_TOGGLE_LOCK, HITM_DROP, HITM_IACT_OBJ, HITM_TRANS_OBJ_SHIFT, HITM_TRANS_ALL_OBJ, HITM_IACT,
        HITM_DROP_WATER;
    //Minimap related
    public static final MouseBind
        MM_GOTO_MARKER, MM_EXAMINE_MARKER, MM_SPECIAL_MENU, MM_FOLLOW_LINK;

    private static MouseBind addMB(final String name, final String group, final String bind) {
        final String display = TranslationLookup.get(name);
        final MouseBind mb = new MouseBind(display, group,
                new IndirSetting<>(datastore, name.replaceAll("\\s","").toLowerCase(), bind));
        if (bindgrps.containsKey(mb.grouping))
            bindgrps.get(mb.grouping).add(mb);
        else {
            final List<MouseBind> set = new ArrayList<>();
            set.add(mb);
            bindgrps.put(mb.grouping, set);
        }
        return mb;
    }

    static {
        //Map related
        final String MV_GRP = TranslationLookup.get("mb_mv");
        MV_LOCK_PLACING_OBJ = addMB(("mb_mv_lock_placing_obj"), MV_GRP, "C-B3");
        MV_SHOW_SPEC_MENU = addMB(("mb_mv_show_spec_menu"), MV_GRP, "M-B3");
        MV_QUEUE_MOVE = addMB(("mb_mv_queue_move"), MV_GRP, "M-B1");
        MV_PATHFIND_MOVE = addMB(("mb_mv_pathfind_move"), MV_GRP, "S-C-M-B1");
        //Item related
        final String ITM_GRP = TranslationLookup.get("mb_itm");
        ITM_TRANSFER = addMB(("mb_itm_transfer"), ITM_GRP, "S-B1");
        ITM_TRANSFER_ALL_ALIKE = addMB(("mb_itm_transfer_all_alike"), ITM_GRP, "M-B1");
        ITM_DROP = addMB(("mb_itm_drop"), ITM_GRP, "C-B1");
        ITM_DROP_ALL_ALIKE = addMB(("mb_itm_drop_all_alike"), ITM_GRP, "M-B1");
        ITM_TAKE = addMB(("mb_itm_take"), ITM_GRP, "B1");
        ITM_TOGGLE_LOCK = addMB(("mb_itm_toggle_lock"), ITM_GRP, "C-B3");
        ITM_AUTO_EQUIP = addMB(("mb_auto_equip"), ITM_GRP, "M-B3");
        ITM_AUTO_EQUIP_LH = addMB(("mb_auto_equip_lh"), ITM_GRP, "S-B2");
        ITM_AUTO_EQUIP_RH = addMB(("mb_auto_equip_rh"), ITM_GRP, "C-B2");
        //Held Item related
        final String HITM_GRP = TranslationLookup.get("mb_hitm");
        HITM_TOGGLE_LOCK = addMB(("mb_hitm_toggle_lock"), HITM_GRP, "C-B3");
        HITM_DROP = addMB(("mb_hitm_drop"), HITM_GRP, "B1");
        HITM_DROP_WATER = addMB(("mb_hitm_drop_water"), HITM_GRP, "C-B1");
        HITM_IACT_OBJ = addMB(("mb_hitm_iact_obj"), HITM_GRP, "B3");
        HITM_TRANS_OBJ_SHIFT = addMB(("mb_hitm_trans_obj_shift"), HITM_GRP, "S-B3");
        HITM_TRANS_ALL_OBJ = addMB(("mb_hitm_trans_all_obj"), HITM_GRP, "S-C-B3");
        //XXX: This one may no longer be possible due to server-side updates
        HITM_IACT = addMB(("mb_hitm_iact"), HITM_GRP, "M-B3");
        //Minimap Related
        final String MM_GRP = TranslationLookup.get("mb_mm");
        MM_EXAMINE_MARKER = addMB("mb_mm_examine_marker", MM_GRP, "B1");
        MM_GOTO_MARKER = addMB("mb_mm_goto_marker", MM_GRP, "S-B1");
        MM_FOLLOW_LINK = addMB("mb_mm_follow_link", MM_GRP, "B3");
        MM_SPECIAL_MENU = addMB("mb_mm_special_menu", MM_GRP, "M-B3");
    }

    @FunctionalInterface
    public interface Command {
        boolean run();
    }

    public final String name;
    public final String grouping;
    public final IndirSetting<String> bind;

    public MouseBind(final String name, final String grouping, final IndirSetting<String> bind) {
        this.name = name;
        this.grouping = grouping;
        this.bind = bind;
    }

    public boolean check(final String ibind, final Command action) {
        return ibind.equals(bind.get()) && action.run();
    }

    public boolean match(final String ibind) {
        return ibind.equals(bind.get());
    }

    public static boolean validBinding(final IndirSetting<String> ignore, final String group, final String binding) {
        if (binding.equals("")) {
            return true;
        } else {
            for (final MouseBind mb : bindgrps.get(group)) {
                if (mb.bind != ignore && mb.bind.get().equals(binding)) {
                    return false;
                }
            }
        }
        return true;
    }

    public static String generateSequence(final UI ui, final int mbutton) {
        final StringBuilder seq = new StringBuilder();
        if (ui.modshift)
            seq.append("S-");
        if (ui.modctrl)
            seq.append("C-");
        if (ui.modmeta)
            seq.append("M-");
        seq.append("B");
        seq.append(mbutton);
        return seq.toString();
    }
}
