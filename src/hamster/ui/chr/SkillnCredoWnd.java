package hamster.ui.chr;

import hamster.ui.core.TabManager;
import haven.Coord;
import haven.UI;
import haven.Widget;
import haven.Window;

public class SkillnCredoWnd extends Window {
    public final SkillTree skills;
    public final CredoTree credos;

    public SkillnCredoWnd() {
        super(Coord.z, "Skills and Credos", "skills-and-credos");
        final TabManager tabs = add(new TabManager(UI.scale(400)));
        credos = new CredoTree();
        skills = new SkillTree();
        tabs.addtab(skills, "Skills");
        tabs.addtab(credos, "Credos");
        pack();
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (!msg.equals("select-tab")) {
            super.wdgmsg(sender, msg, args);
        }
    }
}
