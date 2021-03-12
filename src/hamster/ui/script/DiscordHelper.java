package hamster.ui.script;

import hamster.io.Storage;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class DiscordHelper extends Window {
    private static final List<String> bots = new ArrayList<>();
    private static final List<String> roles = new ArrayList<>();
    static {
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_bot ( api_key TEXT )");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_role ( role_id TEXT )");
                try(final ResultSet res = stmt.executeQuery("SELECT api_key FROM discord_bot")) {
                    while (res.next()) {
                        bots.add(res.getString(1));
                    }
                }
                try(final ResultSet res = stmt.executeQuery("SELECT role_id FROM discord_role")) {
                    while (res.next()) {
                        roles.add(res.getString(1));
                    }
                }
            }
        });
    }

    public DiscordHelper() {
        super(Coord.z, "Discord Info" , "discord-window");
        final int spacer = UI.scale(5);
        final int lsth = UI.scale(20);
        final int width = UI.scale(200);
        final LinearGrouping selg = new LinearGrouping("Bot & Roles", spacer);
        {
            final Coord lstrenderc = UI.scale(1,1);
            final Listbox<String> botlst = new Listbox<>(width, 5, lsth) {
                @Override
                protected String listitem(int i) {
                    return bots.get(i);
                }

                @Override
                protected int listitems() {
                    return bots.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, lstrenderc, "%s", item);
                }
            };
            final Listbox<String> rolelst = new Listbox<>(width, 5, lsth) {
                @Override
                protected String listitem(int i) {
                    return roles.get(i);
                }

                @Override
                protected int listitems() {
                    return roles.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, lstrenderc, "%s", item);
                }
            };
            final Button select = new Button(width, "Select", () -> {
                if(botlst.sel != null && rolelst.sel != null) {
                    ui.sess.details.context.dispatchmsg(this, "discord", botlst.sel, rolelst.sel);
                    ui.destroy(this);
                }
            });
            selg.add(new Label("Bot List"));
            selg.add(botlst);
            selg.add(new Label("Role List"));
            selg.add(rolelst);
            selg.add(select);
            selg.pack();
        }

        final LinearGrouping addg = new LinearGrouping("New bot or role", spacer);
        {
            addg.add(new Label("Token"));
            final TextEntry token = addg.add(new TextEntry(width, ""));
            addg.add(new Button(width, "Add Token",  () ->  {
                addToken(token.text);
                token.settext("");
            }));
            addg.add(new Label("Role ID"));
            final TextEntry role = addg.add(new TextEntry(width, ""));
            addg.add(new Button(width, "Add Role", () ->  {
                addRole(role.text);
                role.settext("");
            }));
            addg.pack();
        }
        add(selg);
        add(addg, selg.c.add(0, selg.sz.y + spacer));
        pack();
    }

    private void addToken(final String token) {
        bots.add(token);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO discord_bot VALUES (?)");
            stmt.setString(1, token);
            stmt.executeUpdate();
        });
    }

    private void addRole(final String role) {
        roles.add(role);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO discord_role VALUES (?)");
            stmt.setString(1, role);
            stmt.executeUpdate();
        });
    }
}
