package hamster.ui.login;

import com.google.common.flogger.FluentLogger;
import hamster.security.AccountManagement;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class AccountManager extends Window {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    //Account data
    private final AccountManagement data;
    //UI
    private final Listbox<AccountManagement.Account> accounts;
    private final TextEntry username;
    private final TextEntry password;
    private final CheckBox useToken;
    private final TextEntry token;
    //Callbacks
    private final Consumer<String> onTokenLogin;
    private final BiConsumer<String, String> onUPLogin;


    public AccountManager(final AccountManagement data, final Consumer<String> onTokenLogin,
                          final BiConsumer<String, String> onUPLogin) {
        super(Coord.z, "Account Manager", "AccountManager");
        this.data = data;
        this.onUPLogin = onUPLogin;
        this.onTokenLogin = onTokenLogin;
        final var spacer = UI.scale(5, 5);
        final var container = add(new LinearGrouping(spacer, false, LinearGrouping.Direction.HORIZONTAL));
        final var listing = new LinearGrouping("Accounts", spacer, true);
        {
            accounts = listing.add(new Listbox<>(UI.scale(205), 20, UI.scale(20)) {
                final Coord offset = new Coord(UI.scale(5), UI.scale(1));

                @Override
                protected AccountManagement.Account listitem(int i) {
                    return data.get(i);
                }

                @Override
                protected int listitems() {
                    return data.size();
                }

                @Override
                protected void drawitem(GOut g, AccountManagement.Account item, int i) {
                    g.text(item.username, offset);
                }

                @Override
                public void change(AccountManagement.Account item) {
                    if (item != null) {
                        username.settext(item.username);
                        password.settext(item.password);
                        useToken.set(item.useToken);
                        token.settext(item.token);
                    }
                    super.change(item);
                }
            });

            listing.add(new Button(UI.scale(205), "Add Account", this::addAccount));
            listing.add(new Button(UI.scale(205), "Remove Account", this::remAccount));
            final var updown = new LinearGrouping(spacer.x, false, LinearGrouping.Direction.HORIZONTAL);
            updown.add(new Button(UI.scale(100), "Bump Up", this::bumpAccountUp));
            updown.add(new Button(UI.scale(100), "Bump Down", this::bumpAccountDown));
            updown.pack();
            listing.add(updown);
            listing.pack();
            container.add(listing);
        }
        final var details = new LinearGrouping("Account Details", spacer, true);
        {
            details.add(new Label("Username: "));
            username = details.add(new TextEntry(UI.scale(200), "", null, null));
            details.add(new Label("Password: "));
            password = details.add(new TextEntry(UI.scale(200), "", null, null));
            password.setpw(true);
            details.add(new Label("Token: "));
            token = details.add(new TextEntry(UI.scale(200), "", null, null));
            token.setpw(true);
            useToken = details.add(new CheckBox("Use Token For Login"));
            details.add(new CheckBox("Show Password", (val) -> password.setpw(!val), false));
            details.add(new CheckBox("Show Token", (val) -> token.setpw(!val), false));
            details.add(new Button(UI.scale(200), "Save", this::saveAccount));
            details.add(new Button(UI.scale(200), "Login", this::login));
            details.pack();
            container.add(details);
        }
        container.pack();
        pack();
    }

    private void addAccount() {
        final var acc = data.addAccount();
        accounts.sel = acc;
        accounts.showsel();
        username.settext(acc.username);
        setfocus(username);
        password.settext(acc.password);
        useToken.set(acc.useToken);
        token.settext(acc.token);
    }

    private void remAccount() {
        if (accounts.sel != null) {
            data.remAccount(accounts.sel);
            save();
        }
    }

    private void bumpAccountUp() {
        if (accounts.sel != null) {
            data.bumpup(accounts.sel);
            save();
        }
    }

    private void bumpAccountDown() {
        if (accounts.sel != null) {
            data.bumpdown(accounts.sel);
            save();
        }
    }

    private void saveAccount() {
        final var acc = accounts.sel;
        if(acc != null) {
            acc.username = username.text;
            acc.password = password.text;
            acc.useToken = useToken.a;
            acc.token = token.text;
            save();
        }
    }

    private void save() {
        try {
            data.save();
        } catch (Exception e) {
            logger.atSevere().withCause(e).log("Failed to save account data");
        }
    }

    private void login() {
        final var acc = accounts.sel;
        saveAccount();
        if(acc != null) {
            if(acc.useToken)
                onTokenLogin.accept(acc.token);
            else
                onUPLogin.accept(acc.username, acc.password);
        }
    }
}
