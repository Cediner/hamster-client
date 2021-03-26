/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import hamster.security.AccountManagement;
import hamster.ui.login.AccountManager;
import hamster.ui.opt.OptionsWnd;

import java.awt.*;

public class LoginScreen extends Widget {
    public final static Text.Foundry textf = new Text.Foundry(Text.sans, 16).aa(true);
    public final static Text.Foundry textfs = new Text.Foundry(Text.sans, 14).aa(true);
    static final Tex bg = Resource.loadtex("gfx/loginscr");
    private final AccountManagement accdb;
    private final TextEntry username;
    private final TextEntry password;
    private final Label error, progress;


    public LoginScreen(final AccountManagement accdb) {
	super(bg.sz());
	setfocustab(true);
	this.accdb = accdb;

	username = new TextEntry(UI.scale(150), "", null, text -> login());
	password = new TextEntry(UI.scale(150), "", null, text -> login());
	password.pw = true;
	CheckBox showPassword = new CheckBox("Show Password", false, val -> password.setpw(!val));
	showPassword.a = false;
	error = new Label("");
	error.setcolor(Color.red);
	progress = new Label("");
	final Label userlbl = new Label("Username");
	final Label passlbl = new Label("Password");

	final int spacer = UI.scale(5);
	add(new Img(bg), Coord.z);
	adda(new IButton("gfx/hud/buttons/login", "u", "d", "o", this::login) {
	    protected void depress() {
		Audio.play(Button.lbtdown.stream());
	    }

	    protected void unpress() {
		Audio.play(Button.lbtup.stream());
	    }
	}, UI.scale(419), UI.scale(520), 0.5, 0.5);
	add(error, new Coord(UI.scale(420) - username.sz.x / 2, UI.scale(300)));
	add(progress, new Coord(UI.scale(420) - username.sz.x / 2, error.c.y + error.sz.y + spacer));
	add(userlbl, new Coord(UI.scale(420) - username.sz.x / 2, progress.c.y + progress.sz.y + spacer));
	add(username, new Coord(UI.scale(420) - username.sz.x / 2, userlbl.c.y + userlbl.sz.y + spacer));
	add(passlbl, new Coord(UI.scale(420) - username.sz.x / 2, username.c.y + username.sz.y + spacer));
	add(password, new Coord(UI.scale(420) - password.sz.x / 2, passlbl.c.y + passlbl.sz.y + spacer));
	add(showPassword, new Coord(password.c.x, password.c.y + password.sz.y + spacer));
    }

    public Object[] data() {
	return new Object[]{new AuthClient.NativeCred(username.text, password.text), false};
    }

    private void login() {
	super.wdgmsg("forget");
	super.wdgmsg("login", data());
    }

    private void error(String error) {
	this.error.settext(error);
    }

    private void progress(String p) {
	this.progress.settext(p);
    }

    private void clear() {
	progress("");
    }

    public void uimsg(String msg, Object... args) {
	switch (msg) {
	    case "passwd", "token" -> clear();
	    case "error" -> error((String) args[0]);
	    case "prg" -> {
		error("");
		progress((String) args[0]);
	    }
	}
    }

    public void presize() {
	c = parent.sz.div(2).sub(sz.div(2));
    }

    private void normalLogin(final String user, final String pass) {
        super.wdgmsg("forget");
        super.wdgmsg("login", new AuthClient.NativeCred(user, pass), false);
    }

    private void tokenLogin(final String token) {
	super.wdgmsg("forget");
    }

    protected void added() {
	presize();
	parent.setfocus(this);
	setfocus(username);
	ui.root.add(new AccountManager(accdb, this::tokenLogin, this::normalLogin));
	adda(new Button(UI.scale(100), "Options", () -> ui.root.adda(new OptionsWnd(ui), sz.div(2), 0.5, 0.5)),
		UI.scale(10), sz.y - UI.scale(10), 0, 1);
	adda(new Button(UI.scale(100), "Close", () -> ui.root.wdgmsg("close")), sz.x - UI.scale(10), sz.y - UI.scale(10), 1, 1);
    }
}
