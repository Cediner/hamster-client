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

import hamster.GlobalSettings;
import hamster.ui.login.AccountLoginScreen;

import java.net.*;
import java.util.*;

public class Bootstrap implements UI.Receiver, UI.Runner {
    Session sess;
    String hostname;
    int port;
    final Queue<Message> msgs = new LinkedList<>();
    String inituser = null;
    byte[] initcookie = null;
	
    public static class Message {
	int id;
	String name;
	Object[] args;
		
	public Message(int id, String name, Object... args) {
	    this.id = id;
	    this.name = name;
	    this.args = args;
	}
    }
	
    public Bootstrap(String hostname, int port) {
	this.hostname = hostname;
	this.port = port;
    }

    public Bootstrap() {
	this(Config.defserv, Config.mainport);
	if((Config.authuser != null) && (Config.authck != null)) {
	    setinitcookie(Config.authuser, Config.authck);
	    Config.authck = null;
	}
    }
    
    public void setinitcookie(String username, byte[] cookie) {
	inituser = username;
	initcookie = cookie;
    }
	
    private String getpref(String name, String def) {
	return(Utils.getpref(name + "@" + hostname, def));
    }
    
    private void setpref(String name, String val) {
	Utils.setpref(name + "@" + hostname, val);
    }

    public UI.Runner run(UI ui) throws InterruptedException {
	ui.setreceiver(this);
	ui.bind(ui.root.add(new AccountLoginScreen()), 1);
	String loginname = getpref("loginname", "");
	String tokendesc = "";
	boolean savepw = false;
	String authserver = (Config.authserv == null)?hostname:Config.authserv;
	int authport = Config.authport;
	retry: do {
	    byte[] cookie;
	    String acctname;
	    if(initcookie != null) {
		acctname = inituser;
		cookie = initcookie;
		initcookie = null;
	    } else {
		AuthClient.Credentials creds;
		ui.uimsg(1, "passwd", loginname, savepw);
		while(true) {
		    Message msg;
		    synchronized(msgs) {
			while((msg = msgs.poll()) == null)
			    msgs.wait();
		    }
		    if(msg.id == 1) {
			if(msg.name.equals("login")) {
			    creds = (AuthClient.Credentials) msg.args[0];
			    savepw = (Boolean) msg.args[1];
			    if(msg.args.length > 2)
				tokendesc = (String)msg.args[2];
			    loginname = creds.name();
			    break;
			}
		    } else if(msg.name.equals("close")) {
		        return null;
		    }
		}
		ui.uimsg(1, "prg", "Authenticating...");
		try {
		    AuthClient auth = new AuthClient(authserver, authport);
		    try {
			try {
			    acctname = creds.tryauth(auth);
			} catch(AuthClient.Credentials.AuthException e) {
			    ui.uimsg(1, "error", e.getMessage());
			    continue;
			}
			cookie = auth.getcookie();
			if(savepw) {
			    if(GlobalSettings.GENERATINGTOKEN.get()) {
				ui.uimsg(1, "generated-token", acctname,
					Utils.byte2hex(auth.gettoken(new AuthClient.TokenInfo(tokendesc))));
			    }
			}
		    } finally {
			auth.close();
		    }
		} catch(UnknownHostException e) {
		    ui.uimsg(1, "error", "Could not locate server");
		    continue;
		} catch(java.io.IOException e) {
		    ui.uimsg(1, "error", e.getMessage());
		    continue;
		}
	    }
	    ui.uimsg(1, "prg", "Connecting...");
	    try {
		sess = new Session(new InetSocketAddress(InetAddress.getByName(hostname), port), acctname, cookie);
	    } catch(UnknownHostException e) {
		ui.uimsg(1, "error", "Could not locate server");
		continue;
	    }
	    Thread.sleep(100);
	    while(true) {
		if(sess.state.equals("")) {
		    if(GlobalSettings.GENERATINGTOKEN.get()) {
		        sess.close();
		        sess = null;
			GlobalSettings.GENERATINGTOKEN.set(false);
		        continue retry;
		    } else {
			setpref("loginname", loginname);
			ui.destroy(1);
			break retry;
		    }
		} else if(sess.connfailed != 0) {
		    String error = sess.connerror;
		    if(error == null)
			error = "Connection failed";
		    ui.uimsg(1, "error", error);
		    sess = null;
		    continue retry;
		}
		synchronized(sess) {
		    sess.wait();
		}
	    }
	} while(true);
	haven.error.ErrorHandler.setprop("usr", sess.username);
	return(new RemoteUI(sess));
    }

    public void rcvmsg(int widget, String msg, Object... args) {
	synchronized(msgs) {
	    msgs.add(new Message(widget, msg, args));
	    msgs.notifyAll();
	}
    }
}
