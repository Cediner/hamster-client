package hamster.security;

import com.google.common.flogger.FluentLogger;
import hamster.io.Storage;
import haven.MessageBuf;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Stored in dynamic.sqlite
 * Table: account_blob
 * | Name (PK) | data (Encrypted blob) | version (0|1)
 * Version 0 => Original data that only stored username/password
 * Version 1 => Includes username, password, token support, and descriptions
 */
public class AccountManagement {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Storage datastore;
    static {
        final Optional<Storage> cls = Storage.create("jdbc:sqlite:data/accounts.sqlite");
        if(cls.isPresent()) {
            datastore = cls.get();
            Runtime.getRuntime().addShutdownHook(new Thread() {
                public void run() {
                    datastore.close();
                }
            });

            datastore.ensure((sql) -> {
                try (final Statement stmt = sql.createStatement()) {
                    stmt.executeUpdate("CREATE TABLE IF NOT EXISTS account_blob ( name TEXT PRIMARY KEY, data BLOB )");
                    //For older client versions account_blob didn't have the `version` column and it needs added in
                    try(final ResultSet res = stmt.executeQuery("PRAGMA table_info(account_blob)")) {
                        boolean hasVer = false;
                        while(res.next()) {
                            if(res.getString(2).equals("version")) {
                                hasVer = true;
                                break;
                            }
                        }
                        if(!hasVer) {
                            //Older client versions need the new column
                            stmt.executeUpdate("ALTER TABLE account_blob ADD COLUMN version INTEGER NOT NULL DEFAULT 0");
                        }
                    }
                }
            });
        } else {
            datastore = null;
            logger.atSevere().log("Failed to open account datastore");
            System.exit(1);
        }
    }

    public static List<String> getAccounts() {
        final var accounts = new ArrayList<String>();
        datastore.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name FROM account_blob")) {
                    while (res.next()) {
                        accounts.add(res.getString(1));
                    }
                }
            }
        });
        return accounts;
    }

    public static void deleteAccount(final String name) {
        datastore.ensure((sql) -> {
            final PreparedStatement stmt = datastore.prepare("DELETE FROM account_blob WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public static class Account {
        public String username;
        public String password;
        public boolean useToken;
        public String token;
        public String description;

        public Account(final String uname, final String pass,
                       final boolean useToken, final String token,
                       final String description) {
            this.username = uname;
            this.password = pass;
            this.useToken = useToken;
            this.token = token;
            this.description = description;
        }

        public Account(final String uname, final String pass) {
            this(uname, pass, false, "", "");
        }

        public Account() {
            this("", "", false, "", "");
        }
    }

    private final String name;
    private final List<Account> accounts;
    private final byte[] key;

    public AccountManagement(final byte[] key, final String name) throws Exception {
        this.name = name;
        DerivedKeyGen keyg = new DerivedKeyGen();
        this.key = keyg.generateKey(key, "spin2win".getBytes());
        accounts = new ArrayList<>();
        load(name);
    }

    public Account addAccount() {
        final Account acc = new Account();
        accounts.add(acc);
        return acc;
    }

    public void remAccount(final Account acc) {
        accounts.remove(acc);
    }

    public void bumpup(final Account acc) {
        final var idx =  accounts.indexOf(acc);
        if(idx != 0) {
            accounts.remove(acc);
            accounts.add(idx-1, acc);
        }
    }

    public void bumpdown(final Account acc) {
        final var idx =  accounts.indexOf(acc);
        if(idx != accounts.size() - 1) {
            accounts.remove(acc);
            accounts.add(idx+1, acc);
        }
    }

    public Account get(int ind) {
        return accounts.get(ind);
    }

    public int size() {
        return accounts.size();
    }

    public void save() throws Exception {
        final MessageBuf buf = new MessageBuf();
        for (final Account acc : accounts) {
            buf.addstring(acc.username);
            buf.addstring(acc.password);
            buf.adduint8(acc.useToken ? 1 : 0);
            buf.addstring(acc.token);
            buf.addstring(acc.description);
        }

        final byte[] data = Crypto.encrypt(buf.fin(), key);

        datastore.write(sql -> {
            final PreparedStatement stmt = datastore.prepare("INSERT OR REPLACE INTO account_blob VALUES (?, ?, ?)");
            stmt.setString(1, name);
            stmt.setBytes(2, data);
            stmt.setInt(3, 1);
            stmt.executeUpdate();
        });
    }

    /* Version 0
     *   +------------------------+
     *   | Account Name - String  |
     *   | Password - String      |
     *   |------------------------|
     *   | Repeat till eof        |
     *   +------------------------|
     * Version 1
     *   +------------------------+
     *   | Account Name - String  |
     *   | Password - String      |
     *   | useToken - uint8 (0,1) |
     *   | token - String         |
     *   | description - String   |
     *   |------------------------|
     *   | Repeat till eof        |
     *   +------------------------|
     */
    private void load(final String name) throws Exception {
        final PreparedStatement stmt = datastore.prepare("SELECT data, version FROM account_blob WHERE name = ?");
        stmt.setString(1, name);
        try (final ResultSet res = stmt.executeQuery()) {
            if (res.next()) {
                //Existing account with data, otherwise new account with nothing
                final byte[] blob = res.getBytes(1);
                final int ver = res.getInt(2);
                final MessageBuf data = new MessageBuf(Crypto.decrypt(blob, key));
                if(ver == 0) {
                    while (!data.eom()) {
                        accounts.add(new Account(data.string(), data.string()));
                    }
                } else if(ver == 1) {
                    while(!data.eom()) {
                        accounts.add(new Account(data.string(), data.string(),
                                data.uint8() == 1, data.string(), data.string()));
                    }
                }
            }
        }
    }
}
